# Reviewer Briefing: SFR IDM Integration

This branch ports MODFLOW 6's SFR (Streamflow Routing) package to the
Input Data Model (IDM) framework: a generic, `.dfn`-driven input loader
that replaces package-specific `BlockParser` reading code. This document
gives independent context for reviewing it — it was prepared by an AI
assistant that worked on this branch, for a reviewer who will not have
access to that working session.

## This is one of five related branches

SFR is one of five advanced-package IDM integrations sharing the same
framework code:

- `idm_lak` (LAK)
- `idm_maw` (MAW)
- `idm_sfr` (SFR) — **this branch**
- `idm_uzf` (UZF)
- `idm_transport_adv` (the 8 GWT/GWE advanced transport packages: LKT,
  MWT, SFT, UZT and their GWE energy-transport counterparts LKE, MWE,
  SFE, UZE)

The shared framework files (listed below) are kept deliberately
near-identical across all five branches — a fix made while working on
one branch is expected to be ported to the others. This branch's copy
of the framework was most recently synced from `idm_transport_adv`,
which had the most current fixes at sync time.

**If you find something that looks like a framework design question
rather than an SFR-specific bug, it is very likely relevant to all five
branches, not just this one.**

## Shared framework files touched in this branch's diff

- `src/Utilities/Idm/LoadContext.f90`
- `src/Utilities/Idm/mf6blockfile/Mf6FileKeystring.f90`
- `src/Utilities/Idm/mf6blockfile/Mf6FileList.f90`
- `src/Utilities/Idm/mf6blockfile/StructArray.f90`
- `src/Utilities/Idm/mf6blockfile/StructVector.f90`
- `src/Utilities/Idm/mf6blockfile/LoadMf6File.f90`
- `src/Utilities/Constants.f90`
- `src/Utilities/TimeSeries/TimeSeriesManager.f90`
- `src/Model/ModelUtilities/BoundaryPackageExt.f90`
- `utils/idmloader/scripts/dfn2f90.py`

SFR-specific files: `doc/mf6io/mf6ivar/dfn/gwf-sfr.dfn`,
`src/Model/GroundWaterFlow/gwf-sfr.f90` and its submodules, plus the
`SPC`/`TVK`/`TVS` utility-package dfns and generated code (these were
also updated on this branch to keep pace with the framework; they are
not SFR-specific but ride along in this diff).

## Two loader families, and an expected "dead code" appearance

The framework has two distinct PERIOD-block loaders:

1. **Keystring loader** (`Mf6FileKeystring.f90`) — used by LAK, MAW,
   SFR, and all 8 `idm_transport_adv` packages. Each PERIOD row carries
   a dispatch keyword (e.g. `STAGE`, `RAINFALL`) that routes the row to
   a typed, permanent, feature-indexed array with cross-period
   persistence (an unrepeated setting keeps its prior value).
2. **List loader** (`Mf6FileList.f90`) — used by UZF, whose PERIOD block
   is a plain fixed-column recarray rather than a keystring. UZF needed
   the same cross-period persistence property, so `Mf6FileList.f90`
   gained its own parallel mechanism (`apply_persistent_settings`) for
   that.

**SFR uses only the keystring loader.** `Mf6FileList.f90`'s
`apply_persistent_settings` code is present in this branch's copy of the
file (kept in sync per the shared-framework convention above) but is
never exercised by anything in this branch — no SFR test reaches it,
because SFR has no plain-recarray PERIOD block. **This is expected, not
a gap in this branch's test coverage.** If you want to evaluate that
code path, the place to do it is `idm_uzf`'s own pushed PR, where it is
actually exercised. One honest caveat: `idm_uzf`'s framework copy is
somewhat behind this branch's, though the list-loader mechanism itself
is believed to be largely in sync between the two — worth a direct
diff if the exact state matters to your review.

## Key generic framework mechanisms (keystring loader)

- **Leading-column classification** (`is_id_colname` in
  `LoadContext.f90`): recognizes a PERIOD keystring's leading id column
  by tagname (`IFNO`, `BNDNO`, and per-package aliases like SFR's own
  `RNO` on other branches) to decide whether the block is
  identifier-addressed. For SFR, the leading tag is already `IFNO`, so
  this classification has always worked correctly here; the recent
  consolidation (see below) is behaviorally a no-op for this branch.
- **`PACKAGEDATA_IFNO` bridge**: an advanced package's `PACKAGEDATA` row
  count is published under this synthetic name, which the PERIOD loader
  reads to size its permanent per-feature arrays (`MAXBOUND`).
- **Generic settings dispatch** (`allocate_settings`/`apply_settings`):
  every `DOUBLE`+`time_series true` PERIOD member gets a permanent,
  cross-period-persistent array, keyed by its own tag.
- **`AUXILIARY`/`AUXVAL` handling** (`apply_auxiliary`): a
  record-compound PERIOD member (`type record auxiliary auxname
  auxval`) resolved dynamically against a sibling `AUXNAME`, rather than
  by its own position. `AUXVAL` was recently changed from `STRING` to
  `DOUBLE`+`time_series true` on this branch (see below) to match
  LAK/MAW's already-established convention.
- **`record_dependency`** (`LoadContext.f90`): a hardcoded, per-package
  lookup table for a record-follower PERIOD field with no `SHAPE` of its
  own — currently has exactly one entry, for **SFR's own `DIVFLOW`**
  (a `DIVERSIONRECORD` sub-member, index-addressed via a sibling `IDV`
  field, dimensioned by `NDV`). This was built specifically to support
  SFR; no other package currently needs a second entry. A fully-generic,
  structural alternative (classifying record followers by type instead
  of a hardcoded table) was designed and estimated but deliberately not
  built — the hardcoded table fails loudly for anything unverified,
  which was judged safer than an unproven generic classifier inferred
  from a single example. This is a known, tracked, open design question,
  not a defect.

## `BoundaryPackageExt.f90`'s role

Advanced packages (MAW/SFR/LAK/UZF) extend `BndExtType`, which bridges
the generic IDM-loaded input context to the package's own `BndType`
connections-level arrays (`nodelist`, `bound`, `auxvar`). Two things to
know:

- `bndext_allocate_arrays` allocates the connections-level `BndType`
  arrays for these multi-row-per-feature packages (a feature like a
  MAW well or an SFR reach can have more than one GWF connection).
- `allocate_featureauxvar` points a package's per-feature AUX array
  directly at the input context's permanent, feature-indexed AUX array
  (a live alias, not a copy), permuting `PACKAGEDATA` row order into
  `IFNO` order.

## SFR-specific bugs found and fixed on this branch

Independent of the framework work above, the SFR package port itself
had five bugs, all found and fixed during this branch's own review:

1. `NREACHES` was missing `release=.false.`, breaking every PERIOD
   keystring setting once the input context released it.
2. The `mf6dimension` mechanism (lets a PERIOD setting size itself from
   a named dimension other than the package's default feature count)
   was never rolled out to SFR's 5 `NREACHES`-dimensioned settings
   (`STAGE`/`INFLOW`/`RAINFALL`/`EVAPORATION`/`RUNOFF`).
3. `CPRIOR` enum validation (`UPTO`/`THRESHOLD`/`FRACTION`/`EXCESS`) was
   silently dropped during the port — an invalid value used to error,
   now silently fell through to a default.
4. The `DEV_NO_FINAL_CHECK` dev-only option was missing from the `.dfn`
   entirely.
5. `DEV_NO_CHECK` itself was silently non-functional — it checked
   presence under the wrong memory-manager key (the tag name instead of
   the `mf6internal`-mapped name), so its intended suppression of a
   validation check never actually worked for any input.

All five are fixed and covered by regression tests. A separate,
independent diff-scoped review of the SFR package code found no further
issues beyond these five.

## Recently found and fixed: cross-record PERIOD dedup

SFR's PERIOD block has two independent compound records in the same
keystring (`AUXILIARYRECORD`, `DIVERSIONRECORD`). A bug was found and
fixed where reissuing one record in a later period could disturb the
other's own independent tracking (e.g. a TS link on one field being
dropped by a PERIOD block that only reissues the other field). See
`autotest/test_gwf_sfr_period_dedup.py` for the regression coverage —
this is a subtle interaction worth extra scrutiny given its recency.

## Test status as of this snapshot

Full local suite for SFR + the SPC/TVK/TVS utility packages (which ride
along on the same framework): 84/84 passing. All diffed test files were
also checked against the actual 6.7.0 release binary to distinguish
real regressions from output-format evolution already present on
`develop`.

## What this review is, and isn't

This is a framework-and-package review requested before this branch's
first commit — nothing has been committed here yet. The person who
prepared this branch wants confidence that the shared framework has no
major gaps for SFR-style (keystring-based) advanced packages before
porting these same framework updates back to the other four branches.
Findings that turn out to be relevant to more than just SFR are
especially valuable to flag as such.
