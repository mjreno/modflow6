# SFR IDM Integration — Review Outcome

Deep review of the SFR-to-IDM integration on branch `idm_sfr_review`,
reviewed as a diff against this branch's `develop` branch point.

- **Base (merge-base with develop):** `f70dd207fa3ea209bebae6ee82481d80c82f04b7`
- **HEAD reviewed:** `9e95411342cdf43a8ba1ba81f29db8dca1f1b341`
- **Branch:** `idm_sfr_review`
- **Scope:** all changes in the diff `f70dd207..9e95411` (36 files, ~6787 insertions / ~2416 deletions).
- **Method:** static code review only. No build, no test execution.
- Findings verified by reading source; where a claim could not be
  confirmed statically (e.g. runtime numeric equivalence), it is marked
  **VERIFY** with the reason.

> This document is written to be actionable by a separate update session
> that does not have access to the review session. Each finding lists a
> file:location, what was observed, why it matters, and a suggested
> action. "Cross-branch" flags items believed to affect the other four
> advanced-package IDM branches (`idm_lak`, `idm_maw`, `idm_uzf`,
> `idm_transport_adv`) because they touch the shared framework.

---

## Severity taxonomy

- **Blocker** — must be resolved before merge; likely wrong or unsafe.
- **Major** — real behavioral risk or capability change; needs a
  decision or verification before merge.
- **Minor** — low-risk nuance, cleanup, or hardening opportunity.
- **Question / VERIFY** — needs confirmation against legacy behavior,
  another branch, or a test run; not necessarily a defect.

**Overall assessment:** The integration is coherent and well-structured.
All five SFR package bugs and the cross-record dedup fix described in the
briefing were confirmed present and correct in the code. No **Blocker**
was found by static review. The items below are the ones a downstream
session should resolve or explicitly sign off on. The highest-value ones
are M4 (a lost input validation — diversion target reach range check),
M1 (generator change latent for all packages), M2 (advanced-package
`_da` pointer asymmetry), and Q1 (transport test tolerance loosening). A
dedicated **Behavior preservation vs develop** section below itemizes
the printing/error-detection comparison against the develop branch
point; M4 and m6 are the only substantive behavior regressions it found.

---

## Major

### M1 — `dfn2f90.py` PERIOD-block varname change is latent for every package (cross-branch)
- **Where:** `utils/idmloader/scripts/dfn2f90.py`, `Param.varname` and
  `Param.found_name` (the `and self.block.upper() != "PERIOD"` guards).
- **Observed:** PERIOD-block parameters are no longer qualified with
  `<component><subcomponent>_period_`; they now generate the *bare*
  `mf6varname` (e.g. `STAGE`, not `gwfsfr_period_stage`). This is
  required by the keystring loader, which keys permanent per-feature
  arrays by the bare tag. Confirmed in the generated
  `src/Idm/gwf-sfridm.f90` (`gwfsfr_stage`, `gwfsfr_bedk`,
  `gwfsfr_divflow`, etc.).
- **Why it matters:** This generator change affects **every package that
  has a PERIOD block**, not just SFR. In this diff only `gwf-sfr.dfn` was
  added to `utils/idmloader/dfns.txt` and only `gwf-sfridm.f90` was
  regenerated, so the change is currently **latent**. If anyone runs a
  full regeneration of all dfns, the generated PERIOD varnames for other
  packages will change, potentially breaking packages that consume
  PERIOD variables by their old `_period_`-qualified name.
- **Suggested action:** Before merge, either (a) confirm no
  currently-generated package reads a PERIOD variable by its old
  qualified name and document that a full regen is safe, or (b) scope the
  guard so it only applies to keystring/advanced packages. Coordinate
  with the other four branches since they share this generator.

### M2 — `bndext_da` advanced branch does not reassign base-class pointers (cross-branch)
- **Where:** `src/Model/ModelUtilities/BoundaryPackageExt.f90`,
  `bndext_da` (advanced `isadvpak /= 0` branch) vs the list/array branch.
- **Observed:** The list/array branch deallocates `AUXVAR_IDM`,
  `BOUNDNAME_IDM`, `CELLID`, `NODEULIST`, then **reassigns**
  `this%boundname_cst => 'BOUNDNAME_CST'` and `this%auxvar => 'AUXVAR'`
  before the base `BndType%bnd_da()` runs. The advanced branch only
  deallocates `AUXVAR_IDM` and nullifies `featureauxvar`/`pkg_ifno`; it
  does **not** reassign `boundname_cst`/`auxvar`.
- **Why it matters:** If the base `BndType` deallocate path touches
  `boundname_cst`/`auxvar` (or expects them associated to specific
  managed names), the advanced branch could leave dangling/incorrect
  pointers or double-free. This is the base class for MAW/SFR/LAK/UZF and
  is also used by the APT transport packages.
- **Suggested action:** VERIFY the base `BndType` `_da` for advanced
  packages does not deallocate or dereference `boundname_cst`/`auxvar`
  under names the advanced branch did not set up. If it does, mirror the
  reassignment (or the appropriate nullify) in the advanced branch.

### M3 — `LoadContext%init` `named_bound` reduced from array to scalar (cross-branch)
- **Where:** `src/Utilities/Idm/LoadContext.f90`, `init` signature and
  the removed `sum_named_bounds`.
- **Observed:** `named_bound` changed from `character(:), dimension(:)`
  to a single scalar `character(*)`, and `sum_named_bounds` (which summed
  multiple named dimension variables into `maxbound`) was deleted.
  `maxbound` is now resolved by a single `setval(this%maxbound,
  this%named_bound, ...)`.
- **Why it matters:** Any caller/package that previously depended on
  summing multiple dimension tokens into `maxbound` loses that behavior.
  This is a genuine capability removal in shared framework code.
- **Suggested action:** VERIFY no package on any of the five branches
  passed more than one `named_bound` token to `ctx%init` (search callers
  of `%init(... named_bound=...)`). If none did, downgrade to Minor and
  note the simplification; if any did, restore summing or handle
  explicitly.

### M4 — Lost validation: diversion **target reach** (`iconr`) range check dropped
- **Where:** `src/Model/GroundWaterFlow/gwf-sfr.f90`,
  `sfr_source_diversions` (~line 1489: `this%divreach(jpos) = d_iconr(i)`).
- **Observed (behavior-preservation vs develop):** develop's
  `sfr_read_diversions` validated the diversion target reach and errored
  with `'Diversion target reach number should be between 1 and <n>.'`
  before storing it. The branch validates the diversion **source** reach
  (`d_ifno`) and the **diversion number** (`d_idv`), but assigns
  `divreach(jpos) = d_iconr(i)` with **no range check** on `iconr`.
- **Why it matters:** An out-of-range diversion target reach used to be a
  clean input error. Now it is stored unchecked and later dereferenced
  (e.g. `n2 = this%divreach(jpos)` at ~line 3936, and in the connection/
  upstream-reach checks ~lines 4573/4748/4772/4838). Depending on the
  value this risks an out-of-bounds array access or silently wrong
  routing rather than a helpful error.
- **Suggested action:** Restore a `iconr` range check
  (`1 <= d_iconr(i) <= this%maxbound`) in `sfr_source_diversions`, with
  the develop-equivalent message, before assigning `divreach`. Add a
  regression case (an out-of-range diversion target reach) — the existing
  `test_gwf_sfr_errors.py` is the natural home. Note: `sfr_check_diversions`
  should also be checked to confirm it does not already cover this (it did
  not appear to during review, but confirm).

---

## Minor

### m1 — `bedk_set`/`manning_set`/`ustrf_set` are never reset (sticky overrides)
- **Where:** `src/Model/GroundWaterFlow/gwf-sfr.f90`: flags initialized
  `.false.` once (~lines 517–519), set `.true.` in `sfr_set_period_value`
  (~4889/4892/4895), consumed in `sfr_ad` (~1983–1985); never reset.
- **Observed:** Once a reach receives a PERIOD `BEDK`/`MANNING`/
  `UPSTREAM_FRAC`, its `_set` flag stays `.true.` for the rest of the
  run, so `sfr_ad` permanently stops re-syncing `hk`/`rough`/`ustrf` from
  the PACKAGEDATA TS-linked values for that reach.
- **Why it matters:** This is almost certainly the intended MF6
  sticky-override semantics (a PERIOD override wins until reissued), and
  the PERIOD value itself is re-applied each timestep, so persistence is
  correct. The only nuance is that a reach can never *revert* to
  following its PACKAGEDATA TS after being overridden.
- **Suggested action:** VERIFY this matches legacy SFR behavior (it
  should). No code change expected; document the intent in a comment.

### m2 — SFR `CELLID == 'NONE'` special-case is hardcoded in generic StructArray (cross-branch)
- **Where:** `src/Utilities/Idm/mf6blockfile/StructArray.f90`,
  `read_param` MTYPE_INT2D branch: `if (subcomponent_type == 'SFR' .and.
  tagname == 'CELLID')`.
- **Observed:** Unconnected-reach handling (accepting the `NONE` keyword
  or `0 0 0`, storing zeros) is gated specifically on SFR inside generic
  framework code.
- **Why it matters:** If LAK/MAW/UZF also accept a `NONE`/unconnected
  cellid sentinel, this hardcode will need to be generalized on those
  branches. It is functionally correct for SFR today.
- **Suggested action:** Note as a generalization candidate; confirm
  whether other advanced packages need the same sentinel handling.

### m3 — Binary list input unsupported for ragged (`shape (:)`) columns
- **Where:** `src/Utilities/Idm/mf6blockfile/StructArray.f90`,
  `read_from_binary` MTYPE_INTVEC branch errors for
  `intvector_ragged`; introduced because CONNECTIONDATA `ic` shape
  changed from `(ncon(ifno))` to `(:)` in `gwf-sfr.dfn`.
- **Observed:** A ragged column cannot be read from binary list input; it
  errors "List style binary inputs not supported for self-sizing (ragged)
  columns."
- **Why it matters:** Potential regression only if SFR CONNECTIONDATA was
  ever read via the binary list path. CONNECTIONDATA uses `reader urword`
  (ASCII recarray, static load), so the binary list path almost certainly
  never applies here.
- **Suggested action:** VERIFY SFR CONNECTIONDATA has no binary-input
  path; if confirmed, this is a non-issue worth a one-line comment.

### m4 — `ts_update_indexed` relies on F2008 pointer rank-remapping
- **Where:** `src/Utilities/Idm/mf6blockfile/StructArray.f90`,
  `ts_update_indexed`: `featarr2d(1:1,1:size(featarr)) => featarr` and
  `raw2d(1:1,1:nrows) => dbl1d(1:nrows)`.
- **Observed:** Rank-remapping pointer assignment of a contiguous 1D
  target to a 2D pointer. `featarr` is declared `contiguous`; the SV
  `dbl1d` data pointers are contiguous.
- **Why it matters:** Standard F2008 and supported by gfortran/ifx/ifort,
  but it is a less-common feature; correctness depends on the source
  being contiguous.
- **Suggested action:** Low risk. Confirm the project's minimum compiler
  set supports it (it is already used elsewhere in modern MF6). No change
  expected.

### m5 — INTEGER keystring sentinel is now `IZERO`
- **Where:** `src/Utilities/Idm/mf6blockfile/StructArray.f90`,
  `read_from_parser_keystring` fill-sentinels loop (new `MTYPE_INT` case).
- **Observed:** Unmatched INTEGER member columns are filled with `IZERO`.
- **Why it matters:** For SFR the only such column is a numeric-index
  `IDV` (1-based via `numeric_index true`), so `0` is a safe "unset"
  sentinel. Fine as long as no INTEGER keystring member has `0` as a
  meaningful value.
- **Suggested action:** VERIFY no advanced-package INTEGER keystring
  member uses `0` as a valid value (none known for SFR).

### m6 — Lost behavior: `CELLID=NONE` deprecation warning dropped
- **Where:** develop `gwf-sfr.f90` `sfr_read_packagedata` emitted
  `deprecation_warning('PACKAGEDATA', 'CELLID=NONE', '6.4.3', ...)`
  ("Unconnected reaches should be specified with a zero for each grid
  dimension ... 0 0 0"). The branch handles `NONE` in
  `src/Utilities/Idm/mf6blockfile/StructArray.f90` `read_param`
  (MTYPE_INT2D, SFR CELLID) by storing zeros **silently**.
- **Observed (behavior-preservation vs develop):** `NONE` still works
  (accepted, treated as unconnected), but the deprecation warning that
  steered users toward `0 0 0` is gone. The branch's only remaining
  `deprecation_warning` in SFR is for `UNIT_CONVERSION` (unchanged).
- **Why it matters:** Loss of a user-facing deprecation notice, not a
  correctness bug. Note `test_gwf_sfr_unconnected_reach.py` confirms
  `NONE` still functions but does **not** assert the warning, so the test
  suite would not catch this loss.
- **Suggested action:** Decide whether the `CELLID=NONE` deprecation
  warning should be preserved. If yes, re-emit it (from the SFR source
  path once the CELLID column is available, or generically in the
  framework's NONE-handling branch). If the deprecation is intentionally
  being retired, note that explicitly. Cross-branch: if other advanced
  packages adopt the same NONE handling (see m2), apply the same
  decision.

---

## Questions / VERIFY

### Q1 — Transport storage-change test tolerances were loosened (not SFR-specific)
- **Where:** `autotest/test_gwe_sfe01sto.py` and
  `autotest/test_gwt_sft02sto.py` (one line each).
- **Observed:** Assertion changed from `np.allclose(diff, 0.0)` (diff of
  computed change vs answer, compared to zero — `atol` dominates near
  zero) to `np.allclose(<changes>, answers, rtol=1e-5)` (direct compare,
  relative tolerance).
- **Why it matters:** These are GWE/GWT SFE/SFT **storage** tests, not
  SFR. If the SFR IDM port changed numeric output enough to require
  loosening transport-package tolerances, that could indicate a subtle
  numeric delta in values read/applied (vs pure output-format evolution).
  The two assertion forms differ in near-zero behavior.
- **Suggested action:** Explicitly confirm whether this is format
  evolution already on develop vs a real numeric change introduced by the
  port. The briefing states all diffed tests were checked against the
  6.7.0 release binary; capture that conclusion here. If a real numeric
  delta exists, investigate its source before merge.

### Q2 — SPC/TVK/TVS PERIOD settings changed `STRING` → `DOUBLE` and dropped `_in` mf6internal (cross-branch)
- **Where:** `doc/mf6io/mf6ivar/dfn/utl-spc.dfn`, `utl-tvk.dfn`,
  `utl-tvs.dfn`; generated `src/Idm/utl-spcidm.f90`, `utl-tvkidm.f90`,
  `utl-tvsidm.f90`.
- **Observed:** `concentration/temperature/k/k22/k33/ss/sy` changed from
  `type string` + `mf6internal xxx_in` to `type double precision`
  (bare varname). The generated Fortran varnames dropped the `_IN`
  suffix and the type is now `DOUBLE`. Consumers (`TspSpc`, `TvBase`) do
  not reference the old `_in` names and consume via the dynamic keystring
  loader (`keystring_by_node` for TVK/TVS, `keystring_by_id` for SPC).
- **Why it matters:** These ride along on the framework but are not
  SFR-specific. The `STRING`→`DOUBLE` dispatch is a real change to how
  those values are stored/applied.
- **Suggested action:** VERIFY TVK/TVS/SPC runtime behavior is unchanged
  (the briefing reports 84/84 local tests pass including these). Confirm
  the `_IN` suffix is now supplied generically via `IDM_INPUT_SUFFIX`
  where a raw column and a permanent array share a name.

### Q3 — `bndext_ad` advances only observations (cross-branch)
- **Where:** `src/Model/ModelUtilities/BoundaryPackageExt.f90`,
  `bndext_ad` (new).
- **Observed:** Advances only `this%obs%obs_ad()`; deliberately does not
  call `TsManager%ad()`/`TasManager%ad()` (documented as unused for this
  type). For advanced packages, TS advance happens in the keystring
  loader's `ts_advance`.
- **Why it matters:** Correctness depends on there being exactly one TS
  advance per step for advanced packages, and none missing.
- **Suggested action:** VERIFY no double-advance or missing-advance of
  time series for advanced packages across a period/timestep boundary
  (SFR `sfr_ad` calls `BndExtType%bnd_ad()` then re-syncs TS fields).

### Q4 — Hardcoded classification tables pending a generated attribute (cross-branch, tracked)
- **Where:** `src/Utilities/Idm/LoadContext.f90`:
  - `is_advanced` — hardcoded list `LAK/MAW/SFR/UZF/LKT/MWT/SFT/UZT/
    LKE/MWE/SFE/UZE`.
  - `is_id_colname` — hardcoded list `IFNO/NUMBER/BNDNO/RNO/LAKENO/
    MAWNO/UZFNO`.
  - `record_dependency` — single hardcoded entry (SFR `DIVFLOW` →
    dimension `NDV`, sibling index `IDV`).
- **Observed:** These gate advanced-package behavior, id-addressed
  keystring classification, and record-follower sizing respectively. The
  code comments acknowledge these are interim, pending a
  `dfn2f90.py`-generated attribute (for `is_advanced`) and a generic
  record-follower classifier (for `record_dependency`, a known tracked
  design question per the briefing).
- **Why it matters:** New advanced packages or new record-follower fields
  must be added to these tables by hand; the `record_dependency` table
  fails loud for anything unlisted (by design, chosen over an unproven
  generic classifier).
- **Suggested action:** No change required for SFR. Keep these
  synchronized across all five branches. Track the generated-attribute
  work as follow-up.

### Q5 — `in_scope` and `resolve_context` now abort on unhandled cases (cross-branch)
- **Where:** `src/Utilities/Idm/LoadContext.f90`, `in_scope` `default`
  case and `resolve_context` `default` case.
- **Observed:** `in_scope` aborts (`store_error(..., .true.)`) for an
  optional param of an unrecognized subcomponent; `resolve_context`
  aborts for an unrecognized `load_scope`. Advanced packages short-circuit
  `in_scope` to `.true.` for all optional leaf params. `ROOT` scope now
  does no scalar setup (`set_scalars` stays `.false.`).
- **Why it matters:** Fail-loud is good, but it means adding any new
  subcomponent with optional params requires touching `in_scope`. The
  `ROOT`-scope no-op should be confirmed harmless.
- **Suggested action:** VERIFY `ROOT`-scoped loads never needed the
  scalar setup that the old `CONTEXT_UNDEF`-guarded path might have
  provided. Confirm the fail-loud default is intended.

---

## Behavior preservation vs develop (printing / error detection / validation)

Method: extracted develop's `gwf-sfr.f90` at the branch point and
diffed behavior-bearing constructs against the branch version
(`store_error`/`store_warning`/`deprecation_warning` calls,
`write(this%iout ...)` prints, `errmsg`/`warnmsg` message text, and the
`sfr_check_*` validation routines and their call sites).

**Structural mapping (no routine lost):** the develop input readers were
renamed, not removed — `sfr_read_options/dimensions/packagedata/
connectiondata/crossection/diversions/initial_stages` →
`sfr_source_*`, and `sfr_set_stressperiod` → `sfr_set_period_value` plus
the PERIOD `rp` loop. `write(this%iout ...)` list-file prints went
35 → 37 (nothing dropped; the PERIOD echo table and block "PROCESSING/
END OF" banners are preserved).

**Validation routines intact:** all seven `sfr_check_*` routines
(`connections`, `conversion`, `diversions`, `initialstages`, `reaches`,
`storage_weight`, `ustrf`) are present and still called with the same
call counts. The reach-parameter checks (length/width/slope/hk >
threshold, Manning's roughness, upstream fraction, bed-bottom vs cell
bottom), the connectivity checks ("is connected to", "downstream
connected reach", "is not a upstream reach"), the initialstage "less
than the reach top" check, the CPRIOR enum check, and the duplicate/
missing coverage checks all survive.

**Mechanical change (not a loss):** develop's `parser%StoreErrorUnit`
(12 sites) is replaced by `store_error_filename(this%input_fname)`
(16 sites) — the IDM equivalent that attaches the input filename to
stored errors. Confirm this still terminates the run as before, but no
error *detection* is lost by this swap.

**Two behaviors lost** (detailed above): **M4** (diversion target reach
`iconr` range check) and **m6** (`CELLID=NONE` deprecation warning).
These are the only substantive develop-vs-branch behavior regressions
found in the SFR package. The overall `store_error` call count dropped
74 → 47, but the bulk of that reduction is input-reading/block-format
errors that are now the IDM framework's responsibility (e.g. TAB6
keyword ordering, block read errors, per-read reach-range checks); those
were spot-checked and are handled upstream. M4/m6 are the two that are
not covered elsewhere.

---

## Confirmed correct (briefing items verified in code)

These were the five SFR package bugs and the dedup fix from the briefing;
each was located and confirmed in the diff.

1. **`NREACHES` release / PERIOD keystring sizing** — advanced packages
   get their feature count via the `PACKAGEDATA_IFNO` → `MAXBOUND` bridge
   in `LoadMf6File.f90` `parse_structarray_block` (guarded by
   `is_id_addressed_keystring .and. ctx%is_advanced`), consumed by
   `resolve_nfeatures` in the keystring loader. Mechanism present.
2. **`mf6dimension`** — `dfn2f90.py` supports the new attribute
   (mutually exclusive with `shape`, treated as scalar dependency);
   SFR's `stage/inflow/rainfall/evaporation/runoff` carry
   `mf6dimension (nreaches)`. Generated `gwfsfr_stage` is `DOUBLE` with
   `shape = NREACHES`, `timeseries = .true.`. Correct.
3. **`CPRIOR` enum validation** — `gwf-sfr.f90` (~lines 1490–1497)
   validates `UPTO/THRESHOLD/FRACTION/EXCESS`, else "Invalid cprior
   type" error. `FRACTION` range check `0.0..1.0` at ~line 1824. Present.
4. **`DEV_NO_FINAL_CHECK`** — added to `gwf-sfr.dfn`
   (`mf6internal iconvchk`); `gwf-sfr.f90` reads presence under
   `ICONVCHK` (~line 876). Present.
5. **`DEV_NO_CHECK`** — reads presence under the `mf6internal`-mapped key
   `ICHECK` (~line 868), not the tag name. Fix confirmed.
6. **Cross-record PERIOD dedup** — keystring loader `apply_settings`/
   `apply_record_targets` build a per-row `row_addr` (0 for rows not
   setting a member) and apply via `ts_update_indexed`, and
   `apply_auxiliary` clears the AUX struct-array column each period, so
   reissuing one compound record (DIVERSION/AUXILIARY) does not disturb
   the other's TS tracking. `reset()` short-circuits for
   `has_setting_dispatch` so links persist. Covered by
   `autotest/test_gwf_sfr_period_dedup.py`.

**STATUS persistence (initially flagged, resolved):** SFR `STATUS` is a
`type string` keystring member and is *not* given a framework permanent
per-tag array (the generic settings dispatch only handles
`DOUBLE`+`time_series`). This is fine because the package applies STATUS
to its own permanent `iboundpak(n)` array (0/1/-1) only for rows that set
it, and `iboundpak` persists across periods. Legacy sticky-STATUS
semantics are preserved. (Cross-branch note: LAK/MAW likely do the same
with their own ibound arrays — worth a consistency glance when porting.)

---

## Coverage of the new/changed tests

- `autotest/test_gwf_sfr_errors.py` (new) — 5 input-validation error
  cases: duplicate/missing PACKAGEDATA IFNO, out-of-range CONNECTIONDATA
  IFNO, mutual-upstream connectivity contradiction, OBS bad reach,
  misspelled CPRIOR. Good validation coverage.
- `autotest/test_gwf_sfr_unconnected_reach.py` (new) — 2 cases covering
  both the `0 0 0` literal and the `NONE` keyword cellid sentinels;
  directly exercises the hardcoded SFR CELLID='NONE' path (m2). Verifies
  the unconnected reach is absent from the GWF-exchange budget.
- `autotest/test_gwf_sfr_period_dedup.py` (new) — targeted regression for
  the cross-record dedup mechanism (item 6 above).
- `autotest/test_gwf_ts_sfr01.py` (+474), `test_gwf_sfr_tbedk.py` (+291)
  — assertions were *strengthened* (explicit "TS value differs between
  timesteps" checks, listing-file value checks, per-period temp/conc).
  `eval_bud_diff` edits are path refactors, not logic weakening.
- `autotest/test_gwe_sfe01sto.py`, `test_gwt_sft02sto.py` — the only
  tolerance *loosening* observed; see Q1.

---

## Cross-branch summary (for porting to idm_lak / idm_maw / idm_uzf / idm_transport_adv)

Shared-framework files changed here that must stay in sync:
`LoadContext.f90`, `Mf6FileKeystring.f90`, `Mf6FileList.f90`,
`StructArray.f90`, `StructVector.f90`, `LoadMf6File.f90`,
`Mf6FileGridArray.f90`, `Mf6FileLayerArray.f90`, `Constants.f90`,
`TimeSeriesManager.f90`, `MemoryManager.f90`, `BlockParser.f90`,
`BoundaryPackageExt.f90`, `dfn2f90.py`.

Highest cross-branch attention: **M1** (generator, affects all PERIOD
packages if regenerated), **M2** (`bndext_da` pointer asymmetry, base
class for all advanced packages + APT), **M3** (`named_bound` scalar),
**Q2** (SPC/TVK/TVS type change), **Q3** (`bndext_ad` single TS advance),
**Q4** (hardcoded classification tables), **m2** (SFR-hardcoded CELLID
`NONE`). The `Mf6FileList.f90` `apply_persistent_settings` path is
**not exercised by SFR** (SFR has no plain-recarray PERIOD block); it was
reviewed for correctness and looks right, but its live validation belongs
to `idm_uzf` — diff the two branches' `Mf6FileList.f90` copies before
relying on it.
