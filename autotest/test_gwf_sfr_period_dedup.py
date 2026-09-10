"""SFR's PERIOD block has two compound records (AUXILIARYRECORD,
DIVERSIONRECORD) in the same keystring -- reissuing one must not disturb
the other's own tracking.

  sfr-divaux-dedup   period 1 reissues DIVERSION only (AUXILIARY must
                      keep tracking its own TS); period 2 reissues
                      AUXILIARY only (DIVFLOW must hold its period-1
                      literal, unreissued)

  sfr-div-ts          reach 4's diversion (reach count > total
                      diversions) goes literal -> TS-linked -> an
                      unreissued gap period -> literal again, 2
                      timesteps/period throughout; reach 1's own
                      diversion is untouched the whole time as a
                      cross-check
"""

import os

import flopy
import numpy as np
import pytest
from framework import TestFramework

paktest = "sfr"
cases = ["sfr-divaux-dedup", "sfr-div-ts"]

nper = 3
inflow = 10.0
div_ts_vals = [0.3] * 4
conc_ts_vals = [50.0, 50.0, 100.0, 200.0]
div_literal = 0.7
conc_literal = 99.0

div_expected = [0.3, div_literal, div_literal]
conc_expected = [50.0, 100.0, conc_literal]


def _build_divaux_dedup(ws, name):
    """AUXILIARY and DIVERSION each start TS-linked, then each is
    reissued as a literal on a different period than the other."""
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    flopy.mf6.ModflowTdis(sim, nper=nper, perioddata=[(1.0, 1, 1.0)] * nper)
    flopy.mf6.ModflowIms(sim, print_option="NONE")
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)
    flopy.mf6.ModflowGwfdis(
        gwf, nlay=1, nrow=1, ncol=1, delr=100.0, delc=100.0, top=0.0, botm=-10.0
    )
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=0)
    flopy.mf6.ModflowGwfic(gwf, strt=0.0)

    cellid = (0, 0, 0)
    sfrrch = [cellid, 100.0, 5.0, 1e-3, 0.0, 1.0, 0.0, 0.03]
    packagedata = [
        [0] + sfrrch + [2, 1.0, 1, "conc_ts"],
        [1] + sfrrch + [1, 0.0, 0, 0.0],
        [2] + sfrrch + [1, 1.0, 0, 0.0],
    ]
    connectiondata = [[0, -1, -2], [1, 0], [2, 0]]
    diversiondata = [[0, 0, 1, "FRACTION"]]
    perioddata = {
        0: [
            [0, "inflow", inflow],
            [0, "diversion", 0, "div_ts"],
            [0, "AUXILIARY", "conc", "conc_ts"],
        ],
        1: [[0, "inflow", inflow], [0, "diversion", 0, div_literal]],
        2: [[0, "inflow", inflow], [0, "AUXILIARY", "conc", conc_literal]],
    }
    sfr = flopy.mf6.ModflowGwfsfr(
        gwf,
        print_input=True,
        auxiliary=["conc"],
        budget_filerecord=f"{name}.{paktest}.cbc",
        nreaches=3,
        packagedata=packagedata,
        connectiondata=connectiondata,
        diversions=diversiondata,
        perioddata=perioddata,
        pname="sfr-1",
    )
    sfr.ts.initialize(
        filename=f"{name}.sfr.ts",
        timeseries=list(zip([0.0, 1.0, 2.0, 3.0], div_ts_vals, conc_ts_vals)),
        time_series_namerecord=["div_ts", "conc_ts"],
        interpolation_methodrecord=["linearend", "linearend"],
    )
    flopy.mf6.ModflowGwfoc(
        gwf, budget_filerecord=f"{name}.cbc", saverecord=[("BUDGET", "ALL")]
    )
    return sim


def _check_divaux_dedup(test, name):
    fname = os.path.join(test.workspace, f"{name}.{paktest}.cbc")
    cbc = flopy.utils.CellBudgetFile(fname, precision="double")

    outflow = cbc.get_data(text="EXT-OUTFLOW")
    div_obs = np.array([-rec["q"][1] / inflow for rec in outflow])
    assert np.allclose(div_obs, div_expected), (
        f"DIVFLOW fraction expected {div_expected}, got {div_obs}"
    )

    aux = cbc.get_data(text="AUXILIARY")
    auxcol = aux[0].dtype.names[-1]
    conc_obs = np.array([rec[auxcol][0] for rec in aux])
    assert np.allclose(conc_obs, conc_expected), (
        f"AUXILIARY conc expected {conc_expected}, got {conc_obs}"
    )


# reach 4 > total diversions (2), exercising the offset table's
# out-of-range case; 2 timesteps/period probes for stale TS values
div_ts_nper = 4
div_ts_inflow = 10.0
r1_frac = 0.1
div4_literal0 = 0.2
div4_literal1 = 0.9
div4_ts_times = [2.0, 3.0, 4.0, 5.0, 6.0, 7.0]
div4_ts_vals = [0.30, 0.35, 0.40, 0.45, 0.50, 0.55]
# (totim, reach 1 expected fraction, reach 4 expected fraction)
div4_checks = [
    (2.0, r1_frac, div4_literal0),
    (3.0, r1_frac, 0.35),
    (4.0, r1_frac, 0.40),
    (5.0, r1_frac, 0.45),
    (6.0, r1_frac, 0.50),
    (8.0, r1_frac, div4_literal1),
]


def _build_div_ts(ws, name):
    """Reach 1 (control) and reach 4 each get their own two termini, so
    each's diverted fraction is independently observable."""
    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    flopy.mf6.ModflowTdis(
        sim, nper=div_ts_nper, perioddata=[(2.0, 2, 1.0)] * div_ts_nper
    )
    flopy.mf6.ModflowIms(sim, print_option="NONE")
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)
    flopy.mf6.ModflowGwfdis(
        gwf, nlay=1, nrow=1, ncol=1, delr=100.0, delc=100.0, top=0.0, botm=-10.0
    )
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=0)
    flopy.mf6.ModflowGwfic(gwf, strt=0.0)

    cellid = (0, 0, 0)
    src = [cellid, 100.0, 5.0, 1e-3, 0.0, 1.0, 0.0, 0.03]
    term = [cellid, 100.0, 5.0, 1e-3, 0.0, 1.0, 0.0, 0.03]
    packagedata = [
        [0] + src + [2, 1.0, 1],  # reach1: source, 1 diversion
        [1] + term + [1, 1.0, 0],  # reach2: reach1's main terminus
        [2] + term + [1, 0.0, 0],  # reach3: reach1's diversion terminus
        [3] + src + [2, 1.0, 1],  # reach4: source, 1 diversion
        [4] + term + [1, 1.0, 0],  # reach5: reach4's main terminus
        [5] + term + [1, 0.0, 0],  # reach6: reach4's diversion terminus
    ]
    connectiondata = [
        [0, -1, -2],
        [1, 0],
        [2, 0],
        [3, -4, -5],
        [4, 3],
        [5, 3],
    ]
    diversiondata = [
        [0, 0, 2, "FRACTION"],
        [3, 0, 5, "FRACTION"],
    ]  # reach1->3, reach4->6
    perioddata = {
        0: [
            [0, "inflow", div_ts_inflow],
            [3, "inflow", div_ts_inflow],
            [0, "diversion", 0, r1_frac],
            [3, "diversion", 0, div4_literal0],
        ],
        1: [[3, "diversion", 0, "div_ts"]],
        2: [],
        3: [[3, "diversion", 0, div4_literal1]],
    }
    sfr = flopy.mf6.ModflowGwfsfr(
        gwf,
        print_input=True,
        budget_filerecord=f"{name}.{paktest}.cbc",
        nreaches=6,
        packagedata=packagedata,
        connectiondata=connectiondata,
        diversions=diversiondata,
        perioddata=perioddata,
        pname="sfr-1",
    )
    sfr.ts.initialize(
        filename=f"{name}.sfr.ts",
        timeseries=list(zip(div4_ts_times, div4_ts_vals)),
        time_series_namerecord=["div_ts"],
        interpolation_methodrecord=["linearend"],
    )
    flopy.mf6.ModflowGwfoc(
        gwf, budget_filerecord=f"{name}.cbc", saverecord=[("BUDGET", "ALL")]
    )
    return sim


def _check_div_ts(test):
    # diversion destinations are reach 3 (reach 1's) and reach 6 (reach 4's)
    fname = os.path.join(test.workspace, f"sfr-div-ts.{paktest}.cbc")
    cbc = flopy.utils.CellBudgetFile(fname, precision="double")
    for totim, r1_expected, r4_expected in div4_checks:
        rec = cbc.get_data(totim=totim, text="EXT-OUTFLOW")[0]
        r1_obs = -rec["q"][rec["node"] == 3][0] / div_ts_inflow
        r4_obs = -rec["q"][rec["node"] == 6][0] / div_ts_inflow
        assert np.isclose(r1_obs, r1_expected), (
            f"t={totim}: reach 1 fraction expected {r1_expected}, got {r1_obs}"
        )
        assert np.isclose(r4_obs, r4_expected), (
            f"t={totim}: reach 4 fraction expected {r4_expected}, got {r4_obs}"
        )


def build_models(idx, test):
    name = cases[idx]
    if idx == 0:
        return _build_divaux_dedup(test.workspace, name)
    return _build_div_ts(test.workspace, name)


def check_output(idx, test):
    name = cases[idx]
    if idx == 0:
        _check_divaux_dedup(test, name)
        return
    _check_div_ts(test)


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
    )
    test.run()
