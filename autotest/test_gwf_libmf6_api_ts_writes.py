"""
Verify that a time series overwrites a value written through the API at the
next time step when the written value is itself linked to the time series.

Companion to test_gwf_libmf6_api_writes.py, which writes values that are not
linked to a time series and requires them to persist. Here the written value
is linked, so the time series must win at the next advance:

    case   package  written (old name)  linked input
    maw    MAW      WELL_HEAD           PACKAGEDATA STRT (constant-head well)
    sfr    SFR      HK                  PACKAGEDATA RHK
    lak    LAK      OUTWIDTH            OUTLETS WIDTH

Each case also writes a value in the same column that is not linked, which
must persist:

    case   unlinked write
    maw    WELL_HEAD of well 1 (constant head, numeric STRT)
    sfr    HK of reach 1
    lak    OUTWIDTH of outlet 2

PACKAGEDATA and OUTLETS rows are listed out of feature order, so the input
row of each written entry differs from its feature number.

Each time series is stepwise with a breakpoint at the start of each time
step. A "flat" series holds one value; a "ramp" series changes every step.
The write happens once, during the first time step, at one of three points:

    phase   written after
    init    initialize
    pts     prepare_time_step
    ps      prepare_solve

Time series advance at the end of prepare_time_step, and the packages copy
linked values into their own arrays in prepare_solve, so a write after init
or pts is overwritten within the first time step, and a write after ps holds
for the first time step only.
"""

import flopy
import numpy as np
import pytest
from modflow_devtools.markers import requires_pkg

cases = ["maw", "sfr", "lak"]
series = ["flat", "ramp"]
phases = ["init", "pts", "ps"]

name = "api_ts_writes"
nper, perlen, nstp = 2, 4.0, 2
top, botm = 10.0, 0.0
tsname = "linked"

# -- case: (package, written old name, index, written value,
#           flat series value, ramp series values at each step start)
writes = {
    "maw": ("MAW-1", "WELL_HEAD", 1, 7.0, 9.0, [9.0, 8.5, 8.0, 7.5]),
    "sfr": ("SFR-1", "HK", 1, 0.0, 0.1, [0.1, 0.2, 0.3, 0.4]),
    "lak": ("LAK-1", "OUTWIDTH", 0, 10.0, 2.0, [2.0, 3.0, 4.0, 5.0]),
}

# -- case: (index, written value) of an unlinked entry of the same variable
unlinked = {
    "maw": (0, 8.0),
    "sfr": (0, 0.0),
    "lak": (1, 10.0),
}


def series_values(case, kind):
    """Expected value of the linked input at each time step."""
    _, _, _, _, flat, ramp = writes[case]
    return [flat] * (nper * nstp) if kind == "flat" else ramp


def get_sim(ws, case, kind):
    sim = flopy.mf6.MFSimulation(sim_name=name, sim_ws=ws)
    flopy.mf6.ModflowTdis(sim, nper=nper, perioddata=[(perlen, nstp, 1.0)] * nper)
    flopy.mf6.ModflowIms(
        sim,
        complexity="complex",
        outer_dvclose=1e-9,
        inner_dvclose=1e-9,
        outer_maximum=200,
        inner_maximum=200,
    )
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name)
    ncol = 5 if case == "maw" else 3
    flopy.mf6.ModflowGwfdis(
        gwf, nlay=1, nrow=1, ncol=ncol, delr=100.0, delc=100.0, top=top, botm=botm
    )
    k = 10.0 if case == "maw" else 1.0
    strt = 9.0 if case == "maw" else 8.0
    flopy.mf6.ModflowGwfic(gwf, strt=strt)
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=0, k=k)

    # -- stepwise series with a breakpoint at the start of each time step
    delt = perlen / nstp
    values = series_values(case, kind)
    timeseries = [(i * delt, v) for i, v in enumerate(values)]
    timeseries.append((nper * perlen, values[-1]))

    if case == "maw":
        flopy.mf6.ModflowGwfchd(
            gwf,
            stress_period_data={0: [[(0, 0, 0), strt], [(0, 0, ncol - 1), strt]]},
        )
        # -- both wells are constant head; well 2 STRT is linked to the time
        #    series
        pd = [[1, 0.1, botm, tsname, "THIEM", 1], [0, 0.1, botm, strt, "THIEM", 1]]
        cd = [[n, 0, (0, 0, 1 + 2 * n), top, botm, -999.0, -999.0] for n in range(2)]
        per = [[0, "STATUS", "CONSTANT"], [1, "STATUS", "CONSTANT"]]
        pkg = flopy.mf6.ModflowGwfmaw(
            gwf,
            nmawwells=2,
            packagedata=pd,
            connectiondata=cd,
            perioddata={0: per, 1: [[0, "RATE", -50.0]]},
            pname="MAW-1",
        )

    elif case == "sfr":
        flopy.mf6.ModflowGwfchd(gwf, stress_period_data={0: [[(0, 0, 0), strt]]})
        # -- reach 2 bed conductivity is linked to the time series
        rhk = [0.1, tsname, 0.1]
        ncon = [1, 2, 1]
        pd = [
            [i, (0, 0, i), 100.0, 5.0, 0.001, 9.0, 1.0, rhk[i], 0.03, ncon[i], 1.0, 0]
            for i in [1, 2, 0]
        ]
        cd = [[0, -1], [1, 0, -2], [2, 1]]
        per = [[0, "INFLOW", 10.0]]
        pkg = flopy.mf6.ModflowGwfsfr(
            gwf,
            nreaches=3,
            packagedata=pd,
            connectiondata=cd,
            perioddata={0: per, 1: per},
            pname="SFR-1",
        )

    elif case == "lak":
        flopy.mf6.ModflowGwfchd(gwf, stress_period_data={0: [[(0, 0, 2), strt]]})
        # -- outlet 1 width is linked to the time series
        pd = [[0, 11.0, 1]]
        cd = [[0, 0, (0, 0, 0), "VERTICAL", 0.001, 0.0, 0.0, 0.0, 0.0]]
        outlets = [
            [1, 0, -1, "WEIR", 10.8, 1.0, 0.03, 0.001],
            [0, 0, -1, "WEIR", 10.5, tsname, 0.03, 0.001],
        ]
        per = [[0, "RAINFALL", 0.5]]
        pkg = flopy.mf6.ModflowGwflak(
            gwf,
            nlakes=1,
            noutlets=2,
            packagedata=pd,
            connectiondata=cd,
            outlets=outlets,
            perioddata={0: per, 1: per},
            pname="LAK-1",
        )

    pkg.ts.initialize(
        filename=f"{name}.{case}.ts",
        timeseries=timeseries,
        time_series_namerecord=tsname,
        interpolation_methodrecord="stepwise",
    )
    return sim


def run_sim(lib, ws, case, phase=None):
    """Run through the API, writing the linked and the unlinked entry once at
    the given phase of the first time step (no write if phase is None).
    Returns both entries after each step."""
    from xmipy import XmiWrapper

    pname, var, idx, value, _, _ = writes[case]
    uidx, uvalue = unlinked[case]
    mf6 = XmiWrapper(lib, working_directory=str(ws))
    mf6.initialize()
    kstp = 0

    def write(at):
        if at == phase and kstp == 0:
            ptr = mf6.get_value_ptr(mf6.get_var_address(var, name.upper(), pname))
            ptr[idx] = value
            ptr[uidx] = uvalue

    try:
        write("init")
        written = []
        while mf6.get_current_time() < mf6.get_end_time():
            mf6.prepare_time_step(mf6.get_time_step())
            write("pts")
            nsol = mf6.get_subcomponent_count()
            for sol_id in range(1, nsol + 1):
                mf6.prepare_solve(sol_id)
                if sol_id == nsol:
                    write("ps")
                for _ in range(mf6.get_value("SLN_1/MXITER")[0]):
                    if mf6.solve(sol_id):
                        break
                else:
                    raise RuntimeError(f"no convergence at step {kstp + 1}")
                mf6.finalize_solve(sol_id)
            mf6.finalize_time_step()
            kstp += 1
            v = mf6.get_value(f"{name.upper()}/{pname}/{var}")
            written.append((v[idx], v[uidx]))
    finally:
        mf6.finalize()
    written = np.array(written)
    return written[:, 0], written[:, 1]


def check(lib, ws, case, kind, phase):
    _, var, _, value, _, _ = writes[case]
    values = np.array(series_values(case, kind))

    # -- without a write the variable follows the time series
    ref_ws = ws / "reference"
    get_sim(ref_ws, case, kind).write_simulation(silent=True)
    ref, _ = run_sim(lib, ref_ws, case)
    assert np.allclose(ref, values), (
        f"{var} does not follow the time series: {ref} (series {values})"
    )

    # -- with a write the time series wins at the next advance
    api_ws = ws / "api"
    get_sim(api_ws, case, kind).write_simulation(silent=True)
    written, uwritten = run_sim(lib, api_ws, case, phase)
    expected = values.copy()
    if phase == "ps":
        expected[0] = value
    assert np.allclose(written, expected), (
        f"{var} written after {phase} with a {kind} time series: {written} "
        f"after each step (expected {expected})"
    )

    # -- an unlinked entry of the same variable persists
    uidx, uvalue = unlinked[case]
    assert np.allclose(uwritten, uvalue), (
        f"unlinked {var}[{uidx}] written after {phase} was overwritten: "
        f"{uwritten} after each step (wrote {uvalue})"
    )


@requires_pkg("xmipy")
@pytest.mark.parametrize("phase", phases)
@pytest.mark.parametrize("kind", series)
@pytest.mark.parametrize("case", cases)
def test_api_ts_writes(case, kind, phase, function_tmpdir, targets):
    check(targets["libmf6"], function_tmpdir, case, kind, phase)
