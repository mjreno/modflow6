"""A TVK PERIOD K value switches cleanly between two different time
series across periods, with no leftover value from the prior series.
Every period reissues K with a TS name, alternating between two
distinctly-valued series, so a stale link would show up as a wrong
value.
"""

import re

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = ["tvkswitch"]

# k_ts_a drives periods 1 and 3; k_ts_b drives period 2. Values are
# distinct at every stress period's start time so a stale link shows up
# as a value matching neither series.
TS_A_VALS = {0.0: 10.0, 1.0: 999.0, 2.0: 25.0, 3.0: 25.0}
TS_B_VALS = {0.0: 888.0, 1.0: 900.0, 2.0: 888.0, 3.0: 888.0}

EXPECTED_P1 = TS_A_VALS[0.0]
EXPECTED_P2 = TS_B_VALS[1.0]
EXPECTED_P3 = TS_A_VALS[2.0]


def build_models(idx, test):
    name = cases[idx]
    ws = test.workspace
    nlay, nrow, ncol = 1, 1, 3
    delr, delc = 1.0, 1.0
    top, botm = 0.0, [-1.0]
    idomain = np.ones((nlay, nrow, ncol), dtype=int)

    sim = flopy.mf6.MFSimulation(sim_name=name, exe_name="mf6", sim_ws=ws)
    flopy.mf6.ModflowTdis(
        sim, time_units="DAYS", nper=3, perioddata=[(1.0, 5, 1.0)] * 3
    )
    gwf = flopy.mf6.ModflowGwf(sim, modelname=name, save_flows=True)
    flopy.mf6.ModflowIms(sim, print_option="NONE", linear_acceleration="BICGSTAB")
    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
    )
    flopy.mf6.ModflowGwfic(gwf, strt=0.0)
    npf = flopy.mf6.ModflowGwfnpf(gwf, save_flows=True, icelltype=0, k=10.0, k33=10.0)
    flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=[[(0, 0, 0), -1.0], [(0, 0, ncol - 1), -2.0]],
        pname="CHD-1",
    )

    tvk = flopy.mf6.ModflowUtltvk(
        npf,
        print_input=True,
        perioddata={
            0: [((0, 0, 1), "K", "k_ts_a")],
            1: [((0, 0, 1), "K", "k_ts_b")],
            2: [((0, 0, 1), "K", "k_ts_a")],
        },
        filename=f"{name}.tvk",
    )
    ts_names = ["k_ts_a", "k_ts_b"]
    ts_data = [(t, TS_A_VALS[t], TS_B_VALS[t]) for t in (0.0, 1.0, 2.0, 3.0)]
    tvk.ts.initialize(
        filename="k.ts",
        timeseries=ts_data,
        time_series_namerecord=ts_names,
        interpolation_methodrecord=["stepwise"] * len(ts_names),
    )

    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{name}.cbc",
        head_filerecord=f"{name}.hds",
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    return sim


def check_output(idx, test):
    name = test.name
    lst_fname = test.workspace / f"{name}.lst"
    text = lst_fname.read_text()

    pattern = re.compile(
        r"Setting K value for cell\s+(\S+)\s+at start of\s+stress "
        r"period\s+(\d+)\s*=\s*([\d.eE+-]+)"
    )
    matches = pattern.findall(text)
    assert len(matches) > 0, (
        f"No 'Setting K value' lines found in list file {lst_fname}"
    )

    # only track the TS-linked cell (0,0,1) -> 1-based cellstr "(1,1,2)"
    vals_by_period = {}
    for cellstr, kper_str, val_str in matches:
        if "1,1,2" not in cellstr:
            continue
        kper = int(kper_str)
        val = float(val_str)
        vals_by_period.setdefault(kper, []).append(val)

    print(f"TVK switch-test applied K values by stress period: {vals_by_period}")

    assert 1 in vals_by_period, "No K applications logged for period 1"
    assert np.allclose(vals_by_period[1], EXPECTED_P1), (
        f"Period 1 (k_ts_a) expected {EXPECTED_P1}, got {vals_by_period[1]}"
    )

    assert 2 in vals_by_period, "No K applications logged for period 2"
    assert np.allclose(vals_by_period[2], EXPECTED_P2), (
        f"Period 2 (switched to k_ts_b) expected {EXPECTED_P2} -- if this "
        f"instead shows a k_ts_a value ({TS_A_VALS[1.0]}), a stale link "
        f"from period 1 is still driving the cell, got {vals_by_period[2]}"
    )

    assert 3 in vals_by_period, "No K applications logged for period 3"
    assert np.allclose(vals_by_period[3], EXPECTED_P3), (
        f"Period 3 (switched back to k_ts_a) expected {EXPECTED_P3} -- if "
        f"this instead shows a k_ts_b value ({TS_B_VALS[2.0]}), a stale "
        f"link from period 2 is still driving the cell, got "
        f"{vals_by_period[3]}"
    )


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
