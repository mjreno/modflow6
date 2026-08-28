"""A TVK PERIOD K value linked to a time series continues tracking that
series in a later period whose own PERIOD block reappears for a
different, unrelated cell, without repeating this cell's K setting.
Checks the applied K value TVK logs (PRINT_INPUT) at every timestep,
parsed from the GWF list file.
"""

import re

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = ["tvkcontinue"]

TS_VAL_P1 = 10.0
TS_VAL_P2 = 20.0
TS_VAL_P3 = 30.0


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
            0: [((0, 0, 1), "K", "k_ts")],
            # periods 1, 2: TVK's own PERIOD block reappears, setting a
            # different, unrelated cell's K, without repeating cell
            # (0,0,1)'s K setting
            1: [((0, 0, 0), "K", 5.0)],
            2: [((0, 0, 0), "K", 5.0)],
        },
        filename=f"{name}.tvk",
    )
    ts_data = [
        (0.0, TS_VAL_P1),
        (1.0, TS_VAL_P2),
        (2.0, TS_VAL_P3),
        (3.0, TS_VAL_P3),
    ]
    tvk.ts.initialize(
        filename="k.ts",
        timeseries=ts_data,
        time_series_namerecord="k_ts",
        interpolation_methodrecord="stepwise",
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

    # only track the TS-linked cell (0,0,1) -> 1-based cellstr "(1,1,2)";
    # ignore the distractor cell (0,0,0) used to force the PERIOD block to
    # reappear in periods 2/3
    vals_by_period = {}
    for cellstr, kper_str, val_str in matches:
        if "1,1,2" not in cellstr:
            continue
        kper = int(kper_str)
        val = float(val_str)
        vals_by_period.setdefault(kper, []).append(val)

    print(f"TVK applied K values by stress period: {vals_by_period}")

    assert 1 in vals_by_period, "No K applications logged for period 1"
    assert np.allclose(vals_by_period[1], TS_VAL_P1), (
        f"Period 1 K expected {TS_VAL_P1}, got {vals_by_period[1]}"
    )

    assert 2 in vals_by_period, (
        f"No K applications logged for period 2 -- TS-linked K did not "
        f"continue tracking after period 1 (period 2's TVK PERIOD block "
        f"reappears for a different cell without repeating this one). "
        f"vals_by_period={vals_by_period}"
    )
    assert np.allclose(vals_by_period[2], TS_VAL_P2), (
        f"Period 2 K expected {TS_VAL_P2}, got {vals_by_period[2]}"
    )

    assert 3 in vals_by_period, "No K applications logged for period 3"
    assert np.allclose(vals_by_period[3], TS_VAL_P3), (
        f"Period 3 K expected {TS_VAL_P3}, got {vals_by_period[3]}"
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
