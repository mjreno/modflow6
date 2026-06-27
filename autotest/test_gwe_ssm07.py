"""
Smoke test that SPC time-series temperatures are correctly applied in GWE
stress periods where the SPC package has no explicit period data (reuse
periods).

Analogous to test_gwt_ssm07.py but using a GWE (heat transport) model with
TEMPERATURE as the dependent variable.  Verifies that the GWE SSM FILEINPUT
path works end-to-end for the list-based SPC case and that IDM-updated time-
series temperature values propagate into reuse stress periods.

Model layout
------------
1 layer, 1 row, 4 columns.  CHD at the last column; array-based recharge on
all other columns.  Three stress periods of 1 day each, 1 step each.

SPC file has a PERIOD 1 block only; periods 2-3 are reuse periods.
Time-series temperatures rise linearly: T(t) = 1 + t.
The SSM energy flux (q = Q_water * T * rho_w * Cp_w) for the recharge
boundaries should therefore increase with each successive stress period.
"""

import os

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = ["ssm07ts_gwe"]

nlay, nrow, ncol = 1, 1, 4
n_rch_cols = ncol - 1
nper = 3
perlen = [1.0] * nper
nstp = [1] * nper
total_time = float(sum(perlen))

Cpw = 4184.0
rhow = 1000.0
Cps = 703.7
rhos = 2700.0


def build_models(idx, test):
    ws = test.workspace
    name = cases[idx]
    gwfname = f"gwf_{name}"
    gwename = f"gwe_{name}"

    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    flopy.mf6.ModflowTdis(
        sim,
        time_units="DAYS",
        nper=nper,
        perioddata=[(p, s, 1.0) for p, s in zip(perlen, nstp)],
    )

    # ------------------------------------------------------------------
    # GWF
    # ------------------------------------------------------------------
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True)
    imsgwf = flopy.mf6.ModflowIms(
        sim, print_option="SUMMARY", filename=f"{gwfname}.ims"
    )
    sim.register_ims_package(imsgwf, [gwf.name])

    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=1.0,
        delc=1.0,
        top=1.0,
        botm=[0.0],
    )
    flopy.mf6.ModflowGwfic(gwf, strt=1.0)
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=0, k=1.0)

    flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=[[(0, 0, ncol - 1), 1.0]],
        pname="CHD-1",
    )

    # List-based RCH on the first n_rch_cols columns
    rch_spd = {
        kper: [[(0, 0, j), 1.0] for j in range(n_rch_cols)] for kper in range(nper)
    }
    flopy.mf6.ModflowGwfrch(
        gwf,
        maxbound=n_rch_cols,
        stress_period_data=rch_spd,
        pname="RCH-1",
        save_flows=True,
        filename=f"{gwfname}.rch1",
    )

    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        saverecord=[("HEAD", "LAST"), ("BUDGET", "LAST")],
    )

    # ------------------------------------------------------------------
    # GWE
    # ------------------------------------------------------------------
    gwe = flopy.mf6.MFModel(
        sim,
        model_type="gwe6",
        modelname=gwename,
        model_nam_file=f"{gwename}.nam",
    )
    gwe.name_file.save_flows = True
    imsgwe = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        linear_acceleration="BICGSTAB",
        filename=f"{gwename}.ims",
    )
    sim.register_ims_package(imsgwe, [gwe.name])

    flopy.mf6.ModflowGwedis(
        gwe,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=1.0,
        delc=1.0,
        top=1.0,
        botm=[0.0],
    )
    flopy.mf6.ModflowGweic(gwe, strt=0.0)
    flopy.mf6.ModflowGweadv(gwe)
    flopy.mf6.ModflowGweest(
        gwe,
        porosity=0.1,
        heat_capacity_solid=Cps,
        density_solid=rhos,
    )

    # SSM: one SPC file for RCH-1
    flopy.mf6.ModflowGwessm(
        gwe,
        print_flows=True,
        sources=[()],
        fileinput=[("RCH-1", f"{gwename}.rch1.spc")],
    )

    # ------------------------------------------------------------------
    # SPC: PERIOD 1 only (periods 2-3 are reuse)
    # Temperature follows T(t) = 1 + t (linear, 1 unit per day)
    # ------------------------------------------------------------------
    tsnames = [f"trch-{j + 1}" for j in range(n_rch_cols)]
    ts_data = [
        tuple([float(t)] + [1.0 + float(t)] * n_rch_cols)
        for t in range(int(total_time) + 1)
    ]
    ts_dict = {
        "timeseries": ts_data,
        "time_series_namerecord": tsnames,
        "interpolation_methodrecord": [n_rch_cols * ("linear",)],
        "filename": f"{gwename}.rch1.spc.ts",
    }
    pd = [[j, "TEMPERATURE", tsnames[j]] for j in range(n_rch_cols)]
    flopy.mf6.ModflowUtlspc(
        gwe,
        perioddata=pd,
        maxbound=n_rch_cols,
        filename=f"{gwename}.rch1.spc",
        timeseries=ts_dict,
        print_input=True,
    )

    flopy.mf6.ModflowGweoc(
        gwe,
        budget_filerecord=f"{gwename}.cbc",
        temperature_filerecord=f"{gwename}.ucn",
        saverecord=[("TEMPERATURE", "ALL"), ("BUDGET", "ALL")],
    )

    flopy.mf6.ModflowGwfgwe(
        sim,
        exgtype="GWF6-GWE6",
        exgmnamea=gwfname,
        exgmnameb=gwename,
        filename=f"{name}.gwfgwe",
    )

    return sim, None


def check_output(idx, test):
    """
    Check that the SSM energy flux for the recharge boundaries increases
    monotonically across the three stress periods, matching the rising
    time-series temperatures.
    """
    name = test.name
    gwename = f"gwe_{name}"

    fpth = os.path.join(test.workspace, f"{gwename}.cbc")
    bobj = flopy.utils.CellBudgetFile(fpth, precision="double")
    ssmbudall = bobj.get_data(text="SOURCE-SINK MIX")

    assert len(ssmbudall) == nper, (
        f"Expected {nper} budget records, got {len(ssmbudall)}"
    )

    # RCH-1 entries appear first (FILEINPUT before sinks); n_rch_cols entries
    mean_q = []
    for kper in range(nper):
        ssmbud = ssmbudall[kper]
        rch_q = ssmbud["q"][:n_rch_cols]
        pos_q = rch_q[rch_q > 0]
        assert len(pos_q) > 0, (
            f"Period {kper + 1}: no positive SSM energy flux found for RCH-1; "
            f"got q={rch_q}"
        )
        mean_q.append(float(np.mean(pos_q)))

    for i in range(1, nper):
        assert mean_q[i] > mean_q[i - 1], (
            f"{name}: SSM energy flux for RCH-1 did not increase from period "
            f"{i} to period {i + 1}: {mean_q[i - 1]:.6f} -> {mean_q[i]:.6f}.\n"
            f"Expected temperature to rise with the time series in reuse "
            f"stress periods."
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
