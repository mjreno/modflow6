"""
Test that SPC/SPCA time-series concentrations are correctly applied in
stress periods where the SPC package has no explicit period data (reuse
periods).

When a SPC/SPCA file has a PERIOD block only for period 1, the concentration
values for periods 2+ come entirely from time-series or time-array-series
interpolation.  This test verifies that spc_ad re-applies those IDM-updated
values to dblvec at every time step, including time steps that fall inside
reuse periods.

Two sub-cases exercise the same behaviour through different code branches:
  ssm07ts_list  -- list-based SPC (UTL-SPC) with scalar time series
                   used with a list-based RCH flow package
  ssm07ts_arr   -- array-based SPCA (UTL-SPCA) with time-array series
                   used with an array-based RCHA flow package

Model layout
------------
1 layer, 1 row, 4 columns.  CHD at the last column only (not covered by
the recharge package) so RCH/RCHA entries appear first in the SSM budget
and their count is known.  Three stress periods of 1 day each, 1 step each.

SPC / SPCA file has a PERIOD 1 block only; periods 2-3 are reuse periods.

Time-series concentrations rise linearly: C(t) = 1 + t.
The SSM mass-flux (q = Q_water * C) for the recharge boundaries should
therefore increase with each successive stress period.
"""

import os

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = ["ssm07ts_list", "ssm07ts_arr"]

nlay, nrow, ncol = 1, 1, 4
# Recharge covers the first (ncol - 1) columns; CHD is at the last column.
n_rch_cols = ncol - 1
nper = 3
perlen = [1.0] * nper
nstp = [1] * nper
total_time = float(sum(perlen))


def build_models(idx, test):
    ws = test.workspace
    name = cases[idx]
    gwfname = f"gwf_{name}"
    gwtname = f"gwt_{name}"

    sim = flopy.mf6.MFSimulation(
        sim_name=name, version="mf6", exe_name="mf6", sim_ws=ws
    )
    flopy.mf6.ModflowTdis(
        sim,
        time_units="DAYS",
        nper=nper,
        perioddata=[(p, s, 1.0) for p, s in zip(perlen, nstp)],
    )

    # GWF
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

    # CHD at the last column only (columns 0..n_rch_cols-1 receive recharge)
    flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=[[(0, 0, ncol - 1), 1.0]],
        pname="CHD-1",
    )

    if idx == 0:
        # list-based RCH on the first n_rch_cols columns only
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
    else:
        # array-based RCHA (must be used with array-based SPCA)
        rch_arr = np.ones((nrow, ncol))
        flopy.mf6.ModflowGwfrcha(
            gwf,
            recharge={kper: rch_arr for kper in range(nper)},
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

    # GWT
    gwt = flopy.mf6.MFModel(
        sim,
        model_type="gwt6",
        modelname=gwtname,
        model_nam_file=f"{gwtname}.nam",
    )
    gwt.name_file.save_flows = True
    imsgwt = flopy.mf6.ModflowIms(
        sim,
        print_option="SUMMARY",
        linear_acceleration="BICGSTAB",
        filename=f"{gwtname}.ims",
    )
    sim.register_ims_package(imsgwt, [gwt.name])
    flopy.mf6.ModflowGwtdis(
        gwt,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=1.0,
        delc=1.0,
        top=1.0,
        botm=[0.0],
    )
    flopy.mf6.ModflowGwtic(gwt, strt=0.0)
    flopy.mf6.ModflowGwtadv(gwt)
    flopy.mf6.ModflowGwtmst(gwt, porosity=0.1)

    # SSM: no AUX sources, one SPC file
    flopy.mf6.ModflowGwtssm(
        gwt,
        print_flows=True,
        sources=[()],
        fileinput=[("RCH-1", f"{gwtname}.rch1.spc")],
    )

    # SPC / SPCA: PERIOD 1 data only
    # Concentrations follow C(t) = 1 + t  (linear, 1 unit per day)
    # Time-series anchor points at integer times 0, 1, 2, 3.
    if idx == 0:
        # list-based SPC with scalar time series
        tsnames = [f"crch-{j + 1}" for j in range(n_rch_cols)]
        ts_data = [
            tuple([float(t)] + [1.0 + float(t)] * n_rch_cols)
            for t in range(int(total_time) + 1)
        ]
        ts_dict = {
            "timeseries": ts_data,
            "time_series_namerecord": tsnames,
            "interpolation_methodrecord": [n_rch_cols * ("linear",)],
            "filename": f"{gwtname}.rch1.spc.ts",
        }
        # Bind each of the n_rch_cols bounds to a time-series name (period 1 only)
        pd = [[j, "CONCENTRATION", tsnames[j]] for j in range(n_rch_cols)]
        flopy.mf6.ModflowUtlspc(
            gwt,
            perioddata=pd,
            maxbound=n_rch_cols,
            filename=f"{gwtname}.rch1.spc",
            timeseries=ts_dict,
            print_input=True,
        )
    else:
        # array-based SPCA with time-array series (period 1 only)
        spc = flopy.mf6.ModflowUtlspca(
            gwt,
            concentration="TIMEARRAYSERIES carray",
            filename=f"{gwtname}.rch1.spc",
            print_input=True,
        )
        # Write one external dat file per anchor time; each contains a
        # uniform array value for the whole model layer.
        tas_array = {}
        for t in range(int(total_time) + 1):
            dat_name = f"{gwtname}.rch1.spc.tas.t{t}.dat"
            tas_array[float(t)] = dat_name
            np.savetxt(
                os.path.join(ws, dat_name),
                np.full((nrow, ncol), 1.0 + float(t)),
                fmt="%7.3f",
            )
        spc.tas.initialize(
            filename=f"{gwtname}.rch1.spc.tas",
            tas_array=tas_array,
            time_series_namerecord="carray",
            interpolation_methodrecord="linear",
        )

    flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.cbc",
        concentration_filerecord=f"{gwtname}.ucn",
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
    )

    flopy.mf6.ModflowGwfgwt(
        sim,
        exgtype="GWF6-GWT6",
        exgmnamea=gwfname,
        exgmnameb=gwtname,
        filename=f"{name}.gwfgwt",
    )

    return sim, None


def check_output(idx, test):
    """
    Check that the SSM mass flux (q) for the recharge boundaries increases
    monotonically across the three stress periods, matching the rising
    time-series concentrations.

    The SSM budget is a flat array per time step containing entries for all
    packages in SSM-processing order.  The RCH-1 SPC entries come first
    (listed in the FILEINPUT block), followed by the CHD-1 sink entries.
    For the list-based case RCH-1 has n_rch_cols entries; for the array-based
    case it has ncol entries (one per cell in the layer).
    """
    name = test.name
    gwtname = f"gwt_{name}"

    fpth = os.path.join(test.workspace, f"{gwtname}.cbc")
    bobj = flopy.utils.CellBudgetFile(fpth, precision="double")
    ssmbudall = bobj.get_data(text="SOURCE-SINK MIX")

    assert len(ssmbudall) == nper, (
        f"Expected {nper} budget records, got {len(ssmbudall)}"
    )

    # Number of RCH-1 SSM entries per time step
    n_rch_entries = n_rch_cols if idx == 0 else ncol

    # Collect the mean positive SSM flux for RCH-1 across periods
    mean_q = []
    for kper in range(nper):
        ssmbud = ssmbudall[kper]
        # RCH-1 entries are the first n_rch_entries rows in the budget record
        rch_q = ssmbud["q"][:n_rch_entries]
        # Only count entries with positive flux (active recharge cells)
        pos_q = rch_q[rch_q > 0]
        assert len(pos_q) > 0, (
            f"Period {kper + 1}: no positive SSM flux found for RCH-1; got q={rch_q}"
        )
        mean_q.append(float(np.mean(pos_q)))

    # C(t) = 1 + t is strictly increasing, so SSM q must increase each period.
    for i in range(1, nper):
        assert mean_q[i] > mean_q[i - 1], (
            f"{name}: SSM flux for RCH-1 did not increase from period {i} "
            f"to period {i + 1}: {mean_q[i - 1]:.6f} -> {mean_q[i]:.6f}.\n"
            f"Expected concentration to rise with the time series in reuse "
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
