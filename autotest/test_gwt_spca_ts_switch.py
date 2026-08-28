"""An SPCA (array-based SPC) CONCENTRATION value switches cleanly
between two different Time-Array Series across periods, with no
leftover value from the prior series. Every period reissues
CONCENTRATION TIMEARRAYSERIES, alternating between two
distinctly-valued series, so a stale link would show up as a wrong
value.

The .spc/.tas files are written directly rather than through flopy's
ModflowUtlspca, since two named time-array series in one .tas file
isn't exercised by flopy's API.
"""

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = ["spcaswitch"]

# conc_tas_a drives periods 1 and 3; conc_tas_b drives period 2. Values
# are distinct at every stress period's start time so a stale link
# shows up as a value matching neither series.
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

    gwfname = "gwf_" + name
    gwf = flopy.mf6.ModflowGwf(sim, modelname=gwfname, save_flows=True)
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
    flopy.mf6.ModflowGwfnpf(gwf, save_flows=True, icelltype=0, k=10.0, k33=10.0)
    flopy.mf6.ModflowGwfchd(
        gwf,
        stress_period_data=[[(0, 0, 0), 0.0], [(0, 0, ncol - 1), 0.0]],
        pname="CHD-1",
    )
    flopy.mf6.ModflowGwfrcha(
        gwf,
        print_flows=True,
        recharge=0.01,
        pname="RCH-1",
        filename=f"{gwfname}.rch",
    )
    flopy.mf6.ModflowGwfoc(
        gwf,
        budget_filerecord=f"{gwfname}.cbc",
        head_filerecord=f"{gwfname}.hds",
        saverecord=[("HEAD", "ALL"), ("BUDGET", "ALL")],
    )

    gwtname = "gwt_" + name
    gwt = flopy.mf6.MFModel(
        sim, model_type="gwt6", modelname=gwtname, model_nam_file=f"{gwtname}.nam"
    )
    ims = flopy.mf6.ModflowIms(
        sim,
        print_option="NONE",
        linear_acceleration="BICGSTAB",
        filename=f"{gwtname}.ims",
    )
    sim.register_ims_package(ims, [gwt.name])
    flopy.mf6.ModflowGwtdis(
        gwt,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=idomain,
    )
    flopy.mf6.ModflowGwtic(gwt, strt=0.0)
    flopy.mf6.ModflowGwtadv(gwt, scheme="UPSTREAM")
    flopy.mf6.ModflowGwtmst(gwt, porosity=0.30)

    spc_filename = f"{gwtname}.rch1.spc"
    flopy.mf6.ModflowGwtssm(
        gwt,
        print_flows=True,
        sources=[()],
        fileinput=[("RCH-1", spc_filename)],
    )
    flopy.mf6.ModflowGwtoc(
        gwt,
        budget_filerecord=f"{gwtname}.cbc",
        concentration_filerecord=f"{gwtname}.ucn",
        saverecord=[("CONCENTRATION", "ALL"), ("BUDGET", "ALL")],
    )
    flopy.mf6.ModflowGwfgwt(
        sim, exgtype="GWF6-GWT6", exgmnamea=gwfname, exgmnameb=gwtname
    )

    # .tas files are one-series-per-file (unlike .ts), so conc_tas_a and
    # conc_tas_b each need their own file and their own TAS6 FILEIN line
    tas_filename_a = f"{gwtname}.rch1.spc.a.tas"
    tas_text_a = (
        "BEGIN ATTRIBUTES\n"
        "  NAME conc_tas_a\n"
        "  METHOD STEPWISE\n"
        "END ATTRIBUTES\n\n"
        f"BEGIN TIME {0.0}\n  CONSTANT {TS_A_VALS[0.0]}\nEND TIME\n\n"
        f"BEGIN TIME {1.0}\n  CONSTANT {TS_A_VALS[1.0]}\nEND TIME\n\n"
        f"BEGIN TIME {2.0}\n  CONSTANT {TS_A_VALS[2.0]}\nEND TIME\n\n"
        f"BEGIN TIME {3.0}\n  CONSTANT {TS_A_VALS[3.0]}\nEND TIME\n"
    )
    (ws / tas_filename_a).write_text(tas_text_a)

    tas_filename_b = f"{gwtname}.rch1.spc.b.tas"
    tas_text_b = (
        "BEGIN ATTRIBUTES\n"
        "  NAME conc_tas_b\n"
        "  METHOD STEPWISE\n"
        "END ATTRIBUTES\n\n"
        f"BEGIN TIME {0.0}\n  CONSTANT {TS_B_VALS[0.0]}\nEND TIME\n\n"
        f"BEGIN TIME {1.0}\n  CONSTANT {TS_B_VALS[1.0]}\nEND TIME\n\n"
        f"BEGIN TIME {2.0}\n  CONSTANT {TS_B_VALS[2.0]}\nEND TIME\n\n"
        f"BEGIN TIME {3.0}\n  CONSTANT {TS_B_VALS[3.0]}\nEND TIME\n"
    )
    (ws / tas_filename_b).write_text(tas_text_b)

    spc_text = (
        "BEGIN options\n"
        "  READASARRAYS\n"
        "  PRINT_INPUT\n"
        f"  TAS6 FILEIN {tas_filename_a}\n"
        f"  TAS6 FILEIN {tas_filename_b}\n"
        "END options\n\n"
        "BEGIN PERIOD 1\n"
        "  CONCENTRATION  TIMEARRAYSERIES conc_tas_a\n"
        "END PERIOD\n\n"
        "# period 2: switch to a different TAS with distinctly different\n"
        "# values -- a leftover link from conc_tas_a must not keep firing\n"
        "BEGIN PERIOD 2\n"
        "  CONCENTRATION  TIMEARRAYSERIES conc_tas_b\n"
        "END PERIOD\n\n"
        "# period 3: switch back to the original TAS\n"
        "BEGIN PERIOD 3\n"
        "  CONCENTRATION  TIMEARRAYSERIES conc_tas_a\n"
        "END PERIOD\n"
    )
    (ws / spc_filename).write_text(spc_text)

    return sim


def check_output(idx, test):
    name = test.name
    gwtname = "gwt_" + name
    lst_fname = test.workspace / f"{gwtname}.lst"
    lines = lst_fname.read_text().splitlines()

    # parse "INPUT VALUES FOR CONCENTRATION PACKAGE" blocks; each
    # occurrence is one apply_input_values() call. Capture cell 1's
    # (array position 1) value from each block, in order.
    cell1_vals = []
    i = 0
    while i < len(lines):
        if "INPUT VALUES FOR CONCENTRATION PACKAGE" in lines[i]:
            j = i + 2
            while j < len(lines):
                parts = lines[j].split()
                if len(parts) != 2:
                    break
                try:
                    no = int(parts[0])
                    val = float(parts[1])
                except ValueError:
                    break
                if no == 1:
                    cell1_vals.append(val)
                j += 1
            i = j
        else:
            i += 1

    print(f"SPCA switch-test cell 1 concentration values, in order: {cell1_vals}")
    assert len(cell1_vals) == 15, (
        f"Expected 15 applied-value blocks (5/period, 3 periods), "
        f"got {len(cell1_vals)}: {cell1_vals}"
    )

    period1 = cell1_vals[:5]
    period2 = cell1_vals[5:10]
    period3 = cell1_vals[10:]

    assert np.allclose(period1, EXPECTED_P1), (
        f"Period 1 (conc_tas_a) expected {EXPECTED_P1}, got {period1}"
    )
    assert np.allclose(period2, EXPECTED_P2), (
        f"Period 2 (switched to conc_tas_b) expected {EXPECTED_P2} -- if "
        f"this instead shows a conc_tas_a value ({TS_A_VALS[1.0]}), a "
        f"stale link from period 1 is still driving the array, "
        f"got {period2}"
    )
    assert np.allclose(period3, EXPECTED_P3), (
        f"Period 3 (switched back to conc_tas_a) expected {EXPECTED_P3} -- "
        f"if this instead shows a conc_tas_b value ({TS_B_VALS[2.0]}), a "
        f"stale link from period 2 is still driving the array, "
        f"got {period3}"
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
