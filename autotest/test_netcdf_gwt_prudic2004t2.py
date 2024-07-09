"""
NetCDF export test version of test_gwt_prudic2004t2.
"""

import os
import sys
import subprocess

import flopy
import numpy as np
import pytest

from framework import TestFramework
from test_gwt_prudic2004t2 import cases

try:
    import xarray as xa
    import xugrid as xu
except ImportError:
    pytest.skip("xuarray and xugrid not found", allow_module_level=True)


def build_models(idx, test, export, gridded_input):
    from test_gwt_prudic2004t2 import build_models as build

    sim, dummy = build(idx, test)
    sim.tdis.start_date_time = "2041-01-01T00:00:00-05:00"
    gwt = sim.gwt[0]
    gwt.name_file.export_netcdf = export
    gwt.dis.export_array_netcdf = True
    gwt.ic.export_array_netcdf = True
    gwt.dsp.export_array_netcdf = True

    name = cases[idx]
    gwtname = "gwt_" + name

    # netcdf config
    ncf = flopy.mf6.ModflowUtlncf(
        gwt.dis,
        filename=f"{gwtname}.dis.ncf",
    )

    return sim, dummy


def check_output(idx, test, export, gridded_input):
    from test_gwt_prudic2004t2 import check_output as check

    name = test.name
    gwtname = "gwt_" + name

    if gridded_input == "netcdf":
        # re-run the simulation with model netcdf input
        input_fname = f"{gwtname}.nc"
        nc_fname = f"{gwtname}.{export}.nc"
        subprocess.run(
            ["mv", test.workspace / input_fname, test.workspace / nc_fname]
        )

        with open(test.workspace / f"{gwtname}.nam", "w") as f:
            f.write("BEGIN options\n")
            f.write(f"  EXPORT_NETCDF {export}\n")
            f.write(f"  NETCDF  FILEIN {gwtname}.{export}.nc\n")
            f.write("END options\n\n")
            f.write("BEGIN packages\n")
            f.write(f"  DIS6  {gwtname}.dis  dis\n")
            f.write(f"  IC6  {gwtname}.ic  ic\n")
            f.write(f"  MST6  {gwtname}.mst  mst\n")
            f.write(f"  ADV6  {gwtname}.adv  adv\n")
            f.write(f"  DSP6  {gwtname}.dsp  dsp\n")
            f.write(f"  SSM6  {gwtname}.ssm  ssm\n")
            f.write(f"  CNC6  {gwtname}.cnc  cnc-1\n")
            f.write(f"  LKT6  {gwtname}.lkt  lak-1\n")
            f.write(f"  SFT6  {gwtname}.sft  sfr-1\n")
            f.write(f"  MVT6  {gwtname}.mvt  mvt\n")
            f.write(f"  OC6  {gwtname}.oc  oc\n")
            f.write("END packages\n")

        with open(test.workspace / f"{gwtname}.dis", "w") as f:
            f.write("BEGIN options\n")
            f.write(f"  EXPORT_ARRAY_NETCDF\n")
            f.write(f"  NCF6  FILEIN  {gwtname}.dis.ncf\n")
            f.write("END options\n\n")
            f.write("BEGIN dimensions\n")
            f.write(f"  NLAY  8\n")
            f.write(f"  NROW  36\n")
            f.write(f"  NCOL  23\n")
            f.write("END dimensions\n\n")
            f.write("BEGIN griddata\n")
            f.write(f"  delr NETCDF\n")
            f.write(f"  delc NETCDF\n")
            f.write(f"  top NETCDF\n")
            f.write(f"  botm NETCDF\n")
            f.write(f"  idomain NETCDF\n")
            f.write("END griddata\n\n")

        with open(test.workspace / f"{gwtname}.ic", "w") as f:
            f.write("BEGIN options\n")
            f.write(f"  EXPORT_ARRAY_NETCDF\n")
            f.write("END options\n\n")
            f.write("BEGIN griddata\n")
            f.write(f"  strt NETCDF\n")
            f.write("END griddata\n")

        with open(test.workspace / f"{gwtname}.dsp", "w") as f:
            f.write("BEGIN options\n")
            f.write(f"  EXPORT_ARRAY_NETCDF\n")
            f.write("END options\n\n")
            f.write("BEGIN griddata\n")
            f.write(f"  alh  NETCDF\n")
            f.write(f"  ath1  NETCDF\n")
            f.write(f"  atv  NETCDF\n")
            f.write("END griddata\n")

        success, buff = flopy.run_model(
            test.targets["mf6"],
            test.workspace / "mfsim.nam",
            model_ws=test.workspace,
            report=True,
        )

        assert success
        test.success = success

    check(idx, test)

    # netcdf
    ws = test.workspace
    nc_fpth = os.path.join(ws, f"{gwtname}.nc")
    if export == "ugrid":
        ds = xu.open_dataset(nc_fpth)
        xds = ds.ugrid.to_dataset()
    elif export == "structured":
        xds = xa.open_dataset(nc_fpth)

    cobj = flopy.utils.HeadFile(
        os.path.join(ws, f"{gwtname}.ucn"),
        precision="double",
        text="CONCENTRATION",
    )

    # Compare NetCDF head arrays with binary headfile concentrations
    gwt = test.sims[0].gwt[0]
    dis = getattr(gwt, "dis")
    tdis = getattr(test.sims[0], "tdis")
    nper = getattr(tdis, "nper").data
    nlay = getattr(dis, "nlay").data
    pd = getattr(tdis, "perioddata").array
    timestep = 0
    for i in range(nper):
        for j in range(pd[i][1]):
            rec = cobj.get_data(kstpkper=(j, i))
            if export == "ugrid":
                for l in range(nlay):
                    assert np.allclose(
                        np.array(rec[l]).flatten(),
                        xds[f"concentration_l{l+1}"][timestep, :]
                        .fillna(1.00000000e30)
                        .data,
                    ), f"NetCDF-concentration comparison failure in timestep {timestep+1}"
                timestep += 1
            elif export == "structured":
                assert np.allclose(
                    # np.array(rec).flatten(),
                    np.array(rec),
                    xds[f"concentration"][timestep, :]
                    .fillna(1.00000000e30)
                    .data,
                ), f"NetCDF-concentration comparison failure in timestep {timestep+1}"
                timestep += 1

    vlist = [
        "dis_delr",
        "dis_delc",
        "dis_top",
        "dis_botm_l",
        "dis_idomain_l",
        "ic_strt_l",
        "dsp_alh_l",
        "dsp_ath1_l",
        "dsp_atv_l",
    ]

    # Compare NetCDF package input arrays with FloPy arrays
    gwt = test.sims[0].gwt[0]
    for i, var in enumerate(vlist):
        tokens = var.split("_", 1)
        package_name = tokens[0]
        array_name = tokens[1].split("_")[0]
        package = getattr(gwt, package_name)
        b = getattr(package, array_name).array
        if export == "ugrid":
            if var.endswith("_l"):
                for l in range(nlay):
                    assert np.allclose(
                        np.array(b[l]).flatten(), xds[f"{var}{l+1}"].data
                    ), f"NetCDF input array comparison failure, variable={var}{l+1}"
            else:
                assert np.allclose(
                    np.array(b).flatten(), xds[var].data
                ), f"NetCDF input array comparison failure, variable={var}"
        elif export == "structured":
            var = var.replace("_l", "")
            assert np.allclose(
                np.array(b), xds[var].data
            ), f"NetCDF input array comparison failure, variable={var}"


@pytest.mark.slow
@pytest.mark.netcdf
@pytest.mark.parametrize("idx, name", enumerate(cases))
@pytest.mark.parametrize("export", ["ugrid", "structured"])
@pytest.mark.parametrize("gridded_input", ["ascii", "netcdf"])
def test_mf6model(idx, name, function_tmpdir, targets, export, gridded_input):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t, export, gridded_input),
        check=lambda t: check_output(idx, t, export, gridded_input),
    )
    test.run()
