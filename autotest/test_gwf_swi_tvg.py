"""
Test the Time-Varying Grid (TVG) Utility as used by the SWI Package.

The saltwater head can be supplied two ways: as the scalar SALTWATER_HEAD
option, or as a full-grid array through a TVG6 file. For a grid whose cells
all carry the same value the two must give identical results, which checks
that TVG reaches every node rather than only the first layer. The grid is
three layers deliberately: a first-layer-only fill fails this test.

A second case gives a different value in each layer, through the LAYERED
keyword, and confirms the interface responds layer by layer.

The other cases compare TVG input with the same saltwater heads given as
TVG arrays in each period (or each one-step period), so results must be
identical:
- swi-tvg-tas: a stepwise time-array series in period 1 (0.05, then 0.10
  from time 1) over two periods.
- swi-tvg-periods: period 2 changes some cells and leaves the others as
  DNODATA; period 3 has no block. Values persist per cell.
- swi-tvg-linear: a linear series (0.05 at time 0 to 0.15 at time 2) over
  one period of two time steps; the reference gives each step's average in
  two one-step periods.
- swi-tvg-multiaux: two auxiliary variables, SALTWATER_HEAD second; one
  given without the other in later periods. Each has its own write stamps.
- swi-tvg-tasnodata: a series whose later arrays are DNODATA at some cells;
  those cells keep their earlier values.
- swi-tvg-relink: a series in period 1 replaced by values in period 2 (the
  link ends, so later series values have no effect), then an empty block in
  period 3, which changes nothing.
- swi-tvg-inactive: a TVG value at an inactive cell is ignored.
"""

import pathlib as pl

import flopy
import numpy as np
import pytest
from framework import TestFramework

cases = [
    "swi-tvg-uniform",
    "swi-tvg-layered",
    "swi-tvg-tas",
    "swi-tvg-periods",
    "swi-tvg-linear",
    "swi-tvg-multiaux",
    "swi-tvg-tasnodata",
    "swi-tvg-relink",
    "swi-tvg-inactive",
]
series_cases = (2, 4, 5, 6, 7)  # cases that write their own TVG files
# saltwater head in the first time step of the equivalence cases, for the
# scalar SALTWATER_HEAD anchor run
first_hsalt = {2: 0.05, 3: 0.05, 4: 0.075, 5: 0.05, 6: 0.05, 7: 0.05, 8: 0.05}
DNODATA = 3.0e30
inactive = (0, 0, 3)

nlay, nrow, ncol = 3, 1, 6
delr = delc = 100.0
top = 0.0
botm = [-10.0, -20.0, -30.0]
k = 1.0
recharge = 1.0e-4
hsalt = 0.05
hsalt_layered = [0.05, 0.10, 0.15]
hsalt_periods = [0.05, 0.10]


def _base(ws, name, nper=1, perioddata=None, idomain=None):
    perioddata = perioddata or [(1.0, 1, 1.0)] * nper
    sim = flopy.mf6.MFSimulation(sim_name=name, sim_ws=ws, exe_name="mf6")
    flopy.mf6.ModflowTdis(sim, nper=len(perioddata), perioddata=perioddata)
    flopy.mf6.ModflowIms(
        sim,
        print_option="summary",
        linear_acceleration="bicgstab",
        outer_maximum=200,
    )
    gwf = flopy.mf6.ModflowGwf(
        sim, modelname="mymodel", save_flows=True, newtonoptions="newton"
    )
    flopy.mf6.ModflowGwfdis(
        gwf,
        nlay=nlay,
        nrow=nrow,
        ncol=ncol,
        delr=delr,
        delc=delc,
        top=top,
        botm=botm,
        idomain=1 if idomain is None else idomain,
    )
    flopy.mf6.ModflowGwfic(gwf, strt=0.0)
    flopy.mf6.ModflowGwfnpf(gwf, icelltype=0, k=k)
    flopy.mf6.ModflowGwfchd(gwf, stress_period_data=[[(0, 0, 0), 0.0]])
    flopy.mf6.ModflowGwfrcha(gwf, recharge=recharge)
    return sim, gwf


def _full(v):
    return np.full((nlay, nrow, ncol), v)


def _tvg_sim(ws, name, aux, perioddata, idomain=None):
    """A model with SWI saltwater head from TVG arrays {period: value array}."""
    sim, gwf = _base(str(ws), name, perioddata=perioddata, idomain=idomain)
    swi = flopy.mf6.ModflowGwfswi(
        gwf, zeta_filerecord="mymodel.zta", tvg6_filerecord="mymodel.swi.tvg"
    )
    swi.tvg.initialize(
        auxiliary=["saltwater_head"], aux={p: [a] for p, a in aux.items()}
    )
    flopy.mf6.ModflowGwfoc(
        gwf, head_filerecord="mymodel.hds", saverecord=[("HEAD", "ALL")]
    )
    return sim


def _scalar_sim(ws, value, perioddata, idomain=None):
    """The scalar SALTWATER_HEAD option, an anchor independent of TVG."""
    sim, gwf = _base(str(ws), "scalar", perioddata=perioddata, idomain=idomain)
    flopy.mf6.ModflowGwfswi(gwf, zeta_filerecord="mymodel.zta", saltwater_head=value)
    flopy.mf6.ModflowGwfoc(
        gwf, head_filerecord="mymodel.hds", saverecord=[("HEAD", "ALL")]
    )
    sim.write_simulation(silent=True)


def _array(a):
    """An INTERNAL array record."""
    rows = "\n".join(
        "    " + " ".join(f"{v:.8e}" for v in r) for r in a.reshape(-1, ncol)
    )
    return f"INTERNAL\n{rows}"


def _write_tvg(ws, auxnames, blocks, series=None):
    """Write the TVG file: blocks {period: [(auxname, array or series name)]},
    an empty list for an empty block; series (method, [(time, array)]) for
    series "hsts"."""
    opts = f"  READARRAYGRID\n  AUXILIARY {' '.join(auxnames)}\n"
    if series is not None:
        opts += "  TAS6 FILEIN mymodel.swi.tvg.tas\n"
    text = f"BEGIN options\n{opts}END options\n\n"
    for per, items in blocks.items():
        lines = "".join(
            f"  {n} TIMEARRAYSERIES {v}\n"
            if isinstance(v, str)
            else f"  {n}\n    {_array(v)}\n"
            for n, v in items
        )
        text += f"BEGIN period {per}\n{lines}END period {per}\n\n"
    (ws / "mymodel.swi.tvg").write_text(text)
    if series is not None:
        method, records = series
        (ws / "mymodel.swi.tvg.tas").write_text(
            f"BEGIN attributes\n  NAME hsts\n  METHOD {method}\nEND attributes\n\n"
            + "".join(
                f"BEGIN time {t}\n  {_array(a)}\nEND time\n\n" for t, a in records
            )
        )


def _write_tvg_series(ws, method, records):
    """Replace the TVG file with one giving the saltwater head in period 1
    by a time-array series of CONSTANT arrays."""
    (ws / "mymodel.swi.tvg").write_text(
        "BEGIN options\n  READARRAYGRID\n  AUXILIARY saltwater_head\n"
        "  TAS6 FILEIN mymodel.swi.tvg.tas\nEND options\n\n"
        "BEGIN period 1\n  saltwater_head TIMEARRAYSERIES hsts\nEND period 1\n"
    )
    (ws / "mymodel.swi.tvg.tas").write_text(
        f"BEGIN attributes\n  NAME hsts\n  METHOD {method}\nEND attributes\n\n"
        + "".join(f"BEGIN time {t}\n  CONSTANT {v}\nEND time\n\n" for t, v in records)
    )


def build_models(idx, test):
    sim, other = _build_models(idx, test)
    if idx in first_hsalt:
        ws = pl.Path(test.workspace)
        tdis = sim.get_package("tdis")
        pd = [tuple(r) for r in tdis.perioddata.get_data()]
        dis = sim.get_model("mymodel").get_package("dis")
        _scalar_sim(ws / "scalar", first_hsalt[idx], pd, dis.idomain.get_data())
    return sim, other


def _build_models(idx, test):
    ws = pl.Path(test.workspace)

    if idx == 2:
        _tvg_sim(
            ws / "ref",
            "ref",
            {p: _full(v) for p, v in enumerate(hsalt_periods)},
            [(1.0, 1, 1.0)] * 2,
        ).write_simulation(silent=True)
        sim = _tvg_sim(ws, "mymodel", {0: _full(hsalt_periods[0])}, [(1.0, 1, 1.0)] * 2)
        sim.write_simulation(silent=True)
        records = [(0.0, 0.05), (1.0, 0.10), (2.0, 0.10)]
        _write_tvg_series(ws, "stepwise", records)
        return sim, None

    if idx == 3:
        first = _full(0.05)
        change = np.full((nlay, nrow, ncol), 3.0e30)
        change[:, :, ncol // 2 :] = 0.10
        merged = np.where(change == 3.0e30, first, change)
        pd = [(1.0, 1, 1.0)] * 3
        _tvg_sim(
            ws / "ref", "ref", {0: first, 1: merged, 2: merged}, pd
        ).write_simulation(silent=True)
        return _tvg_sim(ws, "mymodel", {0: first, 1: change}, pd), None

    if idx == 4:
        pd = [(1.0, 1, 1.0)] * 2
        _tvg_sim(
            ws / "ref", "ref", {0: _full(0.075), 1: _full(0.125)}, pd
        ).write_simulation(silent=True)
        sim = _tvg_sim(ws, "mymodel", {0: _full(0.05)}, [(2.0, 2, 1.0)])
        sim.write_simulation(silent=True)
        _write_tvg_series(ws, "linear", [(0.0, 0.05), (2.0, 0.15)])
        return sim, None

    if idx in (5, 6, 7, 8):
        half = _full(DNODATA)
        half[:, :, ncol // 2 :] = 0.10
        merged = np.where(half == DNODATA, 0.05, half)
        idomain = None
        if idx == 5:
            ref = {0: _full(0.05), 1: _full(0.05), 2: _full(0.10)}
            blocks = {
                1: [("tide", _full(1.0)), ("saltwater_head", _full(0.05))],
                2: [("tide", _full(2.0))],
                3: [("saltwater_head", _full(0.10))],
            }
            args = (["tide", "saltwater_head"], blocks)
        elif idx == 6:
            ref = {0: _full(0.05), 1: merged}
            series = ("stepwise", [(0.0, _full(0.05)), (1.0, half), (2.0, half)])
            args = (["saltwater_head"], {1: [("saltwater_head", "hsts")]}, series)
        elif idx == 7:
            ref = {0: _full(0.05), 1: _full(0.08), 2: _full(0.08)}
            records = [(0.0, _full(0.05)), (1.0, _full(0.07)), (2.0, _full(0.09))]
            records.append((3.0, _full(0.09)))
            blocks = {
                1: [("saltwater_head", "hsts")],
                2: [("saltwater_head", _full(0.08))],
                3: [],
            }
            args = (["saltwater_head"], blocks, ("stepwise", records))
        else:
            idomain = np.ones((nlay, nrow, ncol), dtype=int)
            idomain[inactive] = 0
            given = _full(0.05)
            given[inactive] = 0.5
            refa = _full(0.05)
            refa[inactive] = DNODATA
            ref = {0: refa}
            sim = _tvg_sim(ws, "mymodel", {0: given}, None, idomain)
            _tvg_sim(ws / "ref", "ref", ref, None, idomain).write_simulation(
                silent=True
            )
            return sim, None
        pd = [(1.0, 1, 1.0)] * len(ref)
        _tvg_sim(ws / "ref", "ref", ref, pd).write_simulation(silent=True)
        sim = _tvg_sim(ws, "mymodel", {0: _full(0.05)}, pd)
        sim.write_simulation(silent=True)
        _write_tvg(ws, *args)
        return sim, None

    # reference: the same value supplied through the scalar option
    if idx == 0:
        ref, gwfr = _base(str(ws / "ref"), "ref")
        flopy.mf6.ModflowGwfswi(
            gwfr, zeta_filerecord="mymodel.zta", saltwater_head=hsalt
        )
        flopy.mf6.ModflowGwfoc(
            gwfr, head_filerecord="mymodel.hds", saverecord=[("HEAD", "ALL")]
        )
        ref.write_simulation(silent=True)

    sim, gwf = _base(str(ws), "mymodel")
    swi = flopy.mf6.ModflowGwfswi(
        gwf, zeta_filerecord="mymodel.zta", tvg6_filerecord="mymodel.swi.tvg"
    )
    if idx == 0:
        arr = np.full((nlay, nrow, ncol), hsalt)
    else:
        arr = np.empty((nlay, nrow, ncol))
        for kk in range(nlay):
            arr[kk] = hsalt_layered[kk]
    swi.tvg.initialize(auxiliary=["saltwater_head"], aux={0: [arr]})
    flopy.mf6.ModflowGwfoc(
        gwf, head_filerecord="mymodel.hds", saverecord=[("HEAD", "ALL")]
    )
    return sim, None


def _zeta(ws):
    return (
        flopy.utils.HeadFile(pl.Path(ws) / "mymodel.zta", text="zeta")
        .get_data()
        .flatten()
    )


def check_output(idx, test):
    ws = pl.Path(test.workspace)
    gwf = test.sims[0].gwf[0]
    head = gwf.output.head().get_data().flatten()
    zeta = _zeta(ws)

    if idx >= 2:
        # TVG input must match the same values given as TVG arrays
        ref = flopy.mf6.MFSimulation.load(
            sim_ws=str(ws / "ref"), exe_name=test.targets["mf6"], verbosity_level=0
        )
        success, buff = ref.run_simulation(silent=True)
        assert success, "reference simulation failed"
        h = flopy.utils.HeadFile(ws / "mymodel.hds").get_alldata()
        href = flopy.utils.HeadFile(ws / "ref" / "mymodel.hds").get_alldata()
        z = flopy.utils.HeadFile(ws / "mymodel.zta", text="zeta").get_alldata()
        zref = flopy.utils.HeadFile(
            ws / "ref" / "mymodel.zta", text="zeta"
        ).get_alldata()
        assert h.shape == href.shape, f"time steps {h.shape} != reference {href.shape}"
        assert np.allclose(h, href, atol=1.0e-12), (
            f"{cases[idx]}: head differs from the reference: "
            f"max |diff| = {np.abs(h - href).max():.3e}"
        )
        assert np.allclose(z, zref, atol=1.0e-12), (
            f"{cases[idx]}: zeta differs from the reference: "
            f"max |diff| = {np.abs(z - zref).max():.3e}"
        )
        # anchor: the first time step matches the scalar option, and the
        # later changes are applied (some later step differs from it)
        scalar = flopy.mf6.MFSimulation.load(
            sim_ws=str(ws / "scalar"), exe_name=test.targets["mf6"], verbosity_level=0
        )
        success, buff = scalar.run_simulation(silent=True)
        assert success, "scalar anchor simulation failed"
        zs = flopy.utils.HeadFile(
            ws / "scalar" / "mymodel.zta", text="zeta"
        ).get_alldata()
        assert np.allclose(z[0], zs[0], atol=1.0e-12), (
            f"{cases[idx]}: first time step differs from the scalar option, so "
            "the TVG values were not applied: "
            f"max |diff| = {np.abs(z[0] - zs[0]).max():.3e}"
        )
        if len(z) > 1:
            assert np.abs(z[1:] - zs[1:]).max() > 1.0e-6, (
                f"{cases[idx]}: later saltwater head changes have no effect"
            )
        return

    if idx == 0:
        # run the scalar-option reference and require an identical answer
        ref = flopy.mf6.MFSimulation.load(
            sim_ws=str(ws / "ref"), exe_name=test.targets["mf6"], verbosity_level=0
        )
        success, buff = ref.run_simulation(silent=True)
        assert success, "reference simulation failed"
        href = ref.get_model("mymodel").output.head().get_data().flatten()
        zref = _zeta(ws / "ref")
        assert np.allclose(head, href, atol=1.0e-12), (
            "TVG head differs from the scalar SALTWATER_HEAD option: "
            f"max |diff| = {np.abs(head - href).max():.3e}"
        )
        assert np.allclose(zeta, zref, atol=1.0e-12), (
            "TVG zeta differs from the scalar SALTWATER_HEAD option, which "
            "means TVG did not fill every node: "
            f"max |diff| = {np.abs(zeta - zref).max():.3e}"
        )
        print(f"TVG matches the scalar option to {np.abs(zeta - zref).max():.1e}")
    else:
        # a different value per layer must move the interface per layer
        means = [zeta[kk * ncol : (kk + 1) * ncol].mean() for kk in range(nlay)]
        assert len(set(np.round(means, 6))) == nlay, (
            f"layered saltwater head did not give a distinct interface per "
            f"layer: layer means {means}"
        )
        print(f"layered zeta means: {[round(float(m), 4) for m in means]}")


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        targets=targets,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        # the series case writes its own files after flopy
        overwrite=idx not in series_cases,
    )
    test.run()
