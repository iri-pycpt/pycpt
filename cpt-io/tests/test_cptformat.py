from cptio import *
from pathlib import Path
import pytest


def xarray_equals(x, y):
    return (x - y).sum() == 0


@pytest.mark.fileio
def test_gcm_input():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(
        Path(__file__).absolute().parents[0]
        / "data/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv"
    ).prec
    testfile = to_cptv10(x, opfile="test.tsv")
    assert xarray_equals(open_cptdataset(testfile), x)


@pytest.mark.fileio
def test_cpc_input():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    y = open_cptdataset(
        Path(__file__).absolute().parents[0]
        / "data/SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv"
    ).prate
    testfile = to_cptv10(y, opfile="test.tsv")
    assert xarray_equals(open_cptdataset(testfile), y)


@pytest.mark.fileio
def test_missing_data():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(
        Path(__file__).absolute().parents[0] / "data/lesotho_ond.tsv"
    ).rfe
    testfile = to_cptv10(x, opfile="test.tsv")
    assert xarray_equals(open_cptdataset(testfile), x)


@pytest.mark.fileio
def test_probabilistic_data():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(Path(__file__).absolute().parents[0] / "data/prob_rfcsts.tsv")
    testfile = to_cptv10(getattr(x, [i for i in x.data_vars][0]), opfile="test.tsv")
    assert xarray_equals(open_cptdataset(testfile), x)


@pytest.mark.fileio
def test_skill():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(Path(__file__).absolute().parents[0] / "data/pearson.txt")
    testfile = to_cptv10(
        getattr(x, [i for i in x.data_vars][0]),
        opfile="test.tsv",
        assertmissing=False,
        assert_units=False,
    )
    assert xarray_equals(open_cptdataset(testfile), x)


@pytest.mark.fileio
def test_spatial_loadings():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(
        Path(__file__).absolute().parents[0]
        / "data/predictand_cca_spatial_loadings.txt"
    )
    testfile = to_cptv10(
        getattr(x, [i for i in x.data_vars][0]),
        opfile="test.tsv",
        assertmissing=False,
        assert_units=False,
    )
    assert xarray_equals(open_cptdataset(testfile), x)


@pytest.mark.fileio
def test_eof_timeseries():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(
        Path(__file__).absolute().parents[0] / "data/predictand_eof_timeseries.txt"
    )
    testfile = to_cptv10(
        getattr(x, [i for i in x.data_vars][0]),
        opfile="test.tsv",
        assertmissing=False,
        assert_units=False,
    )
    assert xarray_equals(open_cptdataset(testfile), x)


@pytest.mark.fileio
def test_canonical_correlation():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(
        Path(__file__).absolute().parents[0] / "data/cca_canonical_correlation.txt"
    )
    testfile = to_cptv10(getattr(x, [i for i in x.data_vars][0]), opfile="test.tsv")
    assert xarray_equals(open_cptdataset(testfile), x)


@pytest.mark.fileio
def test_station_data():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(
        Path(__file__).absolute().parents[0] / "data/GHCN_Jun_cptv10.tsv"
    )
    print(x)
    testfile = to_cptv10(getattr(x, [i for i in x.data_vars][0]), opfile="test.tsv")
    assert open_cptdataset(testfile).equals(x)


@pytest.mark.fileio
def test_station_data_known():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(
        Path(__file__).absolute().parents[0] / "data/GHCN_Jun_cptv10.tsv"
    )
    print(x)
    row, col, T, C = guess_cptv10_coords(x, row='T', col='station')
    assert row == 'T'
    assert col == 'station'
    assert T == None
    assert C == None