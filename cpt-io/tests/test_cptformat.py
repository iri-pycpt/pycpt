from cptio import *
import pandas as pd
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
def test_spatial_loadings_station():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    ds = open_cptdataset(
        Path(__file__).absolute().parents[0]
        / "data/predictand_cca_spatial_loadings_station.txt"
    )
    print(ds)
    # don't need to round-trip spatial loadings
    assert set(ds.data_vars) == {"prcp_Y_CCA_loadings"}
    da = ds["prcp_Y_CCA_loadings"]
    assert set(da.dims) == {'station', 'Mode'}
    assert set(da.coords) == {'station', 'Mode', 'Y', 'X'}
    assert da['station'][0] == '300042'
    assert len(da['Mode']) == 2
    assert da['Y'][0] == 42.0


@pytest.mark.fileio
def test_eof_timeseries():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    ds = open_cptdataset(
        Path(__file__).absolute().parents[0] / "data/predictand_eof_timeseries.txt"
    )
    print(ds)
    assert list(ds.data_vars) == ['Y_scores']
    da = ds['Y_scores']
    assert set(da.dims) == {'T', 'index'}
    assert set(da.coords) == {'T', 'index', 'Ti', 'Tf'}
    assert da['T'][0] == pd.Timestamp('1980-07-31T12:00:00')
    assert da['Ti'][0] == pd.Timestamp('1980-06-01')
    # This is actually wrong (should be 10-01), but it's been wrong forever.
    assert da['Tf'][0] == pd.Timestamp('1980-09-30')
    assert da.isnull().sum().item() == 0


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
    from cptio.fileio.cpt import guess_cptv10_coords
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


@pytest.mark.fileio
def test_skill_station():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    ds = open_cptdataset(Path(__file__).absolute().parents[0] / "data/pearson-station.txt")
    # skill data doesn't have to round-trip, we only need to be able to read it.
    print(ds)
    assert set(ds.data_vars) == {"prcp_Pearson's_correlation"}
    da = ds["prcp_Pearson's_correlation"]
    assert set(da.dims) == {'station', 'Mode'}
    assert set(da.coords) == {'station', 'Mode', 'Y', 'X'}
    

@pytest.mark.fileio
def test_prob_station():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(Path(__file__).absolute().parents[0] / "data/forecast_probabilities_station.txt")
    print(x)
    testfile = to_cptv10(
        getattr(x, [i for i in x.data_vars][0]),
        opfile="test.tsv",
        assertmissing=False,
        assert_units=False,
    )
    result = open_cptdataset(testfile)
    print(result)
    assert result.equals(x)

@pytest.mark.fileio
def test_canonical_correlation_station():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(Path(__file__).absolute().parents[0] / "data/cca_canonical_correlation_station.txt")
    print(x)
    testfile = to_cptv10(
        getattr(x, [i for i in x.data_vars][0]),
        opfile="test.tsv",
        assertmissing=False,
        assert_units=False,
    )
    result = open_cptdataset(testfile)
    print(result)
    assert result.equals(x)

@pytest.mark.fileio
def test_forecast_values_station():
    if Path("test.tsv").is_file():
        Path("test.tsv").unlink()
    x = open_cptdataset(Path(__file__).absolute().parents[0] / "data/forecast_values_station.txt")
    print(x)
    testfile = to_cptv10(
        getattr(x, [i for i in x.data_vars][0]),
        opfile="test.tsv",
        assertmissing=False,
        assert_units=False,
    )
    result = open_cptdataset(testfile)
    print(result)
    assert result.equals(x)
