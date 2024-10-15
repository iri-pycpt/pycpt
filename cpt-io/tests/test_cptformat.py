import cptio
from cptio import open_cptdataset, open_cptdataarray, to_cptv10
import datetime as dt
import numpy as np
from pathlib import Path
import pytest
import xarray as xr

datadir = Path(__file__).absolute().parents[0] / 'data'

out_name = "test.tsv"

def prep():
    if Path(out_name).is_file():
        Path(out_name).unlink()

def roundtrip(file_name, data_var=None, assertmissing=True, assert_units=True, exact=True):
    prep()
    ds = open_cptdataset(datadir / file_name)
    das = list(ds.data_vars.values())
    assert len(das) == 1
    da = das[0]
    testfile = to_cptv10(da, opfile=out_name, assertmissing=assertmissing, assert_units=assert_units)
    new_ds = open_cptdataset(testfile)
    new_da = next(iter(new_ds.data_vars.values()))
    if exact:
        assert new_da.equals(da)
    else:
        xr.testing.assert_allclose(new_da, da)

def test_gcm_input():
    roundtrip("SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv")

def test_cpc_input():
    roundtrip("SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv")

def test_missing_data():
    roundtrip("lesotho_ond.tsv")

def test_probabilistic_data():
    # exact=False because the original file from CPT has six digits after the decimal point,
    # while pycpt writes six significant digts, which in this case is four digits after the
    # decimal point. TODO pycpt should use the same precision rules as CPT, even if they
    # differ from Ingrid's precision rules. Saving that for v3.
    roundtrip("prob_rfcsts.tsv", exact=False)

def test_skill():
    roundtrip("pearson.txt", assertmissing=False, assert_units=False)

def test_spatial_loadings():
    # See earlier comment on exact=False
    roundtrip("predictand_cca_spatial_loadings.txt", assertmissing=False, assert_units=False, exact=False)

def test_spatial_loadings_station():
    ds = open_cptdataset(datadir / "predictand_cca_spatial_loadings_station.txt")
    print(ds)
    # don't need to round-trip spatial loadings
    assert set(ds.data_vars) == {"prcp_Y_CCA_loadings"}
    da = ds["prcp_Y_CCA_loadings"]
    assert set(da.dims) == {'station', 'Mode'}
    assert set(da.coords) == {'station', 'Mode', 'Y', 'X'}
    assert da['station'][0] == '300042'
    assert len(da['Mode']) == 2
    assert da['Y'][0] == 42.0

def test_eof_timeseries():
    roundtrip("predictand_eof_timeseries.txt", assertmissing=False, assert_units=False)

def test_canonical_correlation():
    ds = open_cptdataset(datadir / "cca_canonical_correlation.txt")
    print(ds)
    da = ds['correlation']
    assert da.dims == ("Mode", "index")
    np.testing.assert_array_equal(da['Mode'], [1, 2, 3, 4, 5])
    np.testing.assert_array_equal(da['index'], ['correlation'])
    assert da.dtype == np.float64

def test_station_data():
    roundtrip("GHCN_Jun_cptv10.tsv")

def test_skill_station():
    ds = open_cptdataset(datadir / "pearson-station.txt")
    # skill data doesn't have to round-trip, we only need to be able to read it.
    print(ds)
    assert set(ds.data_vars) == {"prcp_Pearson's_correlation"}
    da = ds["prcp_Pearson's_correlation"]
    assert set(da.dims) == {'station', 'Mode'}
    assert set(da.coords) == {'station', 'Mode', 'Y', 'X'}

def test_prob_station():
    roundtrip("forecast_probabilities_station.txt")

def test_canonical_correlation_station():
    roundtrip("cca_canonical_correlation_station.txt", assertmissing=False, assert_units=False)

def test_forecast_values_station():
    roundtrip("forecast_values_station.txt")

def test_onemonth_season():
    prep()
    def check(da):
        assert da['Ti'][0].values == np.datetime64('1960-06-01')
        assert da['T'][0].values == np.datetime64('1960-06-15T12:00')
        assert da['Tf'][0].values == np.datetime64('1960-06-30')

    da = open_cptdataarray(datadir / "GHCN_Jun_cptv10.tsv")
    check(da)
    testfile = to_cptv10(da, opfile=out_name)
    new_da = open_cptdataarray(testfile)
    check(new_da)

def test_read_cpt_date_midnight():
    d = cptio.fileio.cpt.read_cpt_date('2015-03-26T00:00')
    assert d == dt.datetime(2015, 3, 26)

def test_read_cpt_date_4am():
    with pytest.raises(Exception):
        cptio.fileio.cpt.read_cpt_date('2015-03-26T04:00')

def test_read_cpt_date_day():
    with pytest.raises(Exception):
        cptio.fileio.cpt.read_cpt_date('2015-03-26')

def test_read_cpt_date_month():
    with pytest.raises(Exception):
        cptio.fileio.cpt.read_cpt_date('2015-03')

def test_read_cpt_date_threemonth():
    with pytest.raises(Exception):
        cptio.fileio.cpt.read_cpt_date('2015-03/05')

def test_read_cpt_date_range_midnight():
    with pytest.raises(Exception):
        cptio.fileio.cpt.read_cpt_date_range('2015-03-26T00:00')

def test_read_cpt_date_range_day():
    with pytest.raises(Exception):
        cptio.fileio.cpt.read_cpt_date_range('2015-03-26')

def test_read_cpt_date_range_month():
    d = cptio.fileio.cpt.read_cpt_date_range('2015-03')
    assert d == [
        dt.datetime(2015, 3, 1),
        dt.datetime(2015, 3, 16),
        dt.datetime(2015, 3, 31)
    ]

def test_read_cpt_date_range_threemonth():
    d = cptio.fileio.cpt.read_cpt_date_range('2015-03/05')
    assert d == [
        dt.datetime(2015, 3, 1),
        dt.datetime(2015, 4, 15, 12),
        dt.datetime(2015, 5, 31)
    ]

def test_read_cpt_date_range_wrap():
    d = cptio.fileio.cpt.read_cpt_date_range('2015-12/2016-02')
    assert d == [
        dt.datetime(2015, 12, 1),
        dt.datetime(2016, 1, 15),
        dt.datetime(2016, 2, 29)
    ]
