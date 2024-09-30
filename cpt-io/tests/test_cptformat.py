from cptio import open_cptdataset, to_cptv10
import numpy as np
from pathlib import Path
import pytest

datadir = Path(__file__).absolute().parents[0] / 'data'

def xarray_equals(x, y):
    return (x - y).sum() == 0

out_name = "test.tsv"

def prep():
    if Path(out_name).is_file():
        Path(out_name).unlink()

def roundtrip(file_name, assertmissing=True, assert_units=True, exact=True):
    prep()
    ds = open_cptdataset(datadir / file_name)
    das = list(ds.data_vars.values())
    assert len(das) == 1
    da = das[0]
    testfile = to_cptv10(da, opfile=out_name, assertmissing=assertmissing, assert_units=assert_units)
    assert xarray_equals(open_cptdataset(testfile), da)
 
def test_gcm_input():
    roundtrip("SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv")

def test_cpc_input():
    roundtrip("SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv")

def test_missing_data():
    roundtrip("lesotho_ond.tsv")

def test_probabilistic_data():
    roundtrip("prob_rfcsts.tsv")

def test_skill():
    roundtrip("pearson.txt", assertmissing=False, assert_units=False)

def test_spatial_loadings():
    roundtrip("predictand_cca_spatial_loadings.txt", assertmissing=False, assert_units=False)

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
