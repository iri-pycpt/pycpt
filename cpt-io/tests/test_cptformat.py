from cptio import open_cptdataset, to_cptv10
import numpy as np
from pathlib import Path
import pytest
import xarray as xr

datadir = Path(__file__).absolute().parents[0] / 'data'

def xarray_equals(x, y):
    return (x - y).sum() == 0

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
        assert xarray_equals(new_da, da)
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
