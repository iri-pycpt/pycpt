import cptdl as dl
import cptio
import datetime as dt
import pathlib
import pycpt
import pytest
import tempfile
import xarray as xr

reference_dir = pathlib.Path(__file__).absolute().parent / 'data/downloads'

default_download_args = {
    'fdate':  dt.datetime(2020, 5, 1),
    'target_first_year': 2005,
    'target_final_year': 2007,
    'target': 'Jun-Aug',
    'predictor_extent': {
        'west': -76,
        'east':  -74,
        'south': 39,
        'north': 41,
      },
    'pressure': 200, # I don't know how this would be used in PyCPT
}
default_download_args = pycpt.notebook._preprocess_download_args(default_download_args)

overrides = {
    'CPS2': {
        # CPS2 has 2.5 degree resolution and some missing values. Need
        # to select a wider area to get enough points.
        'predictor_extent': {'west': -77, 'east': -74, 'south': 38, 'north': 41},
    },
}

@pytest.mark.parametrize('var', dl.hindcasts)
def test_hindcast_download(var):
    model, _ = var.split('.')
    args = default_download_args | overrides.get(model, {})
    with tempfile.NamedTemporaryFile() as f:
        ds = dl.download(
            dl.hindcasts[var],
            f.name,
            verbose=True,
            **args
        )
        print(ds)

        assert len(ds['T']) == 3
        # Some grids are on the integer degrees, so three points
        # get selected. Others are on the half degree, so two get
        # selected.
        assert len(ds['Y']) in (2, 3)
        assert len(ds['X']) in (2, 3)

        reference_file = reference_dir / f'{var}.tsv'
        if reference_file.exists():
            ds_ref = cptio.open_cptdataset(reference_file)
            xr.testing.assert_equal(ds, ds_ref)
        else:
            reference_file.hardlink_to(f.name)
