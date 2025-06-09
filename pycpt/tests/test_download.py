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
    'CanCM3': {
        'fdate': dt.datetime(2019, 5, 1),
    },
    'CanCM4': {
        'fdate': dt.datetime(2019, 5, 1),
    },
    'CanSIPSIC3': {
        'fdate': dt.datetime(2022, 5, 1),
    },
    'CanSIPSIC4': {
        'fdate': dt.datetime(2024, 8, 1),
    },
    'CPS2': {
        # CPS2 has 2.5 degree resolution and some missing values. Need
        # to select a wider area to get enough points.
        'predictor_extent': {'west': -77, 'east': -74, 'south': 38, 'north': 41},
        'fdate': dt.datetime(2021, 5, 1),
    },
    'GCFS2p1': {
        'fdate': dt.datetime(2021, 5, 1),
    },
    'GEM5NEMO': {
        'fdate': dt.datetime(2022, 5, 1),
    },
    'GLOSEA6': {
        'fdate': dt.datetime(2021, 5, 1),
    },
    'METEOFRANCE8': {
        'fdate': dt.datetime(2022, 5, 1),
    },
    'METEOFRANCE9': {
        'fdate': dt.datetime(2025, 4, 1),
    },
    'SEAS51': {
        'fdate': dt.datetime(2023, 5, 1),
    },
    'SEAS51b': {
        'fdate': dt.datetime(2023, 5, 1),
    },
    'SPEAR': {
        'fdate': dt.datetime(2021, 5, 1),
    },
    'SPEARb': {
        'fdate': dt.datetime(2021, 5, 1),
    },
    'SPSv3p5': {
        'fdate': dt.datetime(2021, 5, 1),
    },
}

@pytest.mark.skip
@pytest.mark.parametrize('var,url', dl.hindcasts.items())
def test_hindcast_download(var, url):
    do_one(var, url, 'hindcast', 3)


@pytest.mark.skip
@pytest.mark.parametrize('var,url', dl.forecasts.items())
def test_forecast_download(var, url):
    do_one(var, url, 'forecast', 1)


def do_one(var, url, basename, expected_len):
    model, _ = var.split('.')
    args = default_download_args | overrides.get(model, {})

    with tempfile.NamedTemporaryFile() as f:
        ds = dl.download(
            url,
            f.name,
            verbose=True,
            **args
        )
        print(ds)

        assert len(ds['T']) == expected_len
        # Some grids are on the integer degrees, so three points
        # get selected. Others are on the half degree, so two get
        # selected.
        assert len(ds['Y']) in (2, 3)
        assert len(ds['X']) in (2, 3)

        reference_file = reference_dir / f'{basename}-{var}.tsv'
        if reference_file.exists():
            ds_ref = cptio.open_cptdataset(reference_file)
            xr.testing.assert_equal(ds, ds_ref)
        else:
            reference_file.hardlink_to(f.name)
