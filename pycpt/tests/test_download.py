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
    'predictand_extent': {
        'west': -76,
        'east':  -74,
        'south': 39,
        'north': 41,
      },
    'pressure': 200,
}

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
        'target': 'Sep-Nov',
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
    # For some reason the UCSB variables are all 0.05 degree, except PRCP where
    # UCSB.PRCP is 0.25 degrees and the 0.05 degree variables is called UCBP0p05.PRCP.
    'UCSB': {
        'predictand_extent': {
            'west': -75.375,
            'east':  -75.475,
            'south': 40.875,
            'north': 40.975,
        },
        'target': 'Jun-Jul'
    },
    'UCSB.PRCP': {
        'predictand_extent': {
            'west': -75.375,
            'east':  -75.875,
            'south': 40.375,
            'north': 40.875,
        },
        'target': 'Jun-Jul'
    },
    'UCSB0p05.PRCP': {
        'predictand_extent': {
            'west': -75.375,
            'east':  -75.475,
            'south': 40.875,
            'north': 40.975,
        },
        'target': 'Jun-Jul'
    },
    'CPCCMAPURD': {
        'predictand_extent': {
            'west': 0,
            'east':  2,
            'south': 40,
            'north': 42,
        },
        'target': 'Jun-Jul'
    },
    'TRMM': {
        'predictand_extent': {
            'west': -75,
            'east':  -72,
            'south': 40,
            'north': 43,
        },
        'target': 'Jun-Jul'
    },
    'ERSSTV5': {
        'predictand_extent': {
            'west': -76,
            'east':  -70,
            'south': 40,
            'north': 46,
        },
        'target': 'Jun-Jul'
    },
    'OISST2p1': {
        'predictand_extent': {
            'west': -75.375,
            'east':  -75.875,
            'south': 40.375,
            'north': 40.875,
        },
        'target': 'Jun-Jul'
    },
    'NNRP': {
        'predictand_extent': {
            'west': -76,
            'east':  -68.5,
            'south': 39,
            'north': 46.5,
        },
        'target': 'Jun-Jul'
    },
}

@pytest.mark.skip
@pytest.mark.parametrize('var', dl.hindcasts.keys())
def test_hindcast_download(var):
    url = dl.hindcasts[var]
    do_one(var, url, 'hindcast', 3)


@pytest.mark.skip
@pytest.mark.parametrize('var', dl.forecasts.keys())
def test_forecast_download(var):
    url = dl.forecasts[var]
    do_one(var, url, 'forecast', 1)

obs_to_test = {
    k: v
    for k, v in dl.observations.items()
    # Omit private datasets. We plan not to add any more
    # (private datasets are now added to the catalog in the
    # notebook rather than in catalog.py), so we just have to
    # omit those that have already been added.
    if not (
        k.startswith('ENACTS') or
        k.startswith('CHILE')
    )
}
@pytest.mark.skip
@pytest.mark.parametrize('var', obs_to_test.keys())
def test_obs_download(var):
    url = dl.observations[var]
    do_one(var, url, 'obs', 3)

def do_one(var, url, basename, expected_len):
    model, _ = var.split('.')
    override = overrides.get(var, overrides.get(model, {}))
    args = default_download_args | override
    args = pycpt.notebook._preprocess_download_args(args)


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
            try:
                xr.testing.assert_equal(ds, ds_ref)
            except Exception:
                reference_file.with_suffix(".new").hardlink_to(f.name)
                raise
        else:
            reference_file.hardlink_to(f.name)
