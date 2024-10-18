# TODO figure out why we get a numpy version incompatibility warning if we don't import this first.
import netCDF4  # noqa: F401

import datetime as dt
from pathlib import Path
import shutil
import tempfile

import pycpt
from pycpt import subseasonal

predictor_names = ["GEFSv12.PRCP"]
predictand_name = 'CHIRPS.PRCP'
download_args = { 
    'fdate':  dt.datetime(2023, 6, 1),
    'leads': [
        ('Week 1', 1, 7),
        ('Week 2', 8, 14),
    ],
    'training_season': 'May-Jul',
    'predictor_extent': {
        'east':  79,
        'west': 81, 
        'north': 21,
        'south': 19, 
    }, 
    'predictand_extent': {
        'east':  79,
        'west': 81, 
        'north': 21,
        'south': 19, 
    },
    'filetype': 'cptv10.tsv'
}

cpt_args = { 
    'transform_predictand': None,
    'tailoring': None,
    'cca_modes': (1,3),
    'x_eof_modes': (1,8),
    'y_eof_modes': (1,6),
    'validation': 'retroactive',
    'drymask': False,
    'crossvalidation_window': 5,
    'synchronous_predictors': True,
    'retroactive_initial_training_period': 45,
    'retroactive_step': 10,    
}

SAVED_DATA_DIR = Path( __file__ ).absolute().parents[0] / 'data' / 'subseasonal'

def download_test_data():
    '''Run this by hand, then track the downloaded files in version control
    so that tests can run without hitting the network.'''
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        Y, hindcast_data, forecast_data = subseasonal.download_data(
            predictor_names, predictand_name, download_args,
            domain_dir, force_download=True
        )
        data_dir = domain_dir / 'data'
        for f in data_dir.glob('*.nc'):
            print(f'copy {data_dir / f} to {SAVED_DATA_DIR}')
            shutil.copy(data_dir / f, SAVED_DATA_DIR)

def read_test_data():
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        for f in SAVED_DATA_DIR.glob('*.nc'):
            shutil.copy(SAVED_DATA_DIR / f, domain_dir / 'data')
        hindcast_data, Y, forecast_data = subseasonal.download_data(
            predictor_names, predictand_name, download_args,
            domain_dir, force_download=False
        )
    return Y, hindcast_data, forecast_data

def test_subseasonal():
    MOS = 'CCA'
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    Y, original_hcsts, original_fcsts = read_test_data()
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        hcsts, fcsts, skill, pxs, pys = subseasonal.evaluate_models(
            original_hcsts, original_fcsts, Y, MOS, cpt_args, domain_dir
        )
        assert set(hcsts.data_vars) == set(['deterministic', 'probabilistic', 'prediction_error_variance'])
        assert hcsts['deterministic'].dims == ('model', 'lead_name', 'T', 'Y', 'X')
        assert hcsts['deterministic'].shape == (1, 2, 137, 5, 5)
        assert hcsts['prediction_error_variance'].dims == ('model', 'lead_name', 'T', 'Y', 'X')
        assert hcsts['prediction_error_variance'].shape == (1, 2, 137, 5, 5)
        assert hcsts['probabilistic'].dims == ('model', 'lead_name', 'C', 'T', 'Y', 'X')
        assert hcsts['probabilistic'].shape == (1, 2, 3, 137, 5, 5)

        assert set(fcsts.data_vars) == set(['deterministic', 'probabilistic', 'prediction_error_variance'])
        assert fcsts['deterministic'].dims == ('model', 'lead_name', 'T', 'Y', 'X')
        assert fcsts['deterministic'].shape == (1, 2, 1, 5, 5)
        assert fcsts['prediction_error_variance'].dims == ('model', 'lead_name', 'T', 'Y', 'X')
        assert fcsts['prediction_error_variance'].shape == (1, 2, 1, 5, 5)
        assert fcsts['probabilistic'].dims == ('model', 'lead_name', 'C', 'T', 'Y', 'X')
        assert fcsts['probabilistic'].shape == (1, 2, 3, 1, 5, 5)



if __name__ == '__main__':
    download_test_data()