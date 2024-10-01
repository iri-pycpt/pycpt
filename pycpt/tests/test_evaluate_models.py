# TODO figure out why we get a numpy version incompatibility warning if we don't import this first.
import netCDF4  # noqa: F401

import datetime as dt
import numpy as np
from pathlib import Path
import tempfile

import cptio
import pycpt

DEFAULT_DOWNLOAD_ARGS = { 
    'fdate':  dt.datetime(2023, 5, 1),
    'first_year': 1992,  
    'final_year': 2021,  
    'target': 'Jun-Sep',
    'predictor_extent': {
        'west': -76, 
        'east':  -74,
        'south': 39, 
        'north': 41,
    }, 
    'predictand_extent': {
        'west':  -75.5,
        'east': -74.5,
        'south': 39.5,
        'north': 40.5,
      },
    'filetype': 'cptv10.tsv'
}

DEFAULT_CPT_ARGS = { 
    'transform_predictand': None,
    'tailoring': None,
    'cca_modes': (1,3),
    'x_eof_modes': (1,8),
    'y_eof_modes': (1,6),
    'validation': 'crossvalidation',
    'drymask_threshold': None,
    'skillmask_threshold': None,
    'scree': True,
    'crossvalidation_window': 5,
    'synchronous_predictors': True,
}

def open_cptdataarray(path):
    'Like cptio.open_cptdataset but returns a DataArray instead of a Dataset'
    ds = cptio.open_cptdataset(path)
    das = list(ds.data_vars.values())
    assert len(das) == 1
    return das[0]
                
def get_test_data(predictor_names, predictand_name):
    datadir = Path( __file__ ).absolute().parents[0] / 'data'
    hindcast_data = []
    forecast_data = []
    for p in predictor_names:
        hindcast_data.append(open_cptdataarray(datadir / f'{p}.tsv'))
        forecast_data.append(open_cptdataarray(datadir / f'{p}_f2023.tsv'))
    Y = open_cptdataarray(datadir / f'{predictand_name}.tsv')
    return Y, hindcast_data, forecast_data

def test_evaluate_models_default():
    MOS = 'CCA'
    predictor_names = [ "SPEAR.PRCP", "CCSM4.PRCP"]
    predictand_name = 'UCSB.PRCP'
    download_args = DEFAULT_DOWNLOAD_ARGS
    cpt_args = DEFAULT_CPT_ARGS
    Y, hindcast_data, forecast_data = get_test_data(predictor_names, predictand_name)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), download_args["predictor_extent"])
        interactive = False
        hcsts, fcsts, skill, pxs, pys = pycpt.evaluate_models(hindcast_data, MOS, Y, forecast_data, cpt_args, domain_dir, predictor_names, interactive)

    assert len(hcsts) == len(predictor_names)
    for h in hcsts:
        assert set(h.data_vars) == set(['deterministic', 'probabilistic', 'prediction_error_variance'])
        assert h['deterministic'].dims == ('T', 'Y', 'X')
        assert h['prediction_error_variance'].dims == ('T', 'Y', 'X')
        assert h['probabilistic'].dims == ('C', 'T', 'Y', 'X')
        assert h['T'].equals(hindcast_data[0]['T'])
        for dim in ('Y', 'X'):
            assert h[dim].equals(Y[dim])

    assert len(fcsts) == len(predictor_names)
    for f in fcsts:
        assert set(f.data_vars) == set(['deterministic', 'probabilistic', 'prediction_error_variance'])
        assert f['deterministic'].dims == ('T', 'Y', 'X')
        assert f['prediction_error_variance'].dims == ('T', 'Y', 'X')
        assert f['probabilistic'].dims == ('C', 'T', 'Y', 'X')
        assert f['T'].equals(forecast_data[0]['T'])
        for dim in ('Y', 'X'):
            assert h[dim].equals(Y[dim])

    assert len(skill)  == len(predictor_names)
    for s in skill:
        assert set(s.data_vars) == set(['pearson', 'spearman', 'two_alternative_forced_choice', 'roc_area_below_normal', 'roc_area_above_normal', 'generalized_roc', 'ignorance', 'rank_probability_skill_score'])
        assert s['pearson'].dims == ('Y', 'X')
        for dim in ('Y', 'X'):
            assert s[dim].equals(Y[dim])

    assert len(pxs) == len(predictor_names)
    for p in pxs:
        assert set(p.data_vars) == set(['x_cca_scores', 'x_eof_scores', 'x_cca_loadings', 'x_eof_loadings', 'x_explained_variance'])
        assert p['x_cca_scores'].dims == ('T', 'Mode')
        assert p['x_cca_scores']['T'].equals(hcsts[0]['T'].drop_vars(['S']))
        np.testing.assert_array_equal(p['x_cca_scores']['Mode'].values, range(1,10))
        for dim in ('Y', 'X'):
            assert p[dim].equals(hindcast_data[0][dim])
    
    assert len(pys) == len(predictor_names)
    for p in pys:
        assert set(p.data_vars) == set(['y_cca_scores', 'y_eof_scores', 'y_cca_loadings', 'y_eof_loadings', 'y_explained_variance'])
        assert p['y_cca_scores'].dims == ('T', 'Mode')
        assert p['y_cca_scores']['T'].equals(hcsts[0]['T'].drop_vars(['S']))
        np.testing.assert_array_equal(p['y_cca_scores']['Mode'].values, range(1,17))
        for dim in ('Y', 'X'):
            assert p[dim].equals(Y[dim])
