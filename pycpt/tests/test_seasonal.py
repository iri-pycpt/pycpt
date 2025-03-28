# TODO figure out why we get a numpy version incompatibility warning if we don't import this first.
import netCDF4  # noqa: F401

import datetime as dt
import numpy as np
from pathlib import Path
import shutil
import tempfile

import cptio
import pycpt

PREDICTOR_NAMES = [ "SPEAR.PRCP", "CCSM4.PRCP"]
DEFAULT_PREDICTAND_NAME = 'UCSB.PRCP'

DOWNLOAD_ARGS = {
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
    'crossvalidation_window': 5,
    'synchronous_predictors': True,
}

SAVED_DATA_DIR = Path( __file__ ).absolute().parents[0] / 'data' / 'seasonal'

def download_test_data():
    '''Run this by hand, then track the downloaded files in version control
    so that tests can run without hitting the network.'''
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        Y, hindcast_data, forecast_data = pycpt.download_data(
            DEFAULT_PREDICTAND_NAME, None, PREDICTOR_NAMES, DOWNLOAD_ARGS,
            domain_dir, force_download=True
        )
        data_dir = domain_dir / 'data'
        for f in data_dir.glob('*.tsv'):
            print(f'copy {data_dir / f} to {SAVED_DATA_DIR}')
            shutil.copy(data_dir / f, SAVED_DATA_DIR)

def get_test_data(predictor_names, predictand_name):
    datadir = SAVED_DATA_DIR
    hindcast_data = []
    forecast_data = []
    for p in predictor_names:
        hindcast_data.append(cptio.open_cptdataarray(datadir / f'{p}.tsv'))
        forecast_data.append(cptio.open_cptdataarray(datadir / f'{p}_f2023.tsv'))
    Y = cptio.open_cptdataarray(datadir / f'{predictand_name}.tsv')
    return Y, hindcast_data, forecast_data

def call_evaluate_models(original_hindcasts, original_forecasts, Y, MOS, cpt_args_override):
    cpt_args = DEFAULT_CPT_ARGS | cpt_args_override
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        interactive = False
        hcsts, fcsts, skill, pxs, pys = pycpt.evaluate_models(
            original_hindcasts, MOS, Y, original_forecasts, cpt_args, domain_dir, PREDICTOR_NAMES, interactive
        )

    return hcsts, fcsts, skill, pxs, pys

def check_MOS_results(original_hcsts, original_fcsts, Y, hcsts, fcsts, pxs, pys):
    assert len(hcsts) == len(PREDICTOR_NAMES)
    for h in hcsts:
        assert set(h.data_vars) == set(['deterministic', 'probabilistic', 'prediction_error_variance'])
        assert h['deterministic'].dims == ('T', 'Y', 'X')
        assert h['prediction_error_variance'].dims == ('T', 'Y', 'X')
        assert h['probabilistic'].dims == ('C', 'T', 'Y', 'X')
        assert h['T'].equals(original_hcsts[0]['T'])
        for dim in ('Y', 'X'):
            assert h[dim].equals(Y[dim])

    assert len(fcsts) == len(PREDICTOR_NAMES)
    for i, f in enumerate(fcsts):
        t = None
        if original_fcsts[i] is None:
            assert f is None
        else:
            assert set(f.data_vars) == set(['deterministic', 'probabilistic', 'prediction_error_variance'])
            assert f['deterministic'].dims == ('T', 'Y', 'X')
            assert f['prediction_error_variance'].dims == ('T', 'Y', 'X')
            assert f['probabilistic'].dims == ('C', 'T', 'Y', 'X')
            if t is None:
                t = f['T']
            else:
                assert f['T'].equals(t)
            for dim in ('Y', 'X'):
                assert f[dim].equals(Y[dim])

    assert len(pxs) == len(PREDICTOR_NAMES)
    print(pxs)
    for p in pxs:
        assert 'x_eof_scores' in p.data_vars
        assert 'x_eof_loadings' in p.data_vars
        assert 'x_explained_variance' in p.data_vars
        for dim in ('Y', 'X'):
            assert p[dim].equals(original_hcsts[0][dim])

    assert len(pys) == len(PREDICTOR_NAMES)


def check_skill(Y, skill, metrics):
    assert set(metrics) <= set(skill.data_vars)
    assert skill[metrics[0]].dims == ('Y', 'X')
    for dim in ('Y', 'X'):
        assert skill[dim].equals(Y[dim])

DETERMINISTIC_SKILL_METRICS = [
    'pearson', 'spearman', 'two_alternative_forced_choice',
    'roc_area_below_normal', 'roc_area_above_normal'
]

PROBABILISTIC_SKILL_METRICS = [
    'generalized_roc', 'ignorance', 'rank_probability_skill_score'
]

def check_model_skills(Y, skills, metrics):
    assert len(skills) == len(PREDICTOR_NAMES)
    for s in skills:
        check_skill(Y, s, metrics)

def test_evaluate_models_cca():
    MOS = 'CCA'
    cpt_args = {}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    check_MOS_results(original_hcsts, original_fcsts, Y, hcsts, fcsts, pxs, pys)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)
    # additional data specific to CCA
    for p in pxs:
        assert 'x_cca_scores' in p.data_vars
        assert p['x_cca_scores'].dims == ('T', 'Mode')
        assert p['x_cca_scores']['T'].equals(hcsts[0]['T'].drop_vars(['S']))
        np.testing.assert_array_equal(p['x_cca_scores']['Mode'].values, range(1,10))
    for p in pys:
        assert 'y_eof_scores' in p.data_vars
        assert 'y_eof_loadings' in p.data_vars
        assert 'y_explained_variance' in p.data_vars
        for dim in ('Y', 'X'):
            assert p[dim].equals(fcsts[0][dim])
        assert p['y_cca_scores'].dims == ('T', 'Mode')
        assert p['y_cca_scores']['T'].equals(hcsts[0]['T'].drop_vars(['S']))
        np.testing.assert_array_equal(p['y_cca_scores']['Mode'].values, range(1,17))

def test_evaluate_models_pcr():
    MOS = 'PCR'
    cpt_args = {}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    check_MOS_results(original_hcsts, original_fcsts, Y, hcsts, fcsts, pxs, pys)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)
    assert pys == [None, None]

def test_evaluate_models_nomos():
    MOS = None
    cpt_args = {}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    assert hcsts == []
    assert fcsts == []
    print(skill)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS)

def test_evaluate_models_gamma():
    MOS = 'CCA'
    cpt_args = {'transform_predictand': 'Gamma'}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    # TODO what should we assert? Something about the magnitude of the prediction error variance?

def test_evaluate_models_empirical():
    MOS = 'CCA'
    cpt_args = {'transform_predictand': 'Empirical'}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    # TODO what should we assert? Something about the magnitude of the prediction error variance?

def test_evaluate_models_anomaly():
    MOS = 'CCA'
    cpt_args = {'tailoring': 'Anomaly'}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    # TODO what should we assert? Something about the magnitude of the prediction error variance?

def test_evaluate_models_drymask():
    MOS = 'CCA'
    thresh = 451
    cpt_args = {'drymask_threshold': thresh}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    mask = Y.mean('T') < thresh
    assert mask.sum() == 4  # four out of sixteen points fall below the threshold
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    f = fcsts[0]['deterministic'].isel(T=0).drop_vars(['T', 'Ti', 'Tf', 'S'])
    # forecast is null at points that average less than the threshold, and only there.
    assert f.isnull().equals(mask)

def test_evaluate_models_skillmask():
    MOS = 'CCA'
    thresh = 0.3
    cpt_args = {'skillmask_threshold': thresh}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    mask = skill[0]['pearson'] < thresh
    assert mask.sum() == 4  # four pixels are masked
    f = fcsts[0]['probabilistic']
    # climatological forecast at the masked locations
    assert (f.sel(C='1') == 33).where(mask).sum() == 4
    assert (f.sel(C='2') == 34).where(mask).sum() == 4
    assert (f.sel(C='3') == 33).where(mask).sum() == 4

def test_evaluate_models_missing_cca():
    MOS = 'CCA'
    cpt_args = {}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    original_fcsts[0] = None
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    check_MOS_results(original_hcsts, original_fcsts, Y, hcsts, fcsts, pxs, pys)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)
    assert pys == [None, None]

def test_evaluate_models_missing_pcr():
    MOS = 'PCR'
    cpt_args = {}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    original_fcsts[0] = None
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    check_MOS_results(original_hcsts, original_fcsts, Y, hcsts, fcsts, pxs, pys)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)
    assert pys == [None, None]

def test_construct_mme():
    MOS = 'CCA'
    cpt_args = DEFAULT_CPT_ARGS
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        interactive = False
        hcsts, fcsts, skill, pxs, pys = pycpt.evaluate_models(
            original_hcsts, MOS, Y, original_fcsts, cpt_args, domain_dir, PREDICTOR_NAMES, interactive
        )
        det_fcst, pr_fcst, pev_fcst, nextgen_skill = pycpt.construct_mme(
            fcsts, hcsts, Y, PREDICTOR_NAMES, PREDICTOR_NAMES, cpt_args, domain_dir
        )
    assert det_fcst.dims == ('T', 'Y', 'X')
    assert det_fcst.shape == (1, 4, 4)
    assert pr_fcst.dims == ('C', 'T', 'Y', 'X')
    assert pr_fcst.shape == (3, 1, 4, 4)
    assert pev_fcst.dims == ('T', 'Y', 'X')
    assert pev_fcst.shape == (1, 4, 4)
    check_skill(Y, nextgen_skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)

def test_construct_mme_missing():
    MOS = 'CCA'
    cpt_args = DEFAULT_CPT_ARGS
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, DEFAULT_PREDICTAND_NAME)
    original_fcsts[0] = None
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        interactive = False
        hcsts, fcsts, skill, pxs, pys = pycpt.evaluate_models(
            original_hcsts, MOS, Y, original_fcsts, cpt_args, domain_dir, PREDICTOR_NAMES, interactive
        )
        det_fcst, pr_fcst, pev_fcst, nextgen_skill = pycpt.construct_mme(
            fcsts, hcsts, Y, PREDICTOR_NAMES, PREDICTOR_NAMES, cpt_args, domain_dir
        )
    print(type(det_fcst.sizes))
    assert det_fcst.sizes == {'T': 1, 'Y': 4, 'X': 4}
    assert pr_fcst.sizes == {'C': 3, 'T': 1, 'Y': 4, 'X': 4}
    assert pev_fcst.sizes == {'T': 1, 'Y': 4, 'X': 4}
    check_skill(Y, nextgen_skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)


if __name__ == '__main__':
    download_test_data()
