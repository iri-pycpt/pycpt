# TODO figure out why we get a numpy version incompatibility warning if we don't import this first.
import netCDF4  # noqa: F401

import datetime as dt
import numpy as np
from pathlib import Path
import pytest
import shutil
import tempfile

import cptdl
import cptio
import pycpt

PREDICTOR_NAMES = [ "CanSIPSIC3.PRCP", 'SEAS51.PRCP']
# GHCN is not in the catalog; we monkeypatch it in download_test_data.
PREDICTAND_NAME = 'GHCN.PRCP'

DOWNLOAD_ARGS = {
    'fdate':  dt.datetime(2023, 5, 1),
    'first_year': 1981,
    'final_year': 1995,
    'target': 'Jun-Sep',
    'predictor_extent': {
        'west': -90,
        'east':  -60,
        'south': 30,
        'north': 55,
      }, 
    'predictand_extent': {
        'west':  -79.3,
        'east': -72.3,
        'south': 40.7,
        'north': 44.9,
      },
    'filetype': 'cptv10.tsv'
}

DEFAULT_CPT_ARGS = { 
    'transform_predictand': None,
    'tailoring': None,
    'cca_modes': (1,2),
    'x_eof_modes': (1,8),
    'y_eof_modes': (1,2),  # have to use fewer modes because there are few stations
    'validation': 'crossvalidation',
    'drymask_threshold': None,
    'skillmask_threshold': None,
    'scree': True,
    'crossvalidation_window': 5,
    'synchronous_predictors': True,
}

SAVED_DATA_DIR = Path( __file__ ).absolute().parents[0] / 'data' / 'station'

def download_test_data():
    '''Run this by hand, then track the downloaded files in version control
    so that tests can run without hitting the network.'''
    fake_predictor_extent = dict(west=0, east=0, south=0, north=0)
    with tempfile.TemporaryDirectory() as case_dir_name:
        domain_dir = pycpt.setup(Path(case_dir_name), fake_predictor_extent)
        saved = cptdl.observations
        cptdl.observations = dict(cptdl.observations, **{'GHCN.PRCP': "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCDC/.GHCN/.v2beta/IWMO/grid%3A//name/(IWMO)/def//units/(ids)/def//long_name/(Station)/def/1/1/20590/%3Agrid/replaceGRID/lon/{predictand_extent['west']}/{predictand_extent['east']}/masknotrange/SELECT/lat/{predictand_extent['south']}/{predictand_extent['north']}/masknotrange/SELECT/dup/a%3A/.lat//name//Y/def/%3Aa%3A/.lon//name//X/def/%3Aa%3A/.Name/%3Aa%3A/.prcp/T/({target}%20{first_year}-{final_year})/seasonalAverage/-999/setmissing_value/%3Aa/%7BY/X/Name/prcp%7Dds/{'%5BIWMO%5D%5BT%5D/' + filetype if filetype == 'cptv10.tsv' else 'data.nc'}"})
        try:
            Y, hindcast_data, forecast_data = pycpt.download_data(
                PREDICTAND_NAME, None, PREDICTOR_NAMES, DOWNLOAD_ARGS,
                domain_dir, force_download=True
            )
        finally:
            cptdl.observations = saved
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
        assert h['deterministic'].dims == ('T', 'station')
        assert h['prediction_error_variance'].dims == ('T', 'station')
        assert h['probabilistic'].dims == ('C', 'T', 'station')
        assert h['T'].equals(original_hcsts[0]['T'])
        for dim in (['station']):
            # using np.equal because Y['station'] has a Name coordinate
            # but h['station'] doesn't, so they're not da.equals().
            np.testing.assert_array_equal(h[dim], Y[dim])

    assert len(fcsts) == len(PREDICTOR_NAMES)
    for f in fcsts:
        assert set(f.data_vars) == set(['deterministic', 'probabilistic', 'prediction_error_variance'])
        assert f['deterministic'].dims == ('T', 'station')
        assert f['prediction_error_variance'].dims == ('T', 'station')
        assert f['probabilistic'].dims == ('C', 'T', 'station')
        assert f['T'].equals(original_fcsts[0]['T'])
        for dim in (['station']):
            np.testing.assert_array_equal(f[dim], Y[dim])

    assert len(pxs) == len(PREDICTOR_NAMES)
    print(pxs)
    for p in pxs:
        assert 'x_eof_scores' in p.data_vars
        assert 'x_eof_loadings' in p.data_vars
        assert 'x_explained_variance' in p.data_vars
        for dim in (['Y', 'X']):
            np.testing.assert_array_equal(p[dim], original_hcsts[0][dim])

    assert len(pys) == len(PREDICTOR_NAMES)


def check_skill(Y, skill, metrics):
    assert set(metrics) <= set(skill.data_vars)
    assert skill[metrics[0]].dims == ('station',)
    for dim in (['station']):
        np.testing.assert_array_equal(skill[dim], Y[dim])

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
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    check_MOS_results(original_hcsts, original_fcsts, Y, hcsts, fcsts, pxs, pys)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)
    # additional data specific to CCA
    for p, h in zip(pxs, hcsts):
        assert 'x_cca_scores' in p.data_vars
        assert p['x_cca_scores'].dims == ('T', 'Mode')
        assert p['x_cca_scores']['T'].equals(h['T'].drop_vars(['S']))
        np.testing.assert_array_equal(p['x_cca_scores']['Mode'].values, range(1,14))
    for p, f in zip(pys, fcsts):
        assert 'y_eof_scores' in p.data_vars
        assert 'y_eof_loadings' in p.data_vars
        assert 'y_explained_variance' in p.data_vars
        for dim in (['station']):
            np.testing.assert_array_equal(p[dim], f[dim])
        assert p['y_cca_scores'].dims == ('T', 'Mode')
        assert p['y_cca_scores']['T'].equals(Y['T'])
        np.testing.assert_array_equal(p['y_cca_scores']['Mode'].values, range(1,14))

def test_coordinate_precision():
    '''Regression test for a bug where station on the boundary of the domain gets
    dropped because of inconsistency in the handling of floating point values'''
    MOS = 'CCA'
    cpt_args = {}
    predictor_names = PREDICTOR_NAMES[:1] # speed it up; no point using multiple here
    Y, original_hcsts, original_fcsts = get_test_data(predictor_names, PREDICTAND_NAME)
    assert Y['X'].min().item() == -79.3
    assert len(Y['station']) == 115
    Y['X'] = Y['X'].where(Y['X'] != -79.3, -79.299999)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    assert len(hcsts[0]['station']) == 115

def test_evaluate_models_pcr():
    MOS = 'PCR'
    cpt_args = {}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    check_MOS_results(original_hcsts, original_fcsts, Y, hcsts, fcsts, pxs, pys)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)
    assert pys == [None, None]

@pytest.mark.skip(reason="Currently hangs. Test again with new version of CPT")
def test_evaluate_models_nomos():
    MOS = None
    cpt_args = {}
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, PREDICTAND_NAME)
    hcsts, fcsts, skill, pxs, pys = call_evaluate_models(original_hcsts, original_fcsts, Y, MOS, cpt_args)
    assert hcsts == []
    assert fcsts == []
    print(skill)
    check_model_skills(Y, skill, DETERMINISTIC_SKILL_METRICS)

def test_construct_mme():
    MOS = 'CCA'
    cpt_args = DEFAULT_CPT_ARGS
    Y, original_hcsts, original_fcsts = get_test_data(PREDICTOR_NAMES, PREDICTAND_NAME)
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
    assert det_fcst.dims == ('T', 'station')
    assert det_fcst.shape == (1, 115)
    assert pr_fcst.dims == ('C', 'T', 'station')
    assert pr_fcst.shape == (3, 1, 115)
    assert pev_fcst.dims == ('T', 'station')
    assert pev_fcst.shape == (1, 115)
    check_skill(Y, nextgen_skill, DETERMINISTIC_SKILL_METRICS + PROBABILISTIC_SKILL_METRICS)


if __name__ == '__main__':
    download_test_data()
