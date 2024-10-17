import numpy as np
from pathlib import Path

from cptcore import canonical_correlation_analysis, deterministic_skill, probabilistic_forecast_verification, principal_components_regression, multiple_regression
import cptio

def load_southasia_nmme():
    x = cptio.open_cptdataset(str(Path( __file__ ).absolute().parents[0] / 'data/seasonal-core/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').replace('.egg', '')).prec
    y = cptio.open_cptdataset(str( Path( __file__).absolute().parents[0] / 'data/seasonal-core/SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv').replace('.egg', '')).prate
    return x, y

def test_cca(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts, fcsts, skill_values, x_pattern_values, y_pattern_values = canonical_correlation_analysis(x, y, F=f, **kwargs)

    assert set(hcsts.data_vars) == set(['deterministic'])
    da = hcsts['deterministic']
    assert da.dims == ('T', 'Y', 'X')
    assert da.shape == (41, 33, 30)
    np.testing.assert_array_equal(da['Y'].values, range(37, 4, -1))
    np.testing.assert_array_equal(da['X'].values, range(68, 98))

    # TODO more assertions

def test_deterministic_skill(**kwargs):
    x, y = load_southasia_nmme()
    skill = deterministic_skill(x, y, **kwargs)
    # TODO make assertions

def test_pfv(**kwargs):
    y = cptio.open_cptdataset(str(Path( __file__ ).absolute().parents[0] / 'data/seasonal-core/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv')).prec
    x = cptio.open_cptdataset( str(Path( __file__).absolute().parents[0] / 'data/seasonal-core/prob_rfcsts.tsv')).probabilistic
    skill = probabilistic_forecast_verification(x, y,  **kwargs)
    print(skill)
    assert set(skill.data_vars) == set(['generalized_roc', 'ignorance', 'rank_probability_skill_score'])
    assert list(skill.coords) == ['Y', 'X']
    np.testing.assert_array_equal(skill.coords['Y'].values, range(37, 4, -1))
    np.testing.assert_array_equal(skill.coords['X'].values, range(68, 98))
    for m in skill.data_vars:
        assert skill[m].notnull().all()

def test_pcr(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts,  fcst,  skill, loadings = principal_components_regression(x, y, F=f, **kwargs)
    # TODO make assertions

def test_mlr(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts, fcst, skill = multiple_regression(x, y, F=f, **kwargs)
    # TODO assertions
