import cptio as ct 
from cptcore import probabilistic_forecast_verification
import numpy as np
from pathlib import Path 

def test_pfv(**kwargs):
    y = ct.open_cptdataset(str(Path( __file__ ).absolute().parents[0] / 'data/seasonal/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').replace('.egg', '')).prec
    x = ct.open_cptdataset( str(Path( __file__).absolute().parents[0] / 'data/seasonal/prob_rfcsts.tsv').replace('.egg', '')).probabilistic
    skill = probabilistic_forecast_verification(x, y,  **kwargs)
    print(skill)
    assert set(skill.data_vars) == set(['generalized_roc', 'ignorance', 'rank_probability_skill_score'])
    assert list(skill.coords) == ['Y', 'X']
    np.testing.assert_array_equal(skill.coords['Y'].values, range(37, 4, -1))
    np.testing.assert_array_equal(skill.coords['X'].values, range(68, 98))
    for m in skill.data_vars:
        assert skill[m].notnull().all()
