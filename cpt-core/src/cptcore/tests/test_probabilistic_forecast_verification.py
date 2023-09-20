import cptio as ct 
from .. import probabilistic_forecast_verification
from pathlib import Path 

def test_pfv(**kwargs):
    y = ct.open_cptdataset(str(Path( __file__ ).absolute().parents[0] / 'data/seasonal/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').replace('.egg', '')).prec
    x = ct.open_cptdataset( str(Path( __file__).absolute().parents[0] / 'data/seasonal/prob_rfcsts.tsv').replace('.egg', '')).probabilistic
    skill = probabilistic_forecast_verification(x, y,  **kwargs)
    return skill