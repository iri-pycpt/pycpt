import cpttools as ct 
from .. import probabilistic_forecast_verification
from pathlib import Path 

def test_pfv():
    y = ct.open_cptdataset(Path( __file__ ).absolute().parents[0] / 'data/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').prec
    x = ct.open_cptdataset( Path( __file__).absolute().parents[0] / 'data/prob_rfcsts.tsv').prate
    skill = probabilistic_forecast_verification(x, y,  cpt_kwargs={'log':'pfv_log'})
