import cpttools as ct 
from .. import deterministic_skill
from pathlib import Path 

def test_deterministic_skill():
    x = ct.open_cptdataset(Path( __file__ ).absolute().parents[0] / 'data/seasonal/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').prec
    y = ct.open_cptdataset( Path( __file__).absolute().parents[0] / 'data/seasonal/SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv') .prate
    skill = deterministic_skill(x, y,  cpt_kwargs={'log':'detskill_log'})
    return skill