import cptio as ct 
from pathlib import Path 

def load_southasia_nmme():
    x = ct.open_cptdataset(str(Path( __file__ ).absolute().parents[0] / 'data/seasonal/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').replace('.egg', '')).prec
    y = ct.open_cptdataset(str( Path( __file__).absolute().parents[0] / 'data/seasonal/SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv').replace('.egg', '')).prate
    return x, y
