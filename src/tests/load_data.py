import cptio as ct 
from .. import canonical_correlation_analysis
import xarray as xr 
from pathlib import Path 

def load_southasia_nmme():
    x = ct.open_cptdataset(Path( __file__ ).absolute().parents[0] / 'data/seasonal/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').prec
    y = ct.open_cptdataset( Path( __file__).absolute().parents[0] / 'data/seasonal/SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv').prate
    return x, y

def load_lesotho_nmme():
    x = xr.open_dataset(Path( __file__ ).absolute().parents[0] / 'data/seasonal/nmme_lead1_ond.nc').prec
    y = ct.open_cptdataset( Path( __file__).absolute().parents[0] / 'data/seasonal/lesotho_ond.tsv').rfe
    return x, y