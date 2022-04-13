import cpttools as ct 
from .. import multiple_regression


from pathlib import Path 
def test_mlr(**kwargs):
    if 'synchronous_predictors' not in kwargs.keys():
        kwargs['synchronous_predictors'] = False
    x = ct.open_cptdataset(Path( __file__ ).absolute().parents[0] / 'data/seasonal/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').prec
    y = ct.open_cptdataset( Path( __file__).absolute().parents[0] / 'data/seasonal/SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv') .prate
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts, fcst, skill = multiple_regression(x, y, F=f, **kwargs)
    return hcsts, fcst, skill
