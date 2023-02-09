import cptio as ct 
from .. import principal_components_regression
from .load_data import load_southasia_nmme

from pathlib import Path 
def test_pcr(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts,  fcst,  skill, loadings = principal_components_regression(x, y, F=f, **kwargs)
    return hcsts,  fcst,  skill, loadings