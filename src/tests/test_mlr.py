import cptio as ct 
from .. import multiple_regression
from .load_data import load_southasia_nmme

from pathlib import Path 
def test_mlr(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts, fcst, skill = multiple_regression(x, y, F=f, **kwargs)
    return hcsts, fcst, skill
