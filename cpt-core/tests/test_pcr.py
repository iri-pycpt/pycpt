from cptcore import principal_components_regression
from .load_data import load_southasia_nmme

def test_pcr(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts,  fcst,  skill, loadings = principal_components_regression(x, y, F=f, **kwargs)
    # TODO make assertions