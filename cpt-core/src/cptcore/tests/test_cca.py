import cptio as ct 
from .. import canonical_correlation_analysis
from .load_data import load_southasia_nmme
from pathlib import Path 

def test_cca(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    return canonical_correlation_analysis(x, y, F=f, **kwargs)
