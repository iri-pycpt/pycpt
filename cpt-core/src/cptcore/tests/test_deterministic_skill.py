import cptio as ct 
from .. import deterministic_skill
from .load_data import load_southasia_nmme
from pathlib import Path 

def test_deterministic_skill(**kwargs):
    x, y = load_southasia_nmme()
    skill = deterministic_skill(x, y, **kwargs)
    return skill