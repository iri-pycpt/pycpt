from .. import deterministic_skill
from .load_data import load_southasia_nmme

def test_deterministic_skill(**kwargs):
    x, y = load_southasia_nmme()
    skill = deterministic_skill(x, y, **kwargs)
    # TODO make assertions