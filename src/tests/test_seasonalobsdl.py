
import pytest 
from .. import * 


def make_obs_args():
    #predictor_domain = GeographicExtent(20,50, -110, -70)
    predictand_domain = GeographicExtent(20,50, -110, -70)
    attrs = [recursive_getattr(SEASONAL.OBSERVED, i) for i in SEASONAL.OBSERVED.walk() if '.' in i and 'ERSSTV5' not in i]
    args = [(predictand_domain, attrs[i]) for i in range(len(attrs))]
    return args

@pytest.mark.observed
@pytest.mark.SEASONAL
@pytest.mark.parametrize('predictand_domain,entry', make_obs_args())
def test_seasonal_obs(predictand_domain, entry):
    entry.observations(predictand_domain, target='Jun-Sep')