
import pytest 
from .. import * 


def make_obs_args():
    #predictor_domain = GeographicExtent(20,50, -110, -70)
    predictand_domain = Geo(20,50, -110, -70)
    attrs = [recursive_getattr(SEASONAL.OBSERVED, i) for i in SEASONAL.OBSERVED.walk() if '.' in i and 'ERSSTV5' not in i and 'ENACTS' not in i]
    args = [(predictand_domain, attrs[i]) for i in range(len(attrs))]
    return args

def make_enacts_args():
    #predictor_domain = GeographicExtent(20,50, -110, -70)
    attrs = [recursive_getattr(SEASONAL.OBSERVED.ENACTS, i) for i in SEASONAL.OBSERVED.ENACTS.walk() if '.' in i ]
    args = [( attrs[i]) for i in range(len(attrs))]
    return args

@pytest.mark.observed
@pytest.mark.SEASONAL
@pytest.mark.parametrize('predictand_domain,entry', make_obs_args())
def test_seasonal_obs(predictand_domain, entry):
    entry.observations(predictand_domain, target='Jun-Sep')

@pytest.mark.ENACTS
@pytest.mark.observed
@pytest.mark.SEASONAL
@pytest.mark.parametrize('entry', make_enacts_args())
def test_enacts_seasonal_obs( entry):
        nsew = entry.catalog_object.describe()['metadata']['extent']
        geo = Geo(nsew[1], nsew[0], nsew[3], nsew[2])
        entry.observations(geo, target='Jun-Sep')