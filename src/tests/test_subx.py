import pytest 
from .. import * 

def make_subx_args(model):
    predictor_domain = Geo(20,50, -110, -70)
    attrs = [recursive_getattr(model, i) for i in model.walk() ]
    args = [(predictor_domain, attrs[i]) for i in range(len(attrs))]
    return args

def make_subx_obs_args(model):
    predictor_domain = Geo(8,25, 70, 90)
    attrs = [recursive_getattr(model, i) for i in model.walk() ]
    obs = ['CPC', 'CHIRPS', 'TRMM', 'IMD1deg', 'IMDp25deg']
    args = [(predictor_domain, attrs[0], obs[i]) for i in range(len(obs))]
    return args


@pytest.mark.SUBX
@pytest.mark.SUBX_CCSM4
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.CCSM4))
def test_subx_CSM4_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_CCSM4
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.CCSM4))
def test_subx_CSM4_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_CCSM4
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.CCSM4))
def test_subx_CSM4_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_CFSv2
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.CFSv2))
def test_subx_FSv2_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_CFSv2
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.CFSv2))
def test_subx_FSv2_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_CFSv2
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.CFSv2))
def test_subx_FSv2_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_FIMr1p1
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.FIMr1p1))
def test_subx_r1p1_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_FIMr1p1
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.FIMr1p1))
def test_subx_r1p1_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_FIMr1p1
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.FIMr1p1))
def test_subx_r1p1_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEFSv12
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEFSv12))
def test_subx_Sv12_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEFSv12
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEFSv12))
def test_subx_Sv12_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEFSv12
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.GEFSv12))
def test_subx_Sv12_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEOS2p1
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEOS2p1))
def test_subx_S2p1_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEOS2p1
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEOS2p1))
def test_subx_S2p1_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEOS2p1
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.GEOS2p1))
def test_subx_S2p1_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEPS6
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEPS6))
def test_subx_EPS6_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEPS6
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEPS6))
def test_subx_EPS6_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEPS6
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.GEPS6))
def test_subx_EPS6_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEPS7
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEPS7))
def test_subx_EPS7_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEPS7
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.GEPS7))
def test_subx_EPS7_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_GEPS7
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.GEPS7))
def test_subx_EPS7_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_NESM
@pytest.mark.hindcast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.NESM))
def test_subx_NESM_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_NESM
@pytest.mark.forecast 
@pytest.mark.parametrize('predictor_domain,entry', make_subx_args(SUBSEASONAL.SUBX.NESM))
def test_subx_NESM_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='week12')

@pytest.mark.SUBX
@pytest.mark.SUBX_NESM
@pytest.mark.observed
@pytest.mark.parametrize('predictor_domain,entry,obs', make_subx_obs_args(SUBSEASONAL.SUBX.NESM))
def test_subx_NESM_obs(predictor_domain, entry,obs):
    entry.observations(predictor_domain, obs=obs, target='week12')
