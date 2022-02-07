import pytest 
from .. import * 

def make_args(model):
    predictor_domain = GeographicExtent(20,50, -110, -70)
    attrs = [recursive_getattr(model, i) for i in model.walk() ]
    args = [(predictor_domain, attrs[i]) for i in range(len(attrs))]
    return args


@pytest.mark.SEASONAL
@pytest.mark.NMME_CANCM4I
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanCM4i))
def test_cancm4i_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.NMME_CANCM4I
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanCM4i))
def test_cancm4i_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')


@pytest.mark.SEASONAL
@pytest.mark.NMME_CANCM3
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanCM3))
def test_cancm3_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_CANCM3
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanCM3))
def test_cancm3_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')


@pytest.mark.SEASONAL
@pytest.mark.NMME_CANCM4
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanCM4))
def test_cancm4_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_CANCM4
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanCM4))
def test_cancm4_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_GEMNEMO
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.GEMNEMO))
def test_gemnemo_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_GEMNEMO
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.GEMNEMO))
def test_gemnemo_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')


@pytest.mark.SEASONAL
@pytest.mark.NMME_CANSIPSV2
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanSIPSv2))
def test_gemnemo_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_CANSIPSV2
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CanSIPSv2))
def test_cansipsv2_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')


@pytest.mark.SEASONAL
@pytest.mark.NMME_CCSM4
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CCSM4))
def test_cansipsv2_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_CCSM4
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CCSM4))
def test_ccsm4_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')


@pytest.mark.SEASONAL
@pytest.mark.NMME_AER04
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.AER04))
def test_aer04_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_AER04
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.AER04))
def test_aer04_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_FLORA06
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.FLORA06))
def test_flora06_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_FLORA06
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.FLORA06))
def test_flora06_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_FLORB01
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.FLORB01))
def test_florb01_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_FLORB01
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.FLORB01))
def test_florb01_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_SPEAR
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.SPEAR))
def test_spear_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_SPEAR
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.SPEAR))
def test_spear_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_GEOSS2S
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.GEOSS2S))
def test_geoss2s_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_GEOSS2S
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.GEOSS2S))
def test_gesos2s_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_CFSV2
@pytest.mark.hindcast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CFSv2))
def test_cfsv2_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Dec-Jan')

@pytest.mark.SEASONAL
@pytest.mark.NMME_CFSV2
@pytest.mark.forecast
@pytest.mark.NMME
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.NMME.CFSv2))
def test_cfsv2_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Dec-Jan')