import pytest 
from .. import * 

def make_args(model):
    predictor_domain = Geo(20,50, -110, -70)
    attrs = [recursive_getattr(model, i) for i in model.walk() ]
    args = [(predictor_domain, attrs[i]) for i in range(len(attrs))]
    return args


@pytest.mark.SEASONAL
@pytest.mark.C3S_SPSV3P5
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.SPSv3p5))
def test_spsv3p5_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.C3S_SPSV3P5
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.SPSv3p5))
def test_spsv3p5_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')


@pytest.mark.SEASONAL
@pytest.mark.C3S_SPSV3P0
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.SPSv3p0))
def test_spsv3_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_SPSV3P0
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.SPSv3p0))
def test_spsv3_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')


@pytest.mark.SEASONAL
@pytest.mark.C3S_GCFS2P1
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GCFS2p1))
def test_gcfs2p1_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_GCFS2P1
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GCFS2p1))
def test_gcfs2p1_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_GCFS2P0
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GCFS2p0))
def test_gcfs2p0_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_GCFS2P0
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GCFS2p0))
def test_gcfs2p0_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')


@pytest.mark.SEASONAL
@pytest.mark.C3S_SEAS5
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.SEAS5))
def test_seas5_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_SEAS5
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.SEAS5))
def test_seas5_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')


@pytest.mark.SEASONAL
@pytest.mark.C3S_CPS2
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.CPS2))
def test_cps2_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_CPS2
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.CPS2))
def test_cps2_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')


@pytest.mark.SEASONAL
@pytest.mark.C3S_METEOFRANCE7
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.METEOFRANCE7))
def test_mf7_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_METEOFRANCE7
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.METEOFRANCE7))
def test_mf7_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_METEOFRANCE8
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.METEOFRANCE8))
def test_mf8_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_METEOFRANCE8
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.METEOFRANCE8))
def test_mf8_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_GLOSEA5
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GLOSEA5))
def test_glosea5_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_GLOSEA5
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.xfail
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GLOSEA5))
def test_glosea5_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_GLOSEA6
@pytest.mark.hindcast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GLOSEA6))
def test_glosea6_hcst(predictor_domain, entry):
    entry.hindcasts(predictor_domain, target='Jun-Sep')

@pytest.mark.SEASONAL
@pytest.mark.C3S_GLOSEA6
@pytest.mark.forecast
@pytest.mark.C3S
@pytest.mark.parametrize('predictor_domain,entry', make_args(SEASONAL.C3S.GLOSEA6))
def test_glosea6_fcst(predictor_domain, entry):
    entry.forecasts(predictor_domain, target='Jun-Sep')