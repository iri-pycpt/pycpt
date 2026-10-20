import numpy as np
import pytest
import xarray as xr

from pycpt.download.interface import (
    BoundingBox,
    SeasonalForecastRequest,
    SeasonalObsRequest,
)
from pycpt.download.opendap import (
    OpendapSeasonalForecastSource,
    OpendapSeasonalObsSource,
)


Y = np.array([10., 11.])
X = np.array([0., 1., 2.])


def make_forecast_ds(init_years, values, n_leads=4, x=X):
    """Synthetic forecast Dataset with January initializations in init_years.
    values is broadcast to (S, L, M), and is the same at every grid point.
    """
    S = np.array([f'{year}-01' for year in init_years], dtype='datetime64[M]')
    L = np.arange(n_leads)
    values = np.asarray(values, dtype=float)
    n_members = values.shape[-1] if values.ndim > 0 else 1
    M = np.arange(n_members)
    target_start = S[:, None] + L[None, :].astype('timedelta64[M]')
    target_bnds = np.stack([target_start, target_start + np.timedelta64(1, 'M')], axis=-1)
    data = np.broadcast_to(
        np.broadcast_to(values, (len(S), len(L), len(M)))[..., None, None],
        (len(S), len(L), len(M), len(Y), len(x)),
    )
    return xr.Dataset(
        {'prcp': (('S', 'L', 'M', 'Y', 'X'), data)},
        coords={
            'S': S.astype('datetime64[ns]'),
            'L': L,
            'M': M,
            'Y': Y,
            'X': x,
            'target_bnds': (('S', 'L', 'nbound'), target_bnds.astype('datetime64[ns]')),
        },
    )


def make_obs_ds(months, values, x=X):
    """Synthetic monthly obs Dataset. months are 'YYYY-MM' strings; values has
    one entry per month, the same at every grid point.
    """
    T = np.array(months, dtype='datetime64[M]').astype('datetime64[ns]')
    data = np.broadcast_to(
        np.asarray(values, dtype=float)[:, None, None],
        (len(T), len(Y), len(x)),
    )
    return xr.Dataset(
        {'prcp': (('T', 'Y', 'X'), data)},
        coords={'T': T, 'Y': Y, 'X': x},
    )


def month_range(first, final):
    """'YYYY-MM' strings from first to final, inclusive."""
    return [
        str(m) for m in
        np.arange(np.datetime64(first), np.datetime64(final) + 1)
    ]


def make_forecast_source(datasets_by_url, extensive_in_time=True, pressure_level=None):
    return OpendapSeasonalForecastSource(
        'test',
        {'urls': list(datasets_by_url), 'extensive_in_time': extensive_in_time},
        pressure_level=pressure_level,
        opener=datasets_by_url.__getitem__,
    )


def make_obs_source(ds, extensive_in_time=True, pressure_level=None):
    return OpendapSeasonalObsSource(
        'test',
        {'url': 'url', 'extensive_in_time': extensive_in_time},
        pressure_level=pressure_level,
        opener={'url': ds}.__getitem__,
    )


def make_forecast_request(season_start_years, season_length=1, season_start_lead=1):
    """January initializations."""
    return SeasonalForecastRequest(
        season_start_month=1 + season_start_lead,
        season_length=season_length,
        season_start_years=season_start_years,
        bounding_box=None,
        season_start_lead=season_start_lead,
    )


def make_obs_request(season_start_years, season_start_month=2, season_length=3):
    return SeasonalObsRequest(
        season_start_month=season_start_month,
        season_length=season_length,
        season_start_years=season_start_years,
        bounding_box=None,
    )


def make_season_source(kind, month_values, extensive_in_time=True, x=X):
    """A source and a request whose result is a single season, Feb-Apr 2000,
    with the given monthly values. The data around the season is NaN, so using
    it by mistake makes the result NaN.
    """
    if kind == 'forecast':
        # January initialization, leads 1-3.
        lead_values = np.array([np.nan, *month_values])[:, None]
        source = make_forecast_source(
            {'url': make_forecast_ds([2000], lead_values, x=x)},
            extensive_in_time,
        )
        request = make_forecast_request((2000, 2000), season_length=3)
    else:
        values = np.full(12, np.nan)
        values[1:4] = month_values
        source = make_obs_source(
            make_obs_ds(month_range('2000-01', '2000-12'), values, x=x),
            extensive_in_time,
        )
        request = make_obs_request((2000, 2000))
    return source, request


# Both sources

KINDS = ['forecast', 'obs']


@pytest.mark.parametrize('kind', KINDS)
def test_download_extensive_sums_season(kind):
    source, request = make_season_source(kind, [1., 2., 4.], extensive_in_time=True)
    da = source.download(request)
    np.testing.assert_equal(da.values, 7.)


@pytest.mark.parametrize('kind', KINDS)
def test_download_intensive_weighted_mean(kind):
    source, request = make_season_source(kind, [1., 2., 4.], extensive_in_time=False)
    da = source.download(request)
    # Feb 2000 has 29 days, Mar 31, Apr 30.
    np.testing.assert_allclose(
        da.values, (1. * 29 + 2. * 31 + 4. * 30) / (29 + 31 + 30)
    )


@pytest.mark.parametrize('extensive_in_time', [True, False])
@pytest.mark.parametrize('kind', KINDS)
def test_download_nan_in_any_month_makes_season_nan(kind, extensive_in_time):
    source, request = make_season_source(kind, [1., np.nan, 4.], extensive_in_time)
    da = source.download(request)
    assert np.isnan(da.values).all()


@pytest.mark.parametrize('kind', KINDS)
def test_download_has_missing_attribute(kind):
    source, request = make_season_source(kind, [1., 2., 4.])
    da = source.download(request)
    assert 'missing' in da.attrs


@pytest.mark.parametrize('kind', KINDS)
def test_download_x_not_monotonic_raises(kind):
    source, request = make_season_source(kind, [1., 2., 4.], x=np.array([0., 2., 1.]))
    with pytest.raises(Exception, match='monotonic'):
        source.download(request)


@pytest.mark.parametrize('kind', KINDS)
def test_download_west_greater_than_east_raises(kind):
    source, request = make_season_source(kind, [1., 2., 4.])
    request.bounding_box = BoundingBox(west=2, east=1, south=10, north=11)
    with pytest.raises(ValueError):
        source.download(request)


# Forecast only

def test_forecast_download_ensemble_mean():
    source = make_forecast_source({
        'url': make_forecast_ds([2000, 2001], [1., 3.]),
    })
    da = source.download(make_forecast_request((2000, 2001)))
    np.testing.assert_equal(da.values, 2.)


def test_forecast_download_uses_only_season_leads():
    # Each lead's value is a different power of 10, so the sum shows which
    # leads were used.
    source = make_forecast_source({
        'url': make_forecast_ds([2000], (10. ** np.arange(6))[:, None], n_leads=6),
    })
    da = source.download(
        make_forecast_request((2000, 2000), season_length=2, season_start_lead=2)
    )
    np.testing.assert_equal(da.values, 100. + 1000.)


def test_forecast_download_overlapping_urls_prefers_later():
    # Hindcast and forecast both contain the 2002 initialization; the forecast
    # (later url) should win.
    source = make_forecast_source({
        'hindcast': make_forecast_ds(range(2000, 2003), [1.]),
        'forecast': make_forecast_ds(range(2002, 2005), [10.]),
    })
    da = source.download(make_forecast_request((2000, 2004)))
    np.testing.assert_equal(
        da.transpose('T', ...).values[:, 0, 0],
        np.array([1., 1., 10., 10., 10.])
    )


def test_forecast_download_concatenates_urls_in_order():
    source = make_forecast_source({
        'hindcast': make_forecast_ds([2000, 2001], [1.]),
        'forecast': make_forecast_ds([2002, 2003], [10.]),
    })
    da = source.download(make_forecast_request((2000, 2003)))
    np.testing.assert_equal(
        da['Ti'].values,
        np.array(['2000-02-01', '2001-02-01', '2002-02-01', '2003-02-01'],
                 dtype='datetime64[ns]')
    )
    np.testing.assert_equal(
        da.transpose('T', ...).values[:, 0, 0],
        np.array([1., 1., 10., 10.])
    )


def test_forecast_download_missing_initializations_left_out():
    source = make_forecast_source({
        'url': make_forecast_ds([2000, 2001, 2003], [1.]),
    })
    da = source.download(make_forecast_request((2000, 2003)))
    np.testing.assert_equal(
        da['Ti'].values,
        np.array(['2000-02-01', '2001-02-01', '2003-02-01'], dtype='datetime64[ns]')
    )
    assert not np.isnan(da.values).any()


def test_forecast_download_different_ensemble_sizes():
    source = make_forecast_source({
        'hindcast': make_forecast_ds([2000, 2001], [1., 3.]),
        'forecast': make_forecast_ds([2002, 2003], [10., 20., 30.]),
    })
    da = source.download(make_forecast_request((2000, 2003)))
    np.testing.assert_equal(
        da.transpose('T', ...).values[:, 0, 0],
        np.array([2., 2., 20., 20.])
    )


def test_forecast_download_selects_pressure_level():
    ds = xr.concat(
        [make_forecast_ds([2000, 2001], [1.]), make_forecast_ds([2000, 2001], [2.])],
        dim='P',
    ).assign_coords(P=[500., 850.])
    source = make_forecast_source({'url': ds}, pressure_level=850.)
    da = source.download(make_forecast_request((2000, 2001)))
    assert 'P' not in da.dims
    np.testing.assert_equal(da.values, 2.)


def test_forecast_download_pressure_level_ignored_without_p():
    source = make_forecast_source(
        {'url': make_forecast_ds([2000, 2001], [1.])},
        pressure_level=850.,
    )
    da = source.download(make_forecast_request((2000, 2001)))
    np.testing.assert_equal(da.values, 1.)


# Obs only

def test_obs_download_incomplete_season_at_end_removed():
    # The data ends in March 2002, so the Feb-Apr 2002 season is incomplete.
    months = month_range('2000-01', '2002-03')
    source = make_obs_source(make_obs_ds(months, np.ones(len(months))))
    da = source.download(make_obs_request((2000, 2002)))
    np.testing.assert_equal(
        da['Ti'].values,
        np.array(['2000-02-01', '2001-02-01'], dtype='datetime64[ns]')
    )


def test_obs_download_incomplete_season_in_middle_removed():
    # March 2001 is missing, so the Feb-Apr 2001 season is incomplete.
    months = [m for m in month_range('2000-01', '2002-12') if m != '2001-03']
    source = make_obs_source(make_obs_ds(months, np.ones(len(months))))
    da = source.download(make_obs_request((2000, 2002)))
    np.testing.assert_equal(
        da['Ti'].values,
        np.array(['2000-02-01', '2002-02-01'], dtype='datetime64[ns]')
    )


def test_obs_download_months_outside_season_not_used():
    months = month_range('2000-01', '2001-12')
    in_season = np.isin(np.array(months, dtype='datetime64[M]').astype(int) % 12, [1, 2, 3])
    values = np.where(in_season, 1., 1000.)
    source = make_obs_source(make_obs_ds(months, values))
    da = source.download(make_obs_request((2000, 2001)))
    np.testing.assert_equal(da.values, 3.)


def test_obs_download_selects_pressure_level():
    months = month_range('2000-01', '2000-12')
    ds = xr.concat(
        [make_obs_ds(months, np.ones(12)), make_obs_ds(months, np.full(12, 2.))],
        dim='P',
    ).assign_coords(P=[500., 850.])
    source = make_obs_source(ds, pressure_level=850.)
    da = source.download(make_obs_request((2000, 2000)))
    assert 'P' not in da.dims
    np.testing.assert_equal(da.values, 6.)


def test_obs_download_pressure_level_without_p_raises():
    months = month_range('2000-01', '2000-12')
    source = make_obs_source(make_obs_ds(months, np.ones(12)), pressure_level=850.)
    with pytest.raises(Exception, match="'P'"):
        source.download(make_obs_request((2000, 2000)))


# TODO: correctness of coordinates including aux coords
