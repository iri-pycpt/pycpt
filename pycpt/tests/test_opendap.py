import numpy as np
import pytest
import xarray as xr

from pycpt.download.interface import BoundingBox, SeasonalForecastRequest
from pycpt.download.opendap import (
    OpendapSeasonalForecastSource,
    OpendapSeasonalObsSource,
    normalize_bounding_box,
)


def test_forecast_sum():
    da = xr.DataArray(
        [[1, 2, 3],
         [4, 5, 6]],
        coords={
            'S': np.array(['2000-01-01', '2001-01-01'], dtype='datetime64[ns]'),
            'L': np.array([0, 1, 2]),
        }
    )
    ds = xr.Dataset({'v': da})
    ds = ds.assign_coords(target_bnds=(
        ('S', 'L', 'nbound'),
        np.array([[['2000-02-01', '2000-03-01'], ['2000-03-01', '2000-04-01'], ['2000-04-01', '2000-05-01']],
                  [['2001-02-01', '2001-03-01'], ['2001-03-01', '2001-04-01'], ['2001-04-01', '2001-05-01']]], dtype='datetime64[ns]')
    ))
    ds = OpendapSeasonalForecastSource.seasonal_value(ds, True)
    np.testing.assert_equal(ds['v'].values, np.array([6, 15]))


def test_forecast_mean():
    da = xr.DataArray(
        [[1, 2, 3],
         [4, 5, 6]],
        coords={
            'S': np.array(['2000-01-01', '2001-01-01'], dtype='datetime64[ns]'),
            'L': np.array([0, 1, 2]),
        }
    )
    ds = xr.Dataset({'v': da})
    ds = ds.assign_coords(target_bnds=(
        ('S', 'L', 'nbound'),
        np.array([[['2000-02-01', '2000-03-01'], ['2000-03-01', '2000-04-01'], ['2000-04-01', '2000-05-01']],
                  [['2001-02-01', '2001-03-01'], ['2001-03-01', '2001-04-01'], ['2001-04-01', '2001-05-01']]], dtype='datetime64[ns]')
    ))
    ds = OpendapSeasonalForecastSource.seasonal_value(ds, False)
    expected = np.array([
        (1 * 29 + 2 * 31 + 3 * 30) / (29 + 31 + 30),
        (4 * 28 + 5 * 31 + 6 * 30) / (28 + 31 + 30)
    ])
    np.testing.assert_allclose(ds['v'].values, expected)


def test_obs_sum():
    da = xr.DataArray(
        [1, 2, 3, 4, 5, 6],
        coords={
            'T': np.array(
                ['2000-01-01', '2000-02-01', '2000-03-01',
                 '2001-01-01', '2001-02-01', '2001-03-01'],
                dtype='datetime64[ns]' 
            ),
        }
    )
    ds = xr.Dataset({'v': da})
    ds = OpendapSeasonalObsSource.seasonal_value(ds, 1, True)
    print(ds)
    np.testing.assert_equal(ds['v'].values, np.array([6, 15]))

def test_obs_mean():
    da = xr.DataArray(
        [1, 2, 3, 4, 5, 6],
        coords={
            'T': np.array(
                ['2000-02-01', '2000-03-01', '2000-04-01',
                 '2001-02-01', '2001-03-01', '2001-04-01'],
                dtype='datetime64[ns]' 
            ),
        }
    )
    ds = xr.Dataset({'v': da})
    ds = OpendapSeasonalObsSource.seasonal_value(ds, 1, False)
    print(ds)
    expected = np.array([
        (1 * 29 + 2 * 31 + 3 * 30) / (29 + 31 + 30),
        (4 * 28 + 5 * 31 + 6 * 30) / (28 + 31 + 30)
    ])
    np.testing.assert_allclose(ds['v'].values, expected)

def make_forecast_ds(first_init_year, n_inits, member_values, n_leads=4):
    """Synthetic forecast Dataset with annual January initializations. Every
    grid point and lead of member m has value member_values[m].
    """
    S = np.array(
        [f'{first_init_year + i}-01-01' for i in range(n_inits)],
        dtype='datetime64[M]'
    )
    L = np.arange(n_leads)
    M = np.arange(len(member_values))
    Y = np.array([10., 11.])
    X = np.array([0., 1., 2.])
    target_start = S[:, None] + L[None, :].astype('timedelta64[M]')
    target_bnds = np.stack([target_start, target_start + np.timedelta64(1, 'M')], axis=-1)
    data = np.broadcast_to(
        np.array(member_values, dtype=float)[None, None, :, None, None],
        (len(S), len(L), len(M), len(Y), len(X)),
    )
    return xr.Dataset(
        {'prcp': (('S', 'L', 'M', 'Y', 'X'), data)},
        coords={
            'S': S.astype('datetime64[ns]'),
            'L': L,
            'M': M,
            'Y': Y,
            'X': X,
            'target_bnds': (('S', 'L', 'nbound'), target_bnds.astype('datetime64[ns]')),
        },
    )


def make_forecast_source(datasets, extensive_in_time=True):
    return OpendapSeasonalForecastSource(
        'test',
        {'urls': list(datasets), 'extensive_in_time': extensive_in_time},
        pressure_level=None,
        opener=datasets.__getitem__,
    )


def make_forecast_request(season_start_years, season_length=1):
    return SeasonalForecastRequest(
        season_start_month=2,
        season_length=season_length,
        season_start_years=season_start_years,
        bounding_box=None,
        season_start_lead=1,
    )


def test_forecast_download_ensemble_mean():
    source = make_forecast_source({
        'url': make_forecast_ds(2000, 2, member_values=[1., 3.]),
    })
    da = source.download(make_forecast_request((2000, 2001)))
    np.testing.assert_equal(da.values, 2.)


def test_forecast_download_extensive_sums_season():
    source = make_forecast_source({
        'url': make_forecast_ds(2000, 2, member_values=[2.]),
    })
    da = source.download(make_forecast_request((2000, 2001), season_length=3))
    np.testing.assert_equal(da.values, 6.)


def test_forecast_download_overlapping_urls_prefers_later():
    # Hindcast and forecast both contain the 2002 initialization; the forecast
    # (later url) should win.
    source = make_forecast_source({
        'hindcast': make_forecast_ds(2000, 3, member_values=[1.]),
        'forecast': make_forecast_ds(2002, 3, member_values=[10.]),
    })
    da = source.download(make_forecast_request((2000, 2004)))
    np.testing.assert_equal(
        da.transpose('T', ...).values[:, 0, 0],
        np.array([1., 1., 10., 10., 10.])
    )


def test_forecast_download_target_season_coords():
    # January initialization, lead 1, length 3: FMA season.
    source = make_forecast_source({
        'url': make_forecast_ds(2000, 2, member_values=[1.]),
    })
    da = source.download(make_forecast_request((2000, 2001), season_length=3))
    np.testing.assert_equal(
        da['Ti'].values,
        np.array(['2000-02-01', '2001-02-01'], dtype='datetime64[ns]')
    )
    # The end of the season is represented by its last day, not the day after.
    np.testing.assert_equal(
        da['Tf'].values,
        np.array(['2000-04-30', '2001-04-30'], dtype='datetime64[ns]')
    )


def make_bb(west, east):
    return BoundingBox(west=west, east=east, south=10, north=10)

def make_ds(x_vals):
    return xr.Dataset({
            'myvar': xr.DataArray(data=np.zeros(len(x_vals)), coords={'X': x_vals})
    })

def test_normalize_bounding_box_small_positive():
    normalized_bbs = normalize_bounding_box(
        make_ds(np.arange(0.5, 359.5, 1)),
        make_bb(10, 20),
    )
    assert len(normalized_bbs) == 1
    bb = normalized_bbs[0]
    assert bb is not None
    assert bb['west'] == 10
    assert bb['east'] == 20

def test_normalize_bounding_box_large_positive():
    normalized_bbs = normalize_bounding_box(
        make_ds(np.arange(0.5, 359.5, 1)),
        make_bb(340, 350)
    )
    assert len(normalized_bbs) == 1
    bb = normalized_bbs[0]
    assert bb is not None
    assert bb['west'] == 340
    assert bb['east'] == 350

def test_normalize_bounding_box_negative():
    normalized_bbs = normalize_bounding_box(
        make_ds(np.arange(0.5, 359.5, 1)),
        make_bb(-20, -10)
    )
    assert len(normalized_bbs) == 1
    bb = normalized_bbs[0]
    assert bb is not None
    assert bb['west'] == 340
    assert bb['east'] == 350

def test_normalize_bounding_box_split():
    normalized_bbs = normalize_bounding_box(
        make_ds(np.arange(0.5, 359.5, 1)),
        make_bb(-20, 10)
    )
    assert len(normalized_bbs) == 2
    bb0 = normalized_bbs[0]
    assert bb0 is not None
    assert bb0['west'] == 340
    assert bb0['east'] == 360
    bb1 = normalized_bbs[1]
    assert bb1 is not None
    assert bb1['west'] == 0
    assert bb1['east'] == 10

def test_normalize_bounding_box_wrong_order():
    with pytest.raises(ValueError):
        normalize_bounding_box(
            make_ds(np.arange(0.5, 359.5, 1)),
            make_bb(20, 10)
        )


# TODO: NaN handling; correctness of coordinates including aux coords; partial
# season (not implemented yet)
