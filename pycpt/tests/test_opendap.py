import numpy as np
import pytest
import xarray as xr

from pycpt.download.interface import BoundingBox
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
