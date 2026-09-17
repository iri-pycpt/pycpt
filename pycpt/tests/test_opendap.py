import numpy as np
import xarray as xr

from pycpt.download.opendap import seasonal_forecast_value, seasonal_obs_value


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
    ds = seasonal_forecast_value(ds, True)
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
    ds = seasonal_forecast_value(ds, False)
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
    ds = seasonal_obs_value(ds, 1, True)
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
    ds = seasonal_obs_value(ds, 1, False)
    print(ds)
    expected = np.array([
        (1 * 29 + 2 * 31 + 3 * 30) / (29 + 31 + 30),
        (4 * 28 + 5 * 31 + 6 * 30) / (28 + 31 + 30)
    ])
    np.testing.assert_allclose(ds['v'].values, expected)
