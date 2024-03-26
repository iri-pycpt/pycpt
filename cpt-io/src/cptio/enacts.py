import datetime as dt
import numpy as np
from pathlib import Path
import pandas as pd
import xarray as xr

def add_t(ds):
    fname = Path(ds.encoding["source"]).name
    prefix = 'rr_mrg_'
    assert fname.startswith(prefix)
    pl = len(prefix)
    year = int(fname[pl : pl + 4])
    month = int(fname[pl + 4 : pl + 6])
    dekad = int(fname[pl + 6 : pl + 7])

    ti = pd.Timestamp(year, month, (dekad - 1) * 10 + 1)
    if dekad == 3:
        tf = pd.Timestamp(year, month, 1) + pd.DateOffset(months=1)
    else:
        tf = ti + pd.DateOffset(days=10)
    t = ti + (tf - ti) / 2
    ds = ds.assign_coords(T=t, Ti=ti, Tf=tf).expand_dims(['T'])
    return ds

def open_enacts_dekadal(dekadal_dir):
    ds = xr.open_mfdataset(f"{dekadal_dir}/*.nc", preprocess=add_t)
    assert len(ds.data_vars) == 1
    da = next(iter(ds.data_vars.values()))
    da = da.rename(Lon='X', Lat='Y')
    # CPT seems to round off coordinate values. If we don't round them
    # off ahead of time, then the forecasts that come back from CPT
    # don't align with the obs. Had this problem with the Lesotho
    # ENACTS; I don't know if other datasets have weird coordinate
    # values too or if they did something wrong in Lesotho.
    da['X'] = da['X'].round(6)
    da['Y'] = da['Y'].round(6)
    return da

def dekadal_to_monthly(ds, agg):
    groups = ds.resample(Ti='1MS')
    result = getattr(groups, agg)(skipna=False)
    return result

def dekadal_to_monthly_incorrect(ds, agg):
    '''Reproduces the often-used but inexact ingrid calculation "T
monthlyAverage 3 mul", which is only an approximation of the monthly
total because it weights dekads by their length.
    '''
    lengths = (ds['Tf'] - ds['Ti']).dt.days
    weighted = ds * lengths
    groups = weighted.resample(T='1MS')
    aggregated = getattr(groups, agg)(skipna=False)
    ds = (
        aggregated /
        lengths.resample(T='1MS').sum()
    ) * 3 # 3 dekads per month
    return ds

def month_range(first_month, last_month):
    m = first_month
    months = [m]
    while m != last_month:
        m = (m % 12) + 1
        months.append(m)
    return months

def season_start(date, season_months):
    """Returns the start of the season containing date,
 or None if it's outside the season."""
    if date.month in season_months:
        if date.month >= season_months[0]:
            start_year = date.year
        else:
            start_year = date.year - 1
        return dt.datetime(start_year, season_months[0], 1)
    return None

def monthly_to_seasonal(ds, first_month, last_month, agg, first_year=None, final_year=None):
    months = month_range(first_month, last_month)

    ti = ds['T'].to_pandas().apply(lambda d: season_start(d, months))
    ds = ds.assign_coords(Ti=('T', ti))
    groups = ds.groupby('Ti')
    ds = getattr(groups, agg)(skipna=False)
    ti = ds['Ti'].to_pandas()
    tf = ti + pd.DateOffset(months=len(months))
    t = ti + (tf - ti) / 2
    ds = ds.assign_coords(T=('Ti', t)).swap_dims(Ti='T')
    ds = ds.assign_coords(Tf=('T', tf))

    if first_year is not None:
        ds = ds.sel(T=(ds['T'].dt.year >= first_year))
    if final_year is not None:
        ds = ds.sel(T=(ds['T'].dt.year <= final_year))

    return ds

def satisfy_pycpt(da, missing="-999"):
    '''Make it a dataset that PyCPT can handle'''

    assert 'missing' in da.attrs
    assert 'units' in da.attrs

    da = da.copy()

    # To match a systematic error in pycpt, shift the end of the
    # season back by 24 hours, e.g. for MAM shift Tf from midnight on
    # June 1 to midnight on May 31. Also shift T back by 12 hours
    # accordingly.
    da['Tf'] = da['Tf'] - np.timedelta64(24, 'h')
    da['T'] = da['T'] - np.timedelta64(12, 'h')

    return da
