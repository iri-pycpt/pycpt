import datetime as dt
import cptdl as dl
from pathlib import Path
import pandas as pd
import xarray as xr

def add_ti(ds):
    fname = Path(ds.encoding["source"]).name
    prefix = 'rr_mrg_'
    assert fname.startswith(prefix)
    pl = len(prefix)
    year = int(fname[pl : pl + 4])
    month = int(fname[pl + 4 : pl + 6])
    dekad = int(fname[pl + 6 : pl + 7])
    ti = dt.datetime(year, month, (dekad - 1) * 10 + 1)
    ds = ds.assign_coords(Ti=ti).expand_dims('Ti')
    return ds

def open_enacts_decadal(decadal_dir):
    ds = xr.open_mfdataset(f"{decadal_dir}/*.nc", preprocess=add_ti)
    ds = ds.rename(Lon='X', Lat='Y')
    # todo add Tf, T
    return ds

def decadal_to_monthly(ds, agg):
    assert agg == 'sum', "Sum is the only aggregation implemented so far"
    ds = ds.resample(Ti='1MS').sum()
    ti = ds['Ti'].to_pandas()
    tf = ti + pd.DateOffset(months=1, days=-1)
    t = ti + (tf - ti) / 2
    ds = ds.assign_coords(T=('Ti', t)).swap_dims(Ti='T')
    ds = ds.assign_coords(Tf=('T', tf))
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
    assert agg == 'sum', "Sum is the only aggregation implemented so far"
    months = month_range(first_month, last_month)

    ti = ds['T'].to_pandas().apply(lambda d: season_start(d, months))
    ds = ds.assign_coords(Ti=('T', ti))
    ds = ds.groupby('Ti').sum()
    ti = ds['Ti'].to_pandas()
    tf = ti + pd.DateOffset(months=len(months), days=-1)
    t = ti + (tf - ti) / 2
    ds = ds.assign_coords(T=('Ti', t)).swap_dims(Ti='T')
    ds = ds.assign_coords(Tf=('T', tf))

    if first_year is not None:
        ds = ds.sel(T=(ds['T'].dt.year >= first_year))
    if final_year is not None:
        ds = ds.sel(T=(ds['T'].dt.year <= final_year))

    return ds

    ds = open_enacts_decadal(decadal_dir)
    ds = open_enacts_monthly(decadal_dir)

def cmd_extract_seasonal(argv=None):
    import argparse
    import sys

    if argv is None:
        argv = sys.argv[1:]

    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-a', '--aggregation', required=True,
        help='Aggregation to use over the season. Currently only "sum" is supported.'
    )
    parser.add_argument('--first-year', type=int)
    parser.add_argument('--final-year', type=int)
    parser.add_argument('-v', '--verbose', action='store_true')
    parser.add_argument('indir')
    parser.add_argument('season')
    parser.add_argument('outfile')
    args = parser.parse_args(argv)

    first_month, last_month = dl.parse_target(args.season)

    decadal = open_enacts_decadal(args.indir)
    monthly = decadal_to_monthly(decadal, args.aggregation)
    seasonal = monthly_to_seasonal(
        monthly, first_month, last_month, 'sum', args.first_year, args.final_year
    )

    if args.verbose:
        print(seasonal)

    seasonal.to_netcdf(args.outfile)
