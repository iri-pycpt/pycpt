import pathlib, requests, sys
import datetime as dt 
from ..fileio import *
from .dlauth import read_dlauth
import xarray as xr
import numpy as np
import intake

def download(url, dest, verbose=True, format='cptv10.tsv', station=False, use_dlauth=True):
    assert format in ['cptv10.tsv', 'data.nc'], 'invalid download format: {}'.format(format)
    if verbose:
        print('\nURL: {}\n'.format(url))
        print()
    if str(use_dlauth) == 'True':
        dlauthid = read_dlauth()
        assert dlauthid is not None, "You need to use pycpt.setup_dlauth('your_iri_email') to get a cookie before you can download this data!"
        cookies = {'__dlauth_id': dlauthid}
    else:
        cookies={}
    with requests.post(url, stream=True, allow_redirects=True, cookies=cookies) as r:
        if r.status_code != 200:
            r.raise_for_status()  # Will only raise for 4xx codes, so...
            raise RuntimeError(f"Request to {url} returned status code {r.status_code}")
        #file_size = int(r.headers.get('Content-Length', 0))
        path = pathlib.Path(dest).expanduser().resolve()
        path.parent.mkdir(parents=True, exist_ok=True)
        start, j, total_size = dt.datetime.now(), 0, 0
        if verbose:
            print('DOWNLOADING: [' + ' ' * 25 + '] ({} KB) {}'.format(total_size // 1000, dt.datetime.now() - start), end='\r')
        with path.open("wb") as f:
            for chunk in r.iter_content(16*1024):
                j += 1
                total_size += sys.getsizeof(chunk)
                if verbose:
                    print('\rDOWNLOADING: [' + '*'*j + ' ' * (25 -j ) + '] ({} KB) {}'.format(total_size // 1000, dt.datetime.now() - start), end='\r')
                j = 0 if j >= 25 else j
                f.write(chunk)
        if verbose:
            print('DOWNLOADING: [' + '*' * 25 + '] ({} KB) {}'.format(total_size // 1000, dt.datetime.now() - start))
        try: 
            ds = open_cptdataset(path) if format == 'cptv10.tsv' else xr.open_dataset(path, decode_times=False)
            if format == 'data.nc':
                ds.S.attrs['calendar'] = '360_day'
                ds.S.attrs['units'] = 'months since 1960-01-01'
                ds = xr.decode_cf(ds)
        except Exception as e: 
            print('Failed to open downloaded cptv10.tsv file: {}'.format(e))
            assert False, "Please check what's downloaded from here, it may be a broken: {}".format(url)
        
        return path if format=='cptv10.tsv' else ds





json_types = [dict, list, str, int, float, bool]

def toJSON(obj):
    ret = {} 
    for key in vars(obj).keys(): 
        if type(vars(obj)[key]) in json_types:
            if type(vars(obj)[key]) == dict:
                dct = vars(obj)[key]
                if type(dct[dct.keys()[0]]) not in json_types:
                    for key1 in dct.keys():
                        dct[key1] = toJSON(dct[key])
                ret[key] = dct
            else: 
                ret[key] = vars(obj)[key]
        else: 
            ret[key] = toJSON(vars(obj)[key])
    return ret
    

def recursive_getattr(obj, attr):
    if '.' in attr:
        attrs = attr.split('.')
        next = '.'.join(attrs[1:])
        return recursive_getattr(getattr(obj, attrs[0]), next)
    else:
        return getattr(obj, attr)


def StandardNormalDeviateFromRank(X, x_sample_dim='T', x_feature_dim='M', x_lat_dim='Y', x_lon_dim='X'):
    ranked = X.rank(x_sample_dim, pct=True)
    mean = (X.quantile(0.75, x_sample_dim) + X.quantile(0.25, x_sample_dim))/2.0
    deviate =  (X.quantile(0.75, x_sample_dim) - X.quantile(0.25, x_sample_dim)) / 1.34898 # lazy version of / normal quantile.75 - normal quantil 0.25
    return mean, deviate

def greedy_ensemble_mean(dss, x_sample_dim='T', x_lat_dim='Y', x_lon_dim='X'):
    index, sample_size, lat_size, lon_size = 0, 0, 0, 0
    for i, ds in enumerate(dss):
        if type(ds) == xr.Dataset:
            dss[i] = getattr(ds, [i for i in ds.data_vars][0])
            ds = getattr(ds, [i for i in ds.data_vars][0])
        assert x_sample_dim in ds.dims, '{}th object missing {} dimension'.format(i+1, x_sample_dim)
        assert x_lat_dim in ds.dims, '{}th object missing {} dimension'.format(i+1, x_lat_dim)
        assert x_lon_dim in ds.dims, '{}th object missing {} dimension'.format(i+1, x_lon_dim)
        if i > 0:
            assert ds.shape[list(ds.dims).index(x_lat_dim)] == lat_size, 'All provided datasets must be the same spatial resolution'
            assert ds.shape[list(ds.dims).index(x_lon_dim)] == lon_size, 'All provided datasets must be the same spatial resolution'
        else:
            lat_size = ds.shape[list(ds.dims).index(x_lat_dim)]
            lon_size = ds.shape[list(ds.dims).index(x_lon_dim)]
        ds_sample_size = ds.shape[list(ds.dims).index(x_sample_dim)]
        index = i if ds_sample_size > sample_size else index
        sample_size = ds_sample_size if ds_sample_size > sample_size else sample_size
    ready = [dss[index]]
    for i, ds in enumerate(dss):
        if i != index: 
            ready.append(ds.reindex_like(ready[0]))
    return xr.concat(ready, 'M').mean('M')

def strict_ensemble_mean(dss, x_sample_dim='T', x_lat_dim='Y', x_lon_dim='X' ):
    index, sample_size, lat_size, lon_size = 0, 0, 0, 0
    for i, ds in enumerate(dss):
        if type(ds) == xr.Dataset:
            dss[i] = getattr(ds, [i for i in ds.data_vars][0])
            ds = getattr(ds, [i for i in ds.data_vars][0])
        assert x_sample_dim in ds.dims, '{}th object missing {} dimension'.format(i+1, x_sample_dim)
        assert x_lat_dim in ds.dims, '{}th object missing {} dimension'.format(i+1, x_lat_dim)
        assert x_lon_dim in ds.dims, '{}th object missing {} dimension'.format(i+1, x_lon_dim)
        if i > 0:
            assert ds.shape[list(ds.dims).index(x_lat_dim)] == lat_size, 'All provided datasets must be the same spatial resolution'
            assert ds.shape[list(ds.dims).index(x_lon_dim)] == lon_size, 'All provided datasets must be the same spatial resolution'
        else:
            lat_size = ds.shape[list(ds.dims).index(x_lat_dim)]
            lon_size = ds.shape[list(ds.dims).index(x_lon_dim)]
        ds_sample_size = ds.shape[list(ds.dims).index(x_sample_dim)]
        index = i if ds_sample_size > sample_size else index
        sample_size = ds_sample_size if ds_sample_size > sample_size else sample_size
    ready = dss[index]
    for i, ds in enumerate(dss):
        if i != index: 
            ready = ready + ds
    return ready / len(dss)





