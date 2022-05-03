import pathlib, requests, sys
import datetime as dt 
from ..fileio import *
from .dlauth import read_dlauth
import xarray as xr


def download(url, dest, verbose=True, format='data.nc', use_dlauth=True):
    assert format in ['cptv10.tsv', 'data.nc'], 'invalid download format: {}'.format(format)
    if verbose:
        print('\nURL: {}\n'.format(url))
        print()
    if str(use_dlauth) == 'True':
        dlauthid = read_dlauth()
        assert dlauthid is not None, "You need to use cpttools.setup_dlauth('your_email') to get a cookie before you can download this data!"
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
        except Exception as e: 
            print('Failed to open downloaded file: {}'.format(e))
            assert False, "Please check what's downloaded from here, it may be a broken: {}".format(url)
        return ds





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










