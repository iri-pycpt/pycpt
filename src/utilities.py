
from pathlib import Path 
import json, getpass, requests, sys
import datetime as dt 
import xarray as xr 
import cptio as cio 

def read_dlauth():
    if not (Path.home().absolute() / '.pycpt_dlauth').is_file():
        print('You need to set up an IRIDLAUTH! Please set up an IRI account here (https://iridl.ldeo.columbia.edu/auth/signup), then use pycpt.setup_dlauth(["your_iri_login_email"]) to setup the cookie. This only needs to happen once!  ')
        return None
    else: 
        with open(str(Path.home().absolute() / '.pycpt_dlauth'), 'rb') as f: 
            auth_dict = json.loads(f.read())
        return auth_dict['key']

def s2s_login(email):
    pswd = getpass.getpass('DLAUTH PASSWORD: ').strip()
    redirect = "/SOURCES/.ECMWF/.S2S/.NCEP/.forecast/.perturbed/.sfc_temperature/.skt/datafiles.html"
    with requests.Session() as s: 
        r = s.post("https://iridl.ldeo.columbia.edu/auth/login/local/submit/login", data={'email': email, 'password':pswd, 'redirect': redirect})
        assert "This dataset has bytes" in r.content.decode('utf-8'), 'Incorrect user or password?' 
        print('You have recorded your agreement to: the S2S terms and conditions / privacy policy\nT&C: https://iridl.ldeo.columbia.edu/auth/legal/terms-of-service\nPrivacy: https://iridl.ldeo.columbia.edu/auth/legal/privacy-policy')

def c3s_login(email):
    pswd = getpass.getpass('DLAUTH PASSWORD: ').strip()
    redirect ="/SOURCES/.EU/.Copernicus/.CDS/.C3S/.DWD/.GCFS2p1/.hindcast/.prcp/datafiles.html"
    with requests.Session() as s: 
        r = s.post("https://iridl.ldeo.columbia.edu/auth/login/local/submit/login", data={'email': email, 'password':pswd, 'redirect': redirect})
        assert "This dataset has bytes" in r.content.decode('utf-8'), 'Incorrect user or password?' 
        print('You have recorded your agreement to: the C3S terms and conditions / privacy policy\nT&C: https://iridl.ldeo.columbia.edu/auth/legal/terms-of-service\nPrivacy: https://iridl.ldeo.columbia.edu/auth/legal/privacy-policy')


def setup_dlauth(email):
    if not (Path.home().absolute() / '.pycpt_dlauth').is_file():
        pswd = getpass.getpass('DLAUTH PASSWORD: ').strip()
        with requests.Session() as s:
            response = s.post("https://iridl.ldeo.columbia.edu/auth/login/local/submit/login", data={'email': email, 'password':pswd, "redirect": "https://iridl.ldeo.columbia.edu/auth"})
            response = s.get("https://iridl.ldeo.columbia.edu/auth/genkey")
            with open(str((Path.home().absolute() / '.pycpt_dlauth')), 'wb') as f: 
                f.write(response.content)
    else: 
        print('You already have an IRIDLAUTH (~/.pycpt_dlauth)!')
        print('Your key is {}'.format(read_dlauth()))

class PyCPT_ERROR(Exception):
    pass    

def evaluate_url(url, **kwargs):
    from .targetleadconv import seasonal_target, seasonal_target_length_monthly, seasonal_target_length, threeletters
    locals().update(**kwargs)
    return eval('f"{}"'.format(url))


def simple_download(url, dest, verbose=False, use_dlauth=False):
    if verbose:
        print('URL: {}'.format(url))
        print()
    if str(use_dlauth) == 'True':
        dlauthid = read_dlauth()
        assert dlauthid is not None, "You need to use cpttools.setup_dlauth('your_email') to get a cookie before you can download this data! or, you can try setting use_dlauth=False"
        cookies = {'__dlauth_id': dlauthid}
    else:
        cookies={}
    with requests.post(url, stream=True, allow_redirects=True, cookies=cookies) as r:
        if r.status_code != 200:
            r.raise_for_status()  # Will only raise for 4xx codes, so...
            raise RuntimeError(f"Request to {url} returned status code {r.status_code}")
        #file_size = int(r.headers.get('Content-Length', 0))
        path = Path(dest).expanduser().resolve()
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
    assert path.is_file(), 'failed to write file'
    return path 


def download(baseurl, dest, verbose=False, format='cptv10.tsv', use_dlauth=True, **kwargs):
    assert format in ['cptv10.tsv', 'data.nc'], 'invalid download format: {}'.format(format)
    try:
        url = evaluate_url(baseurl, **kwargs)
    except:
        raise PyCPT_ERROR("You must pass all the required arguments for this URL as keyword arguments in .download(...). \n URL: {}\n ARGS: {}".format(baseurl, kwargs))

    path = simple_download(url, dest, verbose=verbose, use_dlauth=use_dlauth)
    
    try: 
        ds = cio.open_cptdataset(path) if format == 'cptv10.tsv' else xr.open_dataset(path, decode_times=False) 
    except Exception as e: 
            raise PyCPT_ERROR("Please check what's downloaded from here, it may be a broken: {}".format(url))
    return ds 

