from pathlib import Path 
import json, getpass, requests 
from urllib import parse 

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
        assert "This dataset has  bytes" in r.content.decode('utf-8'), 'Incorrect user or password?' 
        print('You have recorded your agreement to: the S2S terms and conditions / privacy policy\nT&C: https://iridl.ldeo.columbia.edu/auth/legal/terms-of-service\nPrivacy: https://iridl.ldeo.columbia.edu/auth/legal/privacy-policy')

def c3s_login(email):
    pswd = getpass.getpass('DLAUTH PASSWORD: ').strip()
    redirect ="/SOURCES/.EU/.Copernicus/.CDS/.C3S/.DWD/.GCFS2p1/.hindcast/.prcp/datafiles.html"
    with requests.Session() as s: 
        r = s.post("https://iridl.ldeo.columbia.edu/auth/login/local/submit/login", data={'email': email, 'password':pswd, 'redirect': redirect})
        assert "This dataset has  bytes" in r.content.decode('utf-8'), 'Incorrect user or password?' 
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

    