from pathlib import Path 
import json, getpass, requests 

def read_dlauth():
    if not (Path.home().absolute() / '.pycpt_dlauth').is_file():
        print('You need to set up an IRIDLAUTH! Please set up an IRI account here (https://iridl.ldeo.columbia.edu/auth/signup), then use pycpt.setup_dlauth(["your_iri_login_email"]) to setup the cookie. This only needs to happen once!  ')
        return None
    else: 
        with open(str(Path.home().absolute() / '.pycpt_dlauth'), 'rb') as f: 
            auth_dict = json.loads(f.read())
        return auth_dict['key']

def setup_dlauth(email):
    if not (Path.home().absolute() / '.pycpt_dlauth').is_file():
        pswd = getpass.getpass()
        with requests.Session() as s:
            response = s.post("https://iridl.ldeo.columbia.edu/auth/login/local/submit/login", data={'email': email, 'password':pswd, "redirect": "https://iridl.ldeo.columbia.edu/auth"})
            response = s.get("https://iridl.ldeo.columbia.edu/auth/genkey")
            with open(str((Path.home().absolute() / '.pycpt_dlauth')), 'wb') as f: 
                f.write(response.content)
    else: 
        print('You already have an IRIDLAUTH (~/.pycpt_dlauth)!')
        print('Your key is {}'.format(read_dlauth()))

    