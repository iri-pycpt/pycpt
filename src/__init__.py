from .fileio import open_cptdataset, to_cptv10
from .utilities import setup_dlauth, read_dlauth, s2s_login, c3s_login, rmrf, rmstar, ls_files_recursive, threeletters, download, recursive_getattr, seasonal_target_length, target_from_leads, leads_from_target
from .datastructures import Geo
from .drivers import SeasonalDriver, SeasonalObsDriver, SubxDriver

from pathlib import Path
import intake 
import warnings 
import zipfile , os , requests

__version__ = "0.2.0"
__author__  = "Kyle Hall (kjhall@iri.columbia.edu)"
__license__ = "MIT"


intake.register_driver( 'seasonaldriver', SeasonalDriver, overwrite=True)
intake.register_driver( 'seasonalobsdriver', SeasonalObsDriver, overwrite=True)
intake.register_driver( 'subxdriver', SubxDriver, overwrite=True)

CPTTOOLS_SPACE = Path().home().absolute() / '.cpttools_space'
if not CPTTOOLS_SPACE.is_dir():
    CPTTOOLS_SPACE.mkdir(exist_ok=True, parents=True)
assert CPTTOOLS_SPACE.is_dir(), 'could not create ~/.cpttools_space directory'


def update_catalog(version=None):
    if version is None: 
        url = "https://github.com/kjhall-iri/data_catalog/archive/refs/heads/main.zip"
    else:
        url = "https://github.com/kjhall-iri/data_catalog/archive/refs/tags/{}.zip".format(version)
    r = requests.get(url)
    assert r.status_code == 200, 'check this link: {}'.format(url)
    with open(str(CPTTOOLS_SPACE / 'zipfile.zip'), 'wb') as f: 
        f.write(r.content)
    
    #try:
    global catalog, SEASONAL, SUBSEASONAL
    if Path( CPTTOOLS_SPACE / 'new_data_catalog').is_dir():
        rmrf(CPTTOOLS_SPACE / 'new_data_catalog')
    
    (CPTTOOLS_SPACE / 'new_data_catalog').mkdir(exist_ok=True, parents=True)
    with zipfile.ZipFile(str(CPTTOOLS_SPACE / 'zipfile.zip')) as zf:
        zf.extractall( str(CPTTOOLS_SPACE / 'temp'))
    [i for i in (CPTTOOLS_SPACE / 'temp').glob('*')][0].rename(CPTTOOLS_SPACE / 'new_data_catalog')
    assert len([ i for i in (CPTTOOLS_SPACE / 'temp').glob('*')]) == 0, 'moving unpacked dc to new data catalog failed'
    if Path( CPTTOOLS_SPACE / 'new_data_catalog').is_dir():
        if Path(CPTTOOLS_SPACE / 'data_catalog').is_dir():
            rmrf( CPTTOOLS_SPACE / 'data_catalog')
        Path(CPTTOOLS_SPACE / 'new_data_catalog').rename(Path(CPTTOOLS_SPACE / 'data_catalog'))
    else: 
        warnings.warn('Clone of new data catalog failed - defaulting to existing')

    catalog = intake.open_catalog( CPTTOOLS_SPACE / 'data_catalog' / 'catalog.yml')
    SEASONAL = catalog.seasonal
    SUBSEASONAL = catalog.subseasonal
    #except: 
     #   print('FAILED TO LOAD FRESH CATALOG - Data Library downloads may be unavailable')

if (CPTTOOLS_SPACE / 'data_catalog' / 'catalog.yml').is_file():
    catalog = intake.open_catalog( CPTTOOLS_SPACE / 'data_catalog' / 'catalog.yml')
    SEASONAL = catalog.seasonal
    SUBSEASONAL = catalog.subseasonal
else:
    try: 
        update_catalog()
    except:     
        print('No data catalog found - please try updating with cpttools.update_catalog()')

from .scripts import load_c3s, load_nmme, load_observations
