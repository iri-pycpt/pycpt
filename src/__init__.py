from .fileio import open_cptdataset, open_gradsdataset, to_cptv10
from .utilities import setup_dlauth, read_dlauth, rmrf, rmstar, ls_files_recursive, threeletters, download, recursive_getattr
from .datastructures import GeographicExtent
from .drivers import SeasonalDriver, SeasonalObsDriver, SubxDriver
from .tests import * 


from pathlib import Path
import intake 
from git import Repo 
import warnings 

__version__ = "1.0.0"
__author__  = "Kyle Hall (kjhall@iri.columbia.edu)"
__license__ = "MIT"


intake.register_driver( 'seasonaldriver', SeasonalDriver)
intake.register_driver( 'seasonalobsdriver', SeasonalObsDriver)
intake.register_driver( 'subxdriver', SubxDriver)

CPTTOOLS_SPACE = Path().home().absolute() / '.cpttools_space'
if not CPTTOOLS_SPACE.is_dir():
    CPTTOOLS_SPACE.mkdir(exist_ok=True, parents=True)
assert CPTTOOLS_SPACE.is_dir(), 'could not create ~/.cpttools_space directory'

if (CPTTOOLS_SPACE / 'data_catalog' / 'catalog.yml').is_file():
    catalog = intake.open_catalog( CPTTOOLS_SPACE / 'data_catalog' / 'catalog.yml')
    SEASONAL = catalog.seasonal
    SUBSEASONAL = catalog.subseasonal
else: 
    print('No data catalog found - please try updating with cpttools.update_catalog()')

def update_catalog():
    try:
        global catalog, SEASONAL, SUBSEASONAL
        if Path( CPTTOOLS_SPACE / 'new_data_catalog').is_dir():
            rmrf(CPTTOOLS_SPACE / 'new_data_catalog')
        Repo.clone_from('https://github.com/kjhall-iri/data_catalog.git', CPTTOOLS_SPACE / 'new_data_catalog') 

        if Path( CPTTOOLS_SPACE / 'new_data_catalog').is_dir():
            if Path(CPTTOOLS_SPACE / 'data_catalog').is_dir():
                rmrf( CPTTOOLS_SPACE / 'data_catalog')
            Path(CPTTOOLS_SPACE / 'new_data_catalog').rename(Path(CPTTOOLS_SPACE / 'data_catalog'))
        else: 
            warnings.warn('Clone of new data catalog failed - defaulting to existing')

        catalog = intake.open_catalog( CPTTOOLS_SPACE / 'data_catalog' / 'catalog.yml')
        SEASONAL = catalog.seasonal
        SUBSEASONAL = catalog.subseasonal
    except: 
        print('FAILED TO LOAD FRESH CATALOG - Data Library downloads may be unavailable')
    
