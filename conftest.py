from pathlib import Path

CARTOPY_DATA_DIR = Path(__file__).absolute().parent / 'pycpt' / 'tests' / 'data' / 'cartopy'


def pytest_configure(config):
    '''Make cartopy read shapefiles from, and download any missing ones
    to, a directory under version control, so that tests don't depend
    on the Natural Earth server and any new downloads show up in git
    status. See pycpt/tests/subset_natural_earth.py.

    cartopy.config is a process-wide dict with no thread-local
    override, so set it once here, before collection and before any
    test code can start threads, and never change it afterwards.'''
    import cartopy
    # Set both, so the user's CARTOPY_DATA_DIR (which sets
    # pre_existing_data_dir) can't hide a missing file.
    cartopy.config['pre_existing_data_dir'] = CARTOPY_DATA_DIR
    cartopy.config['data_dir'] = CARTOPY_DATA_DIR


def pytest_addoption(parser):
    # Add the custom command-line flag
    parser.addoption(
        "--record", action="store_true", default=False,
        help="Make real network requests and record the responses"
    )
