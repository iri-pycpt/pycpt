import cptdl as dl
import datetime as dt
import os
import pathlib
import pycpt
import pytest
import socket
import subprocess
import tempfile
import time


HERE = pathlib.Path(__file__).absolute().parent

@pytest.fixture(scope="module", autouse=True)
def recording_proxy(request):
    port = 8080
    cmd = [
        "python",
        HERE / "recording_proxy.py",
        "--port", str(port),
        "--dir", HERE / "data/responses"
    ]
    if request.config.getoption("--record"):
        cmd += ["--record"]
    proxy_proc = subprocess.Popen(cmd)

    # Wait for the proxy to start accepting connections
    for i in range(10):
        try:
            sock = socket.socket()
            sock.connect(('127.0.0.1', port))
            break
        except ConnectionRefusedError:
            pass
        finally:
            sock.close()
        time.sleep(.1)
    else:
        raise Exception('Failed to connect to proxy')

    if proxy_proc.poll() is not None:
        raise Exception('recording proxy failed to start')

    # Even though the proxy only handles http for now, intercept
    # both http and https so that tests that accidentally use https
    # don't silently go around the proxy.
    previous_http_proxy = os.environ.get("http_proxy")
    os.environ["http_proxy"] = f"http://127.0.0.1:{port}"
    previous_https_proxy = os.environ.get("https_proxy")
    os.environ["https_proxy"] = f"http://127.0.0.1:{port}"

    yield

    proxy_proc.terminate()
    proxy_proc.wait()

    if previous_http_proxy is None:
        del os.environ["http_proxy"]
    else:
        os.environ["http_proxy"] = previous_http_proxy
    if previous_https_proxy is None:
        del os.environ["https_proxy"]
    else:
        os.environ["https_proxy"] = previous_https_proxy


reference_dir = pathlib.Path(__file__).absolute().parent / 'data/downloads'

default_download_args = {
    'fdate':  dt.datetime(2020, 5, 1),
    'target_first_year': 2005,
    'target_final_year': 2007,
    'target': 'Jun-Jul',
    'predictor_extent': {
        'west': -76,
        'east':  -74,
        'south': 39,
        'north': 41,
      },
    'predictand_extent': {
        'west': -76,
        'east':  -74,
        'south': 39,
        'north': 41,
      },
    'pressure': 200,
}

default_obs_request = {
    'season_years': (2005, 2007),
    'season_start_month': 6,
    'season_length': 2,
    # Need force=True because we're using NamedTemporaryFile,
    # which creates the file before we write to it.
    'force': True,
}


forecast_overrides = {
    'CanCM3': {
        'fdate': dt.datetime(2019, 5, 1),
    },
    'CanCM4': {
        'fdate': dt.datetime(2019, 5, 1),
    },
    'CanSIPSIC3': {
        'fdate': dt.datetime(2022, 5, 1),
    },
    'CanSIPSIC4': {
        'fdate': dt.datetime(2024, 8, 1),
        'target': 'Sep-Oct',
    },
    'CPS2': {
        # CPS2 has 2.5 degree resolution and some missing values.
        'predictor_extent': {'west': -80, 'east': -75, 'south': 35, 'north': 40},
        'fdate': dt.datetime(2021, 5, 1),
    },
    'GCFS2p0': {
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'GCFS2p1': {
        'fdate': dt.datetime(2021, 5, 1),
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'GEM5NEMO': {
        'fdate': dt.datetime(2022, 5, 1),
    },
    'GLOSEA5': {
        'fdate': dt.datetime(2020, 5, 1),
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'GLOSEA6': {
        'fdate': dt.datetime(2021, 5, 1),
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'METEOFRANCE7': {
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'METEOFRANCE8': {
        'fdate': dt.datetime(2022, 5, 1),
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'METEOFRANCE9': {
        'fdate': dt.datetime(2025, 4, 1),
        'target': 'May-Jun',
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'SEAS51': {
        'fdate': dt.datetime(2023, 5, 1),
    },
    'SEAS51b': {
        'fdate': dt.datetime(2023, 5, 1),
        'predictor_extent': {'west': -76, 'east': -74, 'south': 39.5, 'north': 41.5},
    },
    'SEAS51c': {
        'fdate': dt.datetime(2023, 5, 1),
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'SPEAR': {
        'fdate': dt.datetime(2021, 5, 1),
    },
    'SPEARb': {
        'fdate': dt.datetime(2021, 5, 1),
    },
    'SPSv3p0': {
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
    'SPSv3p5': {
        'fdate': dt.datetime(2021, 5, 1),
        'predictor_extent': {'west': -76.5, 'east': -74.5, 'south': 39.5, 'north': 41.5},
    },
}

obs_overrides = {
    # For some reason the UCSB variables are all 0.05 degree, except PRCP where
    # UCSB.PRCP is 0.25 degrees and the 0.05 degree variables is called UCBP0p05.PRCP.
    'UCSB': {
        'predictand_extent': {
            'west': -75.475,
            'east':  -75.375,
            'south': 40.875,
            'north': 40.975,
        },
    },
    'UCSB.PRCP': {
        'predictand_extent': {
            'west': -75.875,
            'east':  -75.375,
            'south': 40.375,
            'north': 40.875,
        },
    },
    'UCSB0p05.PRCP': {
        'predictand_extent': {
            'west': -75.475,
            'east':  -75.375,
            'south': 40.875,
            'north': 40.975,
        },
    },
    'CPCCMAPURD': {
        'predictand_extent': {
            'west': 0,
            'east':  2,
            'south': 40,
            'north': 42,
        },
    },
    'TRMM': {
        'predictand_extent': {
            'west': -75,
            'east':  -72,
            'south': 40,
            'north': 43,
        },
    },
}

@pytest.mark.parametrize('var', dl.hindcasts.keys())
def test_hindcast_download(var):
    # Proxy doesn't support HTTPS, and all the servers we're testing
    # support HTTP, so force HTTP.
    url = dl.hindcasts[var].replace('https', 'http')
    do_one(var, url, 'hindcast', 3, forecast_overrides)


@pytest.mark.parametrize('var', dl.forecasts.keys())
def test_forecast_download(var):
    url = dl.forecasts[var].replace('https', 'http')
    do_one(var, url, 'forecast', 1, forecast_overrides)

obs_to_test = {
    k: v
    for k, v in dl.observations.items()
    # Omit private datasets. We plan not to add any more
    # (private datasets are now added to the catalog in the
    # notebook rather than in catalog.py), so we just have to
    # omit those that have already been added.
    if not (
        k.startswith('ENACTS') or
        k.startswith('CHILE')
    )
}

@pytest.mark.parametrize('var', obs_to_test.keys())
def test_obs_download(var):
    url = dl.observations[var].replace('https', 'http')
    do_one(var, url, 'obs', 3, obs_overrides)

def do_one(var, url, basename, expected_len, overrides):
    model, _ = var.split('.')
    override = overrides.get(var, overrides.get(model, {}))
    args = default_download_args | override
    args = pycpt.notebook._preprocess_download_args(args)

    with tempfile.NamedTemporaryFile() as f:
        ds = dl.download(
            url,
            f.name,
            verbose=True,
            **args
        )
        print(ds)
        assert len(ds['T']) == expected_len
        # Some grids are on the integer degrees, so three points
        # get selected. Others are on the half degree, so two get
        # selected.
        assert len(ds['Y']) == 3
        assert len(ds['X']) == 3
