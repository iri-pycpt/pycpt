import numpy as np
import os
import pandas as pd
import pathlib
import pytest
import socket
import subprocess
import time
from typing import Sequence
import warnings
import xarray as xr

from pycpt.download import (
    BoundingBox,
    SeasonalForecastRequest,
    SeasonalObsRequest,
    SeasonalObsSource,
    catalog,
)
from .utils import MockObsSource, MockForecastSource


HERE = pathlib.Path(__file__).absolute().parent
DATA_DIR = HERE.parent / 'data' / 'responses'

@pytest.fixture(scope="module", autouse=True)
def recording_proxy(request):
    port = 8080
    cmd = [
        "python",
        HERE / "recording_proxy.py",
        "--port", str(port),
        "--dir", DATA_DIR
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


default_forecast_request = {
    'init_month': 5,
    'init_years': (2005, 2007),
    'season_start_lead': 1,
    'season_length': 2,
    'bounding_box': BoundingBox(west=-76, east=-74, south=39, north=41),
    'pressure_level': 200
}

forecast_overrides: dict[str, Sequence[dict]]  = {
    'CanCM3': [
        {},
        {'init_years': (2019, 2019)},
    ],
    'CanCM4': [
        {},
        {'init_years': (2019, 2019)}
    ],
    'CanSIPSIC3': [
        {},
        {'init_years': (2022, 2022)},
    ],
    'CanSIPSIC4': [
        {'init_month': 8},
        {'init_month': 8, 'init_years': (2024, 2024)},
    ],
    'CPS2': [
        {'bounding_box': BoundingBox(west=-80, east=-75, south=35, north=40)},
        {'init_years': (2021, 2021), 'bounding_box': BoundingBox(west=-80, east=-75, south=35, north=40)},
    ],
    'GCFS2p0': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'GCFS2p1': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2021, 2021), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'GEM5NEMO': [
        {},
        {'init_years': (2022, 2022)},
    ],
    'GLOSEA5': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'GLOSEA6': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2021, 2021), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'METEOFRANCE7': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'METEOFRANCE8': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2022, 2022), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'METEOFRANCE9': [
        {'init_month': 4, 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_month': 4, 'init_years': (2025, 2025), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'SEAS51': [
        {},
        {'init_years': (2023, 2023)}
    ],
    'SEAS51b': [
        {'bounding_box': BoundingBox(west=-76, east=-74, south=39.5, north=41.5)},
        {'init_years': (2023, 2023), 'bounding_box': BoundingBox(west=-76, east=-74, south=39.5, north=41.5)},
    ],
    'SEAS51c': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2023, 2023), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'SPEAR': [
        {},
        {'init_years': (2021, 2021)},
    ],
    'SPEARb': [
        {},
        {'init_years': (2021, 2021)},
    ],
    'SPSv3p0': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'SPSv3p5': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'init_years': (2021, 2021), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
}


obs_requests: dict[str, SeasonalObsRequest] = {
    # For some reason the UCSB variables are all 0.05 degree, except PRCP where
    # UCSB.PRCP is 0.25 degrees and the 0.05 degree variable is called UCBP0p05.PRCP.
    'UCSB': SeasonalObsRequest(
        years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75.475, east=-75.375, south=40.875, north=40.975),
    ),
    'UCSB.PRCP': SeasonalObsRequest(
        years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75.875, east=-75.375, south=40.375, north=40.875),
    ),
    'UCSB0p05.PRCP': SeasonalObsRequest(
        years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75.475, east=-75.375, south=40.875, north=40.975),
    ),
    'CPCCMAPURD': SeasonalObsRequest(
        years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=0, east=2, south=40, north=42),
    ),
    'TRMM': SeasonalObsRequest(
        years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75, east=-72, south=40, north=43),
    ),
    'MOCK_OBS': SeasonalObsRequest(
        years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75, east=-72, south=40, north=43),
    ),
}

default_cases = [
    {},
    {'init_years': (2020, 2020)}
]

forecast_test_cases = [
    (source, overrides)
    for var, source in catalog.forecast_sources.items()
    # Omit private datasets. We plan not to add any more
    # (private datasets are now added to the catalog in the
    # notebook rather than in catalog.py), so we just have to
    # omit those that have already been added.
    if not (
        var.startswith('ENACTS') or
        var.startswith('CHILE')
    )
    for overrides in forecast_overrides.get(var.split('.')[0], default_cases)
] + [
    # Run the same kind of test on the mock as we do on the real ones,
    # to ensure that the mock is a correct stand-in.
    (MockForecastSource('MOCK_FORECAST.PRCP'), {}),
]

obs_to_test = {
    k: v
    for k, v in catalog.observation_sources.items()
    # Omit private datasets. We plan not to add any more
    # (private datasets are now added to the catalog in the
    # notebook rather than in catalog.py), so we just have to
    # omit those that have already been added.
    if not (
        k.startswith('ENACTS') or
        k.startswith('CHILE')
    )
} | {
    'MOCK_OBS.PRCP': MockObsSource('MOCK_OBS.PRCP')
}


@pytest.mark.parametrize('case_', forecast_test_cases, ids=lambda x: x[0].name)
def test_forecast_download(case_, monkeypatch):
    source, overrides = case_
    request = SeasonalForecastRequest(**default_forecast_request | overrides)
    if hasattr(source, 'force_http'):
        monkeypatch.setattr(source, 'force_http', True)
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', category=DeprecationWarning)
        da = source.download(request)
    validate_forecast_response(request, da)
    assert len(da['T']) == len(range(*request.init_years)) + 1
    assert len(da['Y']) == 3
    assert len(da['X']) == 3


@pytest.mark.parametrize('var', obs_to_test.keys())
def test_obs_download(var: str, monkeypatch: pytest.MonkeyPatch) -> None:
    source: SeasonalObsSource = obs_to_test[var]
    model, _ = source.name.split('.')
    request = obs_requests.get(var)
    if request is None:
        request = obs_requests[model]
    if hasattr(source, 'force_http'):
        monkeypatch.setattr(source, 'force_http', True)
    da = source.download(request)
    print(da)
    validate_obs_response(request, da)
    assert len(da['T']) == 3
    assert len(da['Y']) == 3
    assert len(da['X']) == 3


def validate_forecast_response(request: SeasonalForecastRequest, da: xr.DataArray):
    assert da.dims == ('T', 'Y', 'X')

    first_year, final_year = request.init_years
    assert ((first_year <= da['S'].dt.year) &
            (da['S'].dt.year <= final_year)).all()
    assert (da['S'].dt.month == request.init_month).all()
    assert (da['S'].dt.day == 1).all()

    season_starts = pd.to_datetime(da['S'].data) + pd.DateOffset(months=request.season_start_lead)
    np.testing.assert_array_equal(da['Ti'], season_starts)
    season_ends = season_starts + pd.DateOffset(months=request.season_length)
    # Adjust to use PyCPT's systematically incorrect season end time
    # TODO fix the bug everywhere in PyCPT
    season_ends = season_ends - pd.Timedelta(hours=24)
    assert (da['Tf'] == season_ends).all()

    if request.bounding_box is not None:
        # Some servers (e.g. Ingrid) will return any grid cell that intersects
        # the requested bounding box, even if its center is outside the bounding
        # box, so add half a grid cell to the limits. If a dimension has fewer than
        # two grid points, we don't know its resolution, so we can't do this check.
        if len(da['Y']) > 1:
            y_half_step = abs(da['Y'][0].item() - da['Y'][1].item())
            assert da['Y'].min().item() >= request.bounding_box['south'] - y_half_step
            assert da['Y'].max().item() <= request.bounding_box['north'] + y_half_step
        if len(da['X']) > 1:
            x_half_step = abs(da['X'][0].item() - da['X'][1].item())
            assert da['X'].min().item() >= request.bounding_box['west'] - x_half_step
            assert da['X'].max().item() <= request.bounding_box['east'] + x_half_step

    # Not asserting anything about pressure_level, because the original Ingrid implementation
    # didn't take care to leave that information in a consistently-named attribute. If
    # we eventually use this library outside of PyCPT, we should to improve on that.


def validate_obs_response(request: SeasonalObsRequest, da: xr.DataArray):
    assert da.dims == ('T', 'Y', 'X')

    first_year, final_year = request.years
    np.testing.assert_array_equal(da['Ti'].dt.year, range(first_year, final_year + 1))
    season_starts = np.array([
        np.datetime64(f'{y}-{request.season_start_month:02}-01')
        for y in range(first_year, final_year + 1)
    ])
    np.testing.assert_array_equal(da['Ti'].values, season_starts)
    season_ends = pd.to_datetime(season_starts) + pd.DateOffset(months=request.season_length)
    # Adjust to use PyCPT's systematically incorrect season end time
    # TODO fix the bug everywhere in PyCPT
    season_ends = season_ends - pd.Timedelta(hours=24)
    np.testing.assert_array_equal(da['Tf'].values, season_ends)

    if request.bounding_box is not None:
        # See note in validate_forecast_response
        if len(da['Y']) > 1:
            y_half_step = abs(da['Y'][0].item() - da['Y'][1].item())
            assert da['Y'].min().item() >= request.bounding_box['south'] - y_half_step
            assert da['Y'].max().item() <= request.bounding_box['north'] + y_half_step
        if len(da['X']) > 1:
            x_half_step = abs(da['X'][0].item() - da['X'][1].item())
            assert da['X'].min().item() >= request.bounding_box['west'] - x_half_step
            assert da['X'].max().item() <= request.bounding_box['east'] + x_half_step

    # See note about pressure_level above.
