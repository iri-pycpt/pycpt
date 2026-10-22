import pathlib
import warnings
from collections.abc import Sequence

import numpy as np
import pandas as pd
import pytest
import recording_proxy
import xarray as xr

from pycpt.download import (
    BoundingBox,
    SeasonalForecastRequest,
    SeasonalObsRequest,
    SeasonalObsSource,
    catalog,
)

from .utils import MockForecastSource, MockObsSource, force_http

HERE = pathlib.Path(__file__).absolute().parent
DATA_DIR = HERE.parent / 'data' / 'responses'

@pytest.fixture(scope="module", autouse=True)
def proxy(request):
    record_opt = request.config.getoption('--record')
    if record_opt == 'new':
        mode = recording_proxy.Mode.record_new
    elif record_opt == 'all':
        mode = recording_proxy.Mode.record_all
    elif record_opt == None:
        mode = recording_proxy.Mode.frozen
    else:
        raise ValueError(f'Unknown recording mode {record_opt}')
    with recording_proxy.recording_proxy(
            str(DATA_DIR),
            mode=mode,
            verbose=True,
    ):
        yield


default_forecast_request = {
    'season_start_month': 6,
    'season_start_years': (2005, 2007),
    'season_start_lead': 1,
    'season_length': 2,
    'bounding_box': BoundingBox(west=-76, east=-74, south=39, north=41),
}

forecast_overrides: dict[str, Sequence[dict]]  = {
    'CanCM3': [
        {},
        {'season_start_years': (2019, 2019)},
    ],
    'CanCM4': [
        {},
        {'season_start_years': (2019, 2019)}
    ],
    'CanSIPSIC3': [
        {},
        {'season_start_years': (2022, 2022)},
    ],
    'CanSIPSIC4': [
        {'season_start_month': 9},
        {'season_start_month': 9, 'season_start_years': (2024, 2024)},
    ],
    'CPS2': [
        {'bounding_box': BoundingBox(west=-80, east=-75, south=35, north=40)},
        {'season_start_years': (2021, 2021), 'bounding_box': BoundingBox(west=-80, east=-75, south=35, north=40)},
    ],
    'GCFS2p0': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'GCFS2p1': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2021, 2021), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'GEM5NEMO': [
        {},
        {'season_start_years': (2022, 2022)},
    ],
    'GLOSEA5': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'GLOSEA6': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2021, 2021), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'METEOFRANCE7': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'METEOFRANCE8': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2022, 2022), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'METEOFRANCE9': [
        {'season_start_month': 5, 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_month': 5, 'season_start_years': (2025, 2025), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'SEAS51': [
        {},
        {'season_start_years': (2023, 2023)}
    ],
    'SEAS51b': [
        {'bounding_box': BoundingBox(west=-76, east=-74, south=39.5, north=41.5)},
        {'season_start_years': (2023, 2023), 'bounding_box': BoundingBox(west=-76, east=-74, south=39.5, north=41.5)},
    ],
    'SEAS51c': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2023, 2023), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'SPEAR': [
        {},
        {'season_start_years': (2021, 2021)},
    ],
    'SPEARb': [
        {},
        {'season_start_years': (2021, 2021)},
    ],
    'SPSv3p0': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2020, 2020), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
    'SPSv3p5': [
        {'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
        {'season_start_years': (2021, 2021), 'bounding_box': BoundingBox(west=-76.5, east=-74.5, south=39.5, north=41.5)},
    ],
}


obs_requests: dict[str, SeasonalObsRequest] = {
    # For some reason the UCSB variables are all 0.05 degree, except PRCP where
    # UCSB.PRCP is 0.25 degrees and the 0.05 degree variable is called UCBP0p05.PRCP.
    'UCSB': SeasonalObsRequest(
        season_start_years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75.475, east=-75.375, south=40.875, north=40.975),
    ),
    'UCSB.PRCP': SeasonalObsRequest(
        season_start_years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75.875, east=-75.375, south=40.375, north=40.875),
    ),
    'UCSB0p05.PRCP': SeasonalObsRequest(
        season_start_years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75.475, east=-75.375, south=40.875, north=40.975),
    ),
    'CPCCMAPURD': SeasonalObsRequest(
        season_start_years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=0, east=2, south=40, north=42),
    ),
    'TRMM': SeasonalObsRequest(
        season_start_years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75, east=-72, south=40, north=43),
    ),
    'MOCK_OBS': SeasonalObsRequest(
        season_start_years=(2005, 2007), season_start_month=6, season_length=2,
        bounding_box=BoundingBox(west=-75, east=-72, south=40, north=43),
    ),
}

default_cases = [
    {},
    {'season_start_years': (2020, 2020)}
]

forecast_test_cases = [
    (force_http(catalog.forecast_source(var, 200)), overrides)
    for var in catalog.list_forecast_sources()
    # Omit private datasets. We plan not to add any more
    # (private datasets are now added to the catalog in the
    # notebook rather than in catalog.py), so we just have to
    # omit those that have already been added.
    if not (
        var.startswith(('ENACTS', 'CHILE'))
    )
    for overrides in forecast_overrides.get(var.split('.')[0], default_cases)
] + [
    # Run the same kind of test on the mock as we do on the real ones,
    # to ensure that the mock is a correct stand-in.
    (MockForecastSource('MOCK_FORECAST.PRCP'), {}),
]

obs_to_test = {
    var: force_http(catalog.observation_source(var, None))
    for var in catalog.list_observation_sources()
    # Omit private datasets. We plan not to add any more
    # (private datasets are now added to the catalog in the
    # notebook rather than in catalog.py), so we just have to
    # omit those that have already been added.
    if not (
        var.startswith(('ENACTS', 'CHILE'))
    )
} | {
    'MOCK_OBS.PRCP': MockObsSource('MOCK_OBS.PRCP')
}


@pytest.mark.parametrize('case_', forecast_test_cases, ids=lambda x: x[0].name)
def test_forecast_download(case_, monkeypatch):
    source, overrides = case_
    request = SeasonalForecastRequest(**default_forecast_request | overrides)
    with warnings.catch_warnings():
        warnings.filterwarnings('ignore', category=DeprecationWarning)
        da = source.download(request)
    validate_forecast_response(request, da)
    assert len(da['T']) == len(range(*request.season_start_years)) + 1
    assert len(da['Y']) == 3
    assert len(da['X']) == 3


@pytest.mark.parametrize('var', obs_to_test.keys())
def test_obs_download(var: str, monkeypatch: pytest.MonkeyPatch) -> None:
    source: SeasonalObsSource = obs_to_test[var]
    model, _ = source.name.split('.')
    request = obs_requests.get(var)
    if request is None:
        request = obs_requests[model]
    da = source.download(request)
    print(da)
    validate_obs_response(request, da)
    assert len(da['T']) == 3
    assert len(da['Y']) == 3
    assert len(da['X']) == 3


def validate_forecast_response(request: SeasonalForecastRequest, da: xr.DataArray):
    assert da.dims == ('T', 'Y', 'X')

    init_first_year, init_final_year = request.init_years
    assert ((init_first_year <= da['S'].dt.year) &
            (da['S'].dt.year <= init_final_year)).all()
    assert (da['S'].dt.month == request.init_month).all()
    assert (da['S'].dt.day == 1).all()

    season_starts = pd.to_datetime(da['S'].data) + pd.DateOffset(months=request.season_start_lead)
    np.testing.assert_array_equal(da['Ti'], season_starts)
    season_ends = season_starts + pd.DateOffset(months=request.season_length)
    # Adjust to use PyCPT's systematically incorrect season end time
    # TODO fix the bug everywhere in PyCPT
    season_ends = season_ends - pd.Timedelta(hours=24)
    assert (da['Tf'] == season_ends).all()

    validate_bounding_box(request.bounding_box, da)

    # Not asserting anything about pressure_level, because the original Ingrid implementation
    # didn't take care to leave that information in a consistently-named attribute. If
    # we eventually use this library outside of PyCPT, we should to improve on that.


def validate_obs_response(request: SeasonalObsRequest, da: xr.DataArray):
    assert da.dims == ('T', 'Y', 'X')

    first_year, final_year = request.season_start_years
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

    validate_bounding_box(request.bounding_box, da)

    # See note about pressure_level above.


def validate_bounding_box(bounding_box: BoundingBox | None, da: xr.DataArray):
    if bounding_box is not None:
        # For historical reasons (PyCPT's original Ingrid URLs selected the
        # bounding box using RANGEEDGES), the download API selects any cell that
        # overlaps the requested bounding box, even if its center is outside the
        # bounding box. Therefore, the selected grid centers can be up to half a
        # cell width beyond the bounding box. If a dimension has fewer than two
        # grid points, we don't know its resolution, so we can't do this check.
        # TODO get resolution information before subsetting
        assert len(da['Y']) > 1, "Don't know how to validate response with length < 2"
        y_half_step = abs(da['Y'][0].item() - da['Y'][1].item())
        assert da['Y'].min().item() >= bounding_box['south'] - y_half_step
        assert da['Y'].max().item() <= bounding_box['north'] + y_half_step

        assert len(da['X']) > 1, "Don't know how to validate response with length < 2"
        x_half_step = abs(da['X'][0].item() - da['X'][1].item())
        assert da['X'].min().item() >= bounding_box['west'] - x_half_step
        assert da['X'].max().item() <= bounding_box['east'] + x_half_step
