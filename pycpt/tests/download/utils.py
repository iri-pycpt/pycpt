import copy
import datetime

import numpy as np
import xarray as xr

import cptio
from pycpt.download.interface import (
    BoundingBox,
    SeasonalForecastRequest,
    SeasonalForecastSource,
    SeasonalObsRequest,
    SeasonalObsSource,
)


def encode_season(first_year: int, target_first_month: float, target_last_month: float) -> tuple[datetime.datetime, datetime.datetime, datetime.datetime]:
    if target_last_month >= target_first_month:
        end_year = first_year
    else:
        end_year = first_year + 1
    return cptio.fileio.read_cpt_date_range(f'{first_year}-{target_first_month}/{end_year}-{target_last_month}')

def generate_mock_data(
        var_name: str,
        years: tuple[int, int],
        season_start_month: int,
        season_length: int,
        bounding_box: BoundingBox,
) -> xr.DataArray | None:
    final_month = (season_start_month + season_length - 2) % 12 + 1
    ti, t, tf = zip(*(
        encode_season(y, season_start_month, final_month)
        for y in range(years[0], years[1] + 1)
    ))

    # Create a 3x3 grid that exactly covers the bounding box
    grid_size = 3
    y_resolution = (bounding_box['north'] - bounding_box['south']) / grid_size
    y = [
        bounding_box['south'] + i * y_resolution
        for i in range(grid_size)
    ]
    x_resolution = (bounding_box['east'] - bounding_box['west']) / grid_size
    x = [
        bounding_box['west'] + i * x_resolution
        for i in range(grid_size)
    ]

    # Generate data that are arbitrary but not random: if called twice
    # for the same slice of the same variable, return the same data.
    rng = np.random.default_rng(seed=abs(hash('-'.join([var_name, str(years[0]), str(years[1])]))))
    da = xr.DataArray(
        data=rng.random((years[1] - years[0] + 1, grid_size, grid_size)),
        coords={'T': list(t), 'Y': y, 'X': x},
        attrs={
            'missing': -999,
            'units': 'mm/day',
        },
    )
    da = da.assign_coords(
        Ti=('T', list(ti)),
        Tf=('T', list(tf)),
    )
    return da


class MockObsSource(SeasonalObsSource):
    def __init__(self, name: str):
        self.name = name

    def download(self, request: SeasonalObsRequest) -> xr.DataArray:
        if request.bounding_box is None:
            raise Exception('Mock obs source requires a bounding_box')
        da = generate_mock_data(
            self.name, request.season_start_years, request.season_start_month, request.season_length, request.bounding_box
        )
        if da is None:
            raise Exception("Mock obs shouldn't be None")
        return da


# Let the type checker verify that MockForecastSource implements the SeasonalForecastSource
# interface.
_: SeasonalObsSource = MockObsSource('test')


class MockForecastSource(SeasonalForecastSource):
    def __init__(self, name, missing_years=()):
        self.name = name
        self.superseded_by = None
        self.superseded_doc = None
        self.missing_years = missing_years

    def download(self, request: SeasonalForecastRequest) -> xr.DataArray:
        season_start_month = (request.init_month + request.season_start_lead - 1) % 12 + 1
        if request.init_month + request.season_start_lead >= 13:
            delta = 1
        else:
            delta = 0
        init_first_year, init_final_year = request.init_years
        season_first_year = init_first_year + delta
        season_final_year = init_final_year + delta
        init_years = [
            y for y in range(init_first_year, init_final_year + 1)
            if y not in self.missing_years
        ]
        if request.bounding_box is None:
            raise Exception("Mock forecast source requires a bounding_box")
        da = generate_mock_data(
            self.name, (season_first_year, season_final_year), season_start_month, request.season_length, request.bounding_box
        )
        if da is None:
            raise Exception("Failed to download")
        da = da.assign_coords(
            S=('T', [datetime.datetime(y, request.init_month, 1) for y in init_years])
        )
        return da


# Let the type checker verify that MockForecastSource implements the SeasonalForecastSource
# interface.
_2: SeasonalForecastSource = MockForecastSource('test')


def force_http(source):
    """Return a copy of source that fetches over http instead of https.

    The recording proxy can't see inside https requests, so the tests have to
    talk to the servers over plain http.
    """
    def to_http(url: str) -> str:
        if url.startswith('https://'):
            return url.replace('https://', 'http://', 1)
        return url

    source = copy.copy(source)
    for attr in ('template', 'hindcast_template', 'forecast_template', 'url'):
        url = getattr(source, attr, None)
        if url is not None:
            setattr(source, attr, to_http(url))
    if hasattr(source, 'urls'):
        source.urls = [to_http(url) for url in source.urls]
    return source
