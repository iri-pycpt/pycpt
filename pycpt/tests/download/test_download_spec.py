"""Specification cases for download(): requests with explicit expected results,
run against both the Ingrid and the opendap implementations. Ingrid is the
reference for legacy behavior. See ../opendap_download_tests.md.
"""
import pathlib
from dataclasses import dataclass

import numpy as np
import pytest
import recording_proxy
import xarray as xr

from pycpt.download import BoundingBox, SeasonalForecastRequest, ingrid, opendap

from .utils import force_http

HERE = pathlib.Path(__file__).absolute().parent
DATA_DIR = HERE.parent / 'data' / 'responses'

BACKENDS = {
    'ingrid': ingrid.forecast_source,
    'opendap': opendap.forecast_source,
}


@pytest.fixture(scope="module", autouse=True)
def proxy(request):
    is_record = request.config.getoption('--record')
    with recording_proxy.recording_proxy(
            str(DATA_DIR),
            is_record=is_record,
            verbose=True,
    ):
        yield


def dates(*strs: str) -> np.ndarray:
    return np.array(strs, dtype='datetime64[ns]')


def floats(*vals: float) -> np.ndarray:
    return np.array(vals, dtype=float)


@dataclass
class ForecastCase:
    id: str
    source_name: str
    request: SeasonalForecastRequest
    # Expected values of the coordinates named by the keys. Coordinates not
    # listed aren't checked.
    expected: dict[str, np.ndarray]
    opendap_only: bool = False


# SPEAR's grid is 1°, with cell centers on whole degrees (so cell edges on
# half degrees), X from 0 to 359 and Y from -90 to 90. Its history starts
# with the January 1991 initialization, and every initialization has leads 0
# through 11. The opendap hindcast and forecast urls overlap at the December
# 2020 initialization.
SPEAR = 'SPEAR.PRCP'

# A 3x3 box whose edges fall on cell centers, for temporal cases.
small_box = BoundingBox(west=284, east=286, south=39, north=41)


def temporal_case(id, *, season_start_month, season_start_lead, season_length,
                  season_start_years, expected, opendap_only=False):
    return ForecastCase(
        id=id,
        source_name=SPEAR,
        request=SeasonalForecastRequest(
            season_start_month=season_start_month,
            season_start_lead=season_start_lead,
            season_length=season_length,
            season_start_years=season_start_years,
            bounding_box=small_box,
        ),
        expected=expected,
        opendap_only=opendap_only,
    )


def spatial_case(id, *, west, east, south, north, X, Y):
    # A single-month season and two years, to keep the recordings small.
    return ForecastCase(
        id=id,
        source_name=SPEAR,
        request=SeasonalForecastRequest(
            season_start_month=2,
            season_start_lead=1,
            season_length=1,
            season_start_years=(2005, 2006),
            bounding_box=BoundingBox(west=west, east=east, south=south, north=north),
        ),
        expected={'X': X, 'Y': Y},
    )


temporal_cases = [
    temporal_case(
        # Jan initialization, lead 1, length 3: FMA season. Non-leap years, so
        # every season is 88 days from Ti to Tf and T is 44 days after Ti.
        'inits-in-month-years-inclusive',
        season_start_month=2, season_start_lead=1, season_length=3,
        season_start_years=(2005, 2007),
        expected={
            'S': dates('2005-01-01', '2006-01-01', '2007-01-01'),
            'Ti': dates('2005-02-01', '2006-02-01', '2007-02-01'),
            'T': dates('2005-03-17', '2006-03-17', '2007-03-17'),
            # The end of the season is represented by its last day, not the
            # day after.
            'Tf': dates('2005-04-30', '2006-04-30', '2007-04-30'),
        },
    ),
    temporal_case(
        # Mar initialization, lead 4, length 2: JA season. An odd number of
        # days from Ti to Tf puts T at noon.
        'lead-and-length',
        season_start_month=7, season_start_lead=4, season_length=2,
        season_start_years=(2005, 2007),
        expected={
            'S': dates('2005-03-01', '2006-03-01', '2007-03-01'),
            'Ti': dates('2005-07-01', '2006-07-01', '2007-07-01'),
            'T': dates('2005-07-31T12', '2006-07-31T12', '2007-07-31T12'),
            'Tf': dates('2005-08-31', '2006-08-31', '2007-08-31'),
        },
    ),
    temporal_case(
        'jan-init-mam',
        season_start_month=3, season_start_lead=2, season_length=3,
        season_start_years=(2005, 2007),
        expected={
            'S': dates('2005-01-01', '2006-01-01', '2007-01-01'),
            'Ti': dates('2005-03-01', '2006-03-01', '2007-03-01'),
            'T': dates('2005-04-15T12', '2006-04-15T12', '2007-04-15T12'),
            'Tf': dates('2005-05-31', '2006-05-31', '2007-05-31'),
        },
    ),
    temporal_case(
        # The season starts the year after initialization, and
        # season_start_years refers to the season, not the initialization.
        'dec-init-mam',
        season_start_month=3, season_start_lead=3, season_length=3,
        season_start_years=(2005, 2007),
        expected={
            'S': dates('2004-12-01', '2005-12-01', '2006-12-01'),
            'Ti': dates('2005-03-01', '2006-03-01', '2007-03-01'),
            'T': dates('2005-04-15T12', '2006-04-15T12', '2007-04-15T12'),
            'Tf': dates('2005-05-31', '2006-05-31', '2007-05-31'),
        },
    ),
    temporal_case(
        # Nov initialization, lead 1, length 3: DJF, spanning the new year.
        # The last season ends in a leap year.
        'djf',
        season_start_month=12, season_start_lead=1, season_length=3,
        season_start_years=(2005, 2007),
        expected={
            'S': dates('2005-11-01', '2006-11-01', '2007-11-01'),
            'Ti': dates('2005-12-01', '2006-12-01', '2007-12-01'),
            'T': dates('2006-01-14T12', '2007-01-14T12', '2008-01-15'),
            'Tf': dates('2006-02-28', '2007-02-28', '2008-02-29'),
        },
    ),
    temporal_case(
        # The Dec 2019 initialization is only in the opendap hindcast url,
        # Dec 2020 is in both, and Dec 2021 is only in the forecast url.
        'hindcast-forecast-split',
        season_start_month=1, season_start_lead=1, season_length=1,
        season_start_years=(2020, 2022),
        expected={
            'S': dates('2019-12-01', '2020-12-01', '2021-12-01'),
            'Ti': dates('2020-01-01', '2021-01-01', '2022-01-01'),
            'T': dates('2020-01-16', '2021-01-16', '2022-01-16'),
            'Tf': dates('2020-01-31', '2021-01-31', '2022-01-31'),
        },
    ),
    temporal_case(
        # The Jun 2025 and Jun 2026 initializations are missing, and missing
        # years are left out. Ingrid can't take part: its S grid is regular,
        # and it returns those years with all-NaN values. If the missing data
        # is ever filled in on the server, re-recording will break this case;
        # the synthetic test of missing initializations covers the behavior.
        'missing-initializations',
        season_start_month=7, season_start_lead=1, season_length=1,
        season_start_years=(2024, 2026),
        expected={
            'S': dates('2024-06-01'),
            'Ti': dates('2024-07-01'),
            'T': dates('2024-07-16'),
            'Tf': dates('2024-07-31'),
        },
        opendap_only=True,
    ),
    temporal_case(
        # Before the start of SPEAR's history. Ingrid can't take part: it
        # returns an error rather than an empty result.
        'no-data-in-range',
        season_start_month=2, season_start_lead=1, season_length=1,
        season_start_years=(1980, 1982),
        expected={'S': dates(), 'Ti': dates(), 'T': dates(), 'Tf': dates()},
        opendap_only=True,
    ),
]

spatial_cases = [
    spatial_case(
        # Cells 283 and 287 only touch the box along an edge.
        'edges-on-cell-edges',
        west=283.5, east=286.5, south=38.5, north=41.5,
        X=floats(284, 285, 286), Y=floats(41, 40, 39),
    ),
    spatial_case(
        'edges-on-cell-centers',
        west=284, east=286, south=39, north=41,
        X=floats(284, 285, 286), Y=floats(41, 40, 39),
    ),
    spatial_case(
        'edges-inside-cells-off-center',
        west=284.2, east=286.7, south=39.2, north=40.7,
        X=floats(284, 285, 286, 287), Y=floats(41, 40, 39),
    ),
    spatial_case(
        'aligned-in-x-not-y',
        west=283.5, east=285.5, south=39.3, north=40.2,
        X=floats(284, 285), Y=floats(40, 39),
    ),
    spatial_case(
        'smaller-than-one-cell',
        west=285.1, east=285.3, south=40.1, north=40.3,
        X=floats(285), Y=floats(40),
    ),
    spatial_case(
        'zero-width-and-height',
        west=285.2, east=285.2, south=40.2, north=40.2,
        X=floats(285), Y=floats(40),
    ),
    spatial_case(
        'past-latitude-range',
        west=284, east=286, south=87, north=95,
        X=floats(284, 285, 286), Y=floats(90, 89, 88, 87),
    ),
]

seam_cases = [
    spatial_case(
        # Data is 0-360, box is -180-180; X follows the box.
        'other-convention',
        west=-76, east=-74, south=39, north=41,
        X=floats(-76, -75, -74), Y=floats(41, 40, 39),
    ),
    spatial_case(
        # As above, but the cell at the west edge has its center outside the
        # box, so it isn't enough to wrap X into [west, west + 360).
        'other-convention-edges-inside-cells',
        west=-75.8, east=-74.3, south=39, north=41,
        X=floats(-76, -75, -74), Y=floats(41, 40, 39),
    ),
    # SPEAR's seam is the cell edge at 359.5 (= -0.5), not 360.
    spatial_case(
        'edge-on-seam-0-360',
        west=349.5, east=359.5, south=39, north=41,
        X=floats(*range(350, 360)), Y=floats(41, 40, 39),
    ),
    spatial_case(
        'edge-on-seam-negative',
        west=-10.5, east=-0.5, south=39, north=41,
        X=floats(*range(-10, 0)), Y=floats(41, 40, 39),
    ),
    # Boxes that end at 360 or 0 overlap the cell centered on 0 (359.5 to
    # 360.5), so for SPEAR they span the seam.
    spatial_case(
        'east-edge-360',
        west=350, east=360, south=39, north=41,
        X=floats(*range(350, 361)), Y=floats(41, 40, 39),
    ),
    spatial_case(
        'east-edge-0',
        west=-10, east=0, south=39, north=41,
        X=floats(*range(-10, 1)), Y=floats(41, 40, 39),
    ),
    spatial_case(
        'spans-seam-edges-on-cell-edges',
        west=-2.5, east=2.5, south=39, north=41,
        X=floats(-2, -1, 0, 1, 2), Y=floats(41, 40, 39),
    ),
    spatial_case(
        'spans-seam-edges-inside-cells',
        west=-2.7, east=2.2, south=39, north=41,
        X=floats(-3, -2, -1, 0, 1, 2), Y=floats(41, 40, 39),
    ),
    # The cell at the box's east edge is also the one at its west edge, and
    # it's returned at both ends: 361 points. To get the whole globe, leave
    # the bounding box None instead.
    spatial_case(
        'all-longitudes-0-360',
        west=0, east=360, south=39.5, north=40.5,
        X=floats(*range(0, 361)), Y=floats(40),  # noqa: PIE808
    ),
    spatial_case(
        'all-longitudes-negative',
        west=-180, east=180, south=39.5, north=40.5,
        X=floats(*range(-180, 181)), Y=floats(40),
    ),
]

forecast_cases = temporal_cases + spatial_cases + seam_cases


@pytest.mark.parametrize('backend', BACKENDS)
@pytest.mark.parametrize('case', forecast_cases, ids=lambda c: c.id)
def test_forecast_spec(case: ForecastCase, backend: str):
    if case.opendap_only and backend != 'opendap':
        pytest.skip('opendap-only case')
    source = force_http(BACKENDS[backend](case.source_name, None))
    da = source.download(case.request)
    validate_format(da)
    for name, expected in case.expected.items():
        np.testing.assert_array_equal(da[name].values, expected, err_msg=name)


def validate_format(da: xr.DataArray):
    assert da.dims == ('T', 'Y', 'X')
    assert (np.diff(da['Y'].values) < 0).all()
