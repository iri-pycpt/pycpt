"""Specification cases for download(): requests with explicit expected results,
run against both the Ingrid and the opendap implementations. Ingrid is the
reference for legacy behavior. See ../opendap_download_tests.md.
"""
import pathlib

import numpy as np
import pytest
import recording_proxy

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


def dates(*strs: str) -> np.ndarray:
    return np.array(strs, dtype='datetime64[ns]')


def floats(*vals: float) -> np.ndarray:
    return np.array(vals, dtype=float)



# NMME grid is 1°, with cell centers on whole degrees (so cell edges on
# half degrees), X from 0 to 359 and Y from -90 to 90. SPEAR's history starts
# with the January 1991 initialization, and every initialization has leads 0
# through 11. The opendap hindcast and forecast urls overlap at the December
# 2020 initialization.

# A 3x3 box whose edges fall on cell centers, for temporal selection tests.
small_box = BoundingBox(west=284, east=286, south=39, north=41)

INGRID_SPEAR = force_http(ingrid.forecast_source('SPEAR.PRCP', None))
OPENDAP_SPEAR = force_http(opendap.forecast_source('SPEAR.PRCP', None))

def validate(da, expected):
    assert da.dims == ('T', 'Y', 'X')
    assert (np.diff(da['Y'].values) < 0).all()
    for name, values in expected.items():
        np.testing.assert_array_equal(da[name].values, values, err_msg=name)


# Temporal selection

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_lead_1_length_2(source):
    request = SeasonalForecastRequest(
        season_start_month=2, season_start_lead=1, season_length=2,
        season_start_years=(2005, 2007),
        bounding_box=small_box,
    )
    expected = {
        'S': dates('2005-01-01', '2006-01-01', '2007-01-01'),
        'Ti': dates('2005-02-01', '2006-02-01', '2007-02-01'),
        'T': dates('2005-03-02', '2006-03-02', '2007-03-02'),
        'Tf': dates('2005-03-31', '2006-03-31', '2007-03-31'),
    }
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_lead_1_length_3(source):
    request = SeasonalForecastRequest(
        season_start_month=2, season_start_lead=1, season_length=3,
        season_start_years=(2005, 2007),
        bounding_box=small_box,
    )
    expected = {
        'S': dates('2005-01-01', '2006-01-01', '2007-01-01'),
        'Ti': dates('2005-02-01', '2006-02-01', '2007-02-01'),
        'T': dates('2005-03-17', '2006-03-17', '2007-03-17'),
        'Tf': dates('2005-04-30', '2006-04-30', '2007-04-30'),
    }
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_lead_0(source):
    request = SeasonalForecastRequest(
        season_start_month=2, season_start_lead=0, season_length=2,
        season_start_years=(2005, 2007),
        bounding_box=small_box,
    )
    expected = {
        'S': dates('2005-02-01', '2006-02-01', '2007-02-01'),
        'Ti': dates('2005-02-01', '2006-02-01', '2007-02-01'),
        'T': dates('2005-03-02', '2006-03-02', '2007-03-02'),
        'Tf': dates('2005-03-31', '2006-03-31', '2007-03-31'),
    }
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_season_following_year(source):
    request = SeasonalForecastRequest(
        season_start_month=3, season_start_lead=3, season_length=3,
        season_start_years=(2005, 2007),
        bounding_box=small_box
    )
    expected = {
        'S': dates('2004-12-01', '2005-12-01', '2006-12-01'),
        'Ti': dates('2005-03-01', '2006-03-01', '2007-03-01'),
        'T': dates('2005-04-15T12', '2006-04-15T12', '2007-04-15T12'),
        'Tf': dates('2005-05-31', '2006-05-31', '2007-05-31'),
    }
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_season_spanning_jan_1(source):
    request = SeasonalForecastRequest(
        season_start_month=12, season_start_lead=1, season_length=3,
        season_start_years=(2005, 2007),
        bounding_box=small_box,
    )
    expected = {
        'S': dates('2005-11-01', '2006-11-01', '2007-11-01'),
        'Ti': dates('2005-12-01', '2006-12-01', '2007-12-01'),
        'T': dates('2006-01-14T12', '2007-01-14T12', '2008-01-15'),
        'Tf': dates('2006-02-28', '2007-02-28', '2008-02-29'),
    }
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_hindcast_forecast_split(source):
    """The Dec 2019 initialization is only in the opendap hindcast url,
    Dec 2020 is in both, and Dec 2021 is only in the forecast url.
    """
    request = SeasonalForecastRequest(
        season_start_month=1, season_start_lead=1, season_length=1,
        season_start_years=(2020, 2022),
        bounding_box=small_box,
    )
    expected = {
        'S': dates('2019-12-01', '2020-12-01', '2021-12-01'),
        'Ti': dates('2020-01-01', '2021-01-01', '2022-01-01'),
        'T': dates('2020-01-16', '2021-01-16', '2022-01-16'),
        'Tf': dates('2020-01-31', '2021-01-31', '2022-01-31'),
    }
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_missing_inits(source):
    """The Jun 2025 and Jun 2026 initializations are missing, and missing
    years are left out. Ingrid can't take part: its S grid is regular,
    and it returns those years with all-NaN values. If the missing data
    is ever filled in on the server, re-recording will break this case;
    the synthetic test of missing initializations covers the behavior.
    """
    request = SeasonalForecastRequest(
        season_start_month=7, season_start_lead=1, season_length=1,
        season_start_years=(2024, 2026),
        bounding_box=small_box,
    )
    expected = {
        'S': dates('2024-06-01'),
        'Ti': dates('2024-07-01'),
        'T': dates('2024-07-16'),
        'Tf': dates('2024-07-31'),
    }
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_no_inits_in_range(source):
    """Before the start of SPEAR's history. Ingrid can't take part: it
    returns an error rather than an empty result.
    """
    request = SeasonalForecastRequest(
        season_start_month=2, season_start_lead=1, season_length=1,
        season_start_years=(1980, 1982),
        bounding_box=small_box,
    )
    expected = {'S': dates(), 'Ti': dates(), 'T': dates(), 'Tf': dates()}
    validate(source.download(request), expected)


# Spatial selection

def spatial_case(source, west, east, south, north, X, Y):
    # A single-month season and two years, to keep the recordings small.
    request = SeasonalForecastRequest(
        season_start_month=2, season_start_lead=1, season_length=1,
        season_start_years=(2005, 2006),
        bounding_box=BoundingBox(west=west, east=east, south=south, north=north),
    )
    expected = {'X': X, 'Y': Y}
    validate(source.download(request), expected)

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_bounds_on_cell_edges(source):
    """Cells 283 and 287 only touch the box along an edge."""
    spatial_case(
        source,
        west=283.5, east=286.5, south=38.5, north=41.5,
        X=floats(284, 285, 286), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_bounds_on_cell_centers(source):
    spatial_case(
        source,
        west=284, east=286, south=39, north=41,
        X=floats(284, 285, 286), Y=floats(41, 40, 39),
     )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_bounds_unaligned(source):
    spatial_case(
        source,
        west=284.2, east=286.7, south=39.2, north=40.7,
        X=floats(284, 285, 286, 287), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_smaller_than_one_cell(source):
    spatial_case(
        source,
        west=285.1, east=285.3, south=40.1, north=40.3,
        X=floats(285), Y=floats(40),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_zero_width_and_height(source):
    """Arguably this should return an empty result, to be consistent with the
    behavior when the bounding box only intersects the dataset at an edge (area
    of overlap is zero), but we are preserving the legacy Ingrid behavior.
    """
    spatial_case(
        source,
        west=285.2, east=285.2, south=40.2, north=40.2,
        X=floats(285), Y=floats(40),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_past_pole(source):
    spatial_case(
        source,
        west=284, east=286, south=87, north=95,
        X=floats(284, 285, 286), Y=floats(90, 89, 88, 87),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_shift_seam(source):
    """Data is 0-360, box is -180-180; X follows the box."""
    spatial_case(
        source,
        west=-76, east=-74, south=39, north=41,
        X=floats(-76, -75, -74), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_shift_seam_2(source):
    """As above, but the cell at the west edge has its center outside the
    box, so it isn't enough to wrap X into [west, west + 360).
    """
    spatial_case(
        source,
        west=-75.8, east=-74.3, south=39, north=41,
        X=floats(-76, -75, -74), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_bound_on_seam(source):
    """SPEAR's seam is the cell edge at 359.5 (= -0.5), not 360."""
    spatial_case(
        source,
        west=349.5, east=359.5, south=39, north=41,
        X=floats(*range(350, 360)), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_bound_on_seam_negative(source):
    spatial_case(
        source,
        west=-10.5, east=-0.5, south=39, north=41,
        X=floats(*range(-10, 0)), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_east_bound_360(source):
    """Boxes that end at 360 or 0 overlap the cell centered on 0 (359.5 to
    360.5), so for SPEAR they span the seam.
    """
    spatial_case(
        source,
        west=350, east=360, south=39, north=41,
        X=floats(*range(350, 361)), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_east_bound_0(source):
    spatial_case(
        source,
        west=-10, east=0, south=39, north=41,
        X=floats(*range(-10, 1)), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_span_seam_bounds_on_cell_edges(source):
    spatial_case(
        source,
        west=-2.5, east=2.5, south=39, north=41,
        X=floats(-2, -1, 0, 1, 2), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_span_seam_bounds_unaligned(source):
    spatial_case(
        source,
        west=-2.7, east=2.2, south=39, north=41,
        X=floats(-3, -2, -1, 0, 1, 2), Y=floats(41, 40, 39),
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_exceed_360(source):
    """The cell at the box's east edge is also the one at its west edge, and
    it's returned at both ends, so we get 361 points. (To get the whole globe
    with no duplicate cells, use bounding_box=None instead.)
    """
    spatial_case(
        source,
        west=0, east=360, south=39.5, north=40.5,
        X=floats(*range(0, 361)), Y=floats(40),  # noqa: PIE808
    )

@pytest.mark.parametrize('source', [INGRID_SPEAR, OPENDAP_SPEAR], ids=lambda x: x.__class__.__name__)
def test_exceed_360_2(source):
    spatial_case(
        source,
        west=-180, east=180, south=39.5, north=40.5,
        X=floats(*range(-180, 181)), Y=floats(40),
    )
