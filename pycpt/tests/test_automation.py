from contextlib import contextmanager
from dataclasses import dataclass, field
import datetime
import pathlib
import pytest
import tempfile
import unittest.mock

import pycpt.automation

from .download.utils import MockForecastSource, MockObsSource


def create_mock_sources(missing_years=None):
    if missing_years is None:
        missing_years = {}

    def mock_obs(name, pressure_level):
        return MockObsSource(name)

    def mock_fcst(name, pressure_level):
        return MockForecastSource(name, missing_years=missing_years.get(name, ())) 

    return mock_obs, mock_fcst


@contextmanager
def mock_data_sources(missing_years=None):
    mock_obs, mock_fcst = create_mock_sources(missing_years=missing_years)

    with unittest.mock.patch('pycpt.download.catalog.observation_source', mock_obs), \
         unittest.mock.patch('pycpt.download.catalog.forecast_source', mock_fcst):
        yield


@dataclass
class Config:
    operational_forecasts_dir: pathlib.Path
    forecast_name = 'testforecast'
    MOS =  'CCA'
    ensemble: list[str] = field(default_factory=lambda: ['M1.PRCP', 'M2.PRCP'])
    predictand_name = 'O1.PRCP'
    local_predictand_file: str | None = None
    download_args: dict = field(default_factory=lambda: {
        'target_first_year': 2010,
        'target_final_year': 2020,
        'target': 'Jun-Sep',
        'predictand_extent': {'west': 0., 'east': 1, 'south': 2., 'north': 3},
        'predictor_extent': {'west': 0., 'east': 1, 'south': 2., 'north': 3},
    })
    cpt_args: dict = field(default_factory=lambda: {
        'cpt_kwargs': {
            #'interactive': True,
            #'outputdir': 'foo',
        },
        'tailoring': None,
    })
    issue_months: list[int] = field(default_factory=lambda: [5])


def test_update_uninitialized():
    with mock_data_sources():
        with tempfile.TemporaryDirectory() as outdir:
            outdir = pathlib.Path(outdir)
            config = Config(
                operational_forecasts_dir=outdir,
            )
            with pytest.raises(Exception):
                pycpt.automation.generate_one_issue(config)


def test_initialize():
    with mock_data_sources():
        with tempfile.TemporaryDirectory() as outdir:
            outdir = pathlib.Path(outdir)
            config = Config(
                issue_months=[5, 6],
                operational_forecasts_dir=outdir,
            )
            pycpt.automation.initialize(config, until=datetime.datetime(2022, 6, 2))
            assert set(map(lambda x: x.name, (outdir / config.forecast_name / '05').iterdir())) == {
                'MME_deterministic_hindcasts.nc',
                'MME_hindcast_prediction_error_variance.nc',
                'MME_deterministic_forecast_2021.nc',
                'MME_forecast_prediction_error_variance_2021.nc',
                'forecast_Jun-Sep_ini-2021-05.png',
                'MME_deterministic_forecast_2022.nc',
                'MME_forecast_prediction_error_variance_2022.nc',
                'forecast_Jun-Sep_ini-2022-05.png',
            }
            assert set(map(lambda x: x.name, (outdir / config.forecast_name / '06').iterdir())) == {
                'MME_deterministic_hindcasts.nc',
                'MME_hindcast_prediction_error_variance.nc',
                'MME_deterministic_forecast_2021.nc',
                'MME_forecast_prediction_error_variance_2021.nc',
                'forecast_Jun-Sep_ini-2021-06.png',
                'MME_deterministic_forecast_2022.nc',
                'MME_forecast_prediction_error_variance_2022.nc',
                'forecast_Jun-Sep_ini-2022-06.png',
            }
            wrote = pycpt.automation.generate_one_issue(config, issue_date=datetime.datetime(2023, 5, 1))
            assert wrote


def test_add():
    with mock_data_sources():
        with tempfile.TemporaryDirectory() as outdir:
            outdir = pathlib.Path(outdir)
            config = Config(
                operational_forecasts_dir=outdir,
            )
            pycpt.automation.initialize(config, until=datetime.datetime(2021, 5, 2))
            monthdir = outdir /config.forecast_name / '05'
            assert (monthdir / 'MME_deterministic_forecast_2021.nc').is_file()
            assert not (monthdir / 'MME_deterministic_forecast_2022.nc').is_file()
            wrote = pycpt.automation.generate_one_issue(config, issue_date=datetime.datetime(2022, 5, 1))
            assert wrote
            assert (monthdir / 'MME_deterministic_forecast_2022.nc').is_file()


def test_update():
    with tempfile.TemporaryDirectory() as outdir:
        outdir = pathlib.Path(outdir)
        config = Config(
            operational_forecasts_dir=outdir,
        )

        # Generate the 2021-05-01 forecast with one missing model
        with mock_data_sources(missing_years={'M1.PRCP': (2021,)}):
            pycpt.automation.initialize(config, until=datetime.datetime(2021, 5, 2))
        monthdir = outdir /config.forecast_name / '05'
        assert (monthdir / 'MME_deterministic_forecast_2021.nc').is_file()

        # Generate the same forecast again with all models present
        with mock_data_sources():
            wrote = pycpt.automation.generate_one_issue(
                config, issue_date=datetime.datetime(2021, 5, 1), update=True
            )
        assert wrote

        # One more time, this time it should not overwrite.
        with mock_data_sources():
            wrote = pycpt.automation.generate_one_issue(config, issue_date=datetime.datetime(2021, 5, 1))
        assert not wrote

def test_update_no_change():
    with mock_data_sources():
        with tempfile.TemporaryDirectory() as outdir:
            outdir = pathlib.Path(outdir)
            config = Config(
                operational_forecasts_dir=outdir,
            )

            # Generate the 2021-05-01 forecast
            pycpt.automation.initialize(config, until=datetime.datetime(2021, 5, 2))
            monthdir = outdir /config.forecast_name / '05'
            assert (monthdir / 'MME_deterministic_forecast_2021.nc').is_file()

            # Generate the same forecast again with --update
            wrote = pycpt.automation.generate_one_issue(
                config, issue_date=datetime.datetime(2021, 5, 1), update=True
            )
            assert not wrote

def test_no_update_no_change():
    with mock_data_sources():
        with tempfile.TemporaryDirectory() as outdir:
            outdir = pathlib.Path(outdir)
            config = Config(
                operational_forecasts_dir=outdir,
            )

            # Generate the 2021-05-01 forecast
            pycpt.automation.initialize(config, until=datetime.datetime(2021, 5, 2))
            monthdir = outdir /config.forecast_name / '05'
            assert (monthdir / 'MME_deterministic_forecast_2021.nc').is_file()

            # Generate the same forecast again without --update
            wrote = pycpt.automation.generate_one_issue(
                config, issue_date=datetime.datetime(2021, 5, 1), update=False
            )
            assert not wrote
