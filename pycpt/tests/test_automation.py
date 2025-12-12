from dataclasses import dataclass, field
import datetime
import numpy as np
import pathlib
import pytest
import tempfile
import unittest.mock
import xarray as xr

import cptio
import pycpt.automation
import pycpt.notebook

def encode_season(first_year, target_first_month, target_last_month):
    if target_last_month >= target_first_month:
        end_year = first_year
    else:
        end_year = first_year + 1
    return cptio.fileio.read_cpt_date_range(f'{first_year}-{target_first_month}/{end_year}-{target_last_month}')

def make_data(dataset, years, target_first_month, target_last_month, start_month=None):
    ti, t, tf = zip(*(
        encode_season(y, target_first_month, target_last_month)
        for y in years
    ))
    # Generate data that are arbitrary but not random: if called twice
    # for the same slice of the same dataset, return the same data.
    rng = np.random.default_rng(seed=abs(hash('-'.join([dataset, str(years)]))))
    da = xr.DataArray(
        data=rng.random((len(years), 4, 4)),
        coords={ 
            'T': list(t),
            'Y': [40.375, 40.125, 39.875, 39.625],
            'X': [-75.375, -75.125, -74.875, -74.625],
        },
        attrs={
            'missing': -999,
            'units': 'mm/day',
        },
    )
    da = da.assign_coords(
        Ti=('T', list(ti)),
        Tf=('T', list(tf)),
    )
    if start_month is not None:
        da = da.assign_coords(
            S=('T', [datetime.datetime(y, start_month, 1) for y in years])
        )
    return da

def mock_download_data(
    predictand_name,
    local_predictand_file,
    predictor_names,
    download_args,
    files_root,
    force_download,
):
    target_years = range(download_args['target_first_year'], download_args['target_final_year'] + 1)
    start_month = download_args['fdate'].month
    target_first_month, target_last_month = cptio.utilities.parse_target(download_args['target'])
    Y = make_data(predictand_name, target_years, target_first_month, target_last_month)
    hcsts = [
        make_data(predictor, target_years, target_first_month, target_last_month, start_month)
        for predictor in predictor_names
    ]
    fcsts = [
        make_data(predictor, [download_args['fdate'].year], target_first_month, target_last_month, start_month)
        for predictor in predictor_names
    ]
    return Y, hcsts, fcsts

@dataclass
class Config:
    operational_forecasts_dir: str
    forecast_name = 'testforecast'
    MOS =  'CCA'
    ensemble: list[str] = field(default_factory=lambda: ['M1.PRCP', 'M2.PRCP'])
    predictand_name = 'UCSB.PRCP'
    local_predictand_file = None
    download_args: dict = field(default_factory=lambda: {
        'target_first_year': 2010,
        'target_final_year': 2020,
        'target': 'Jun-Sep',
    })
    cpt_args: dict = field(default_factory=lambda: {
        'cpt_kwargs': {
            #'interactive': True,
            #'outputdir': 'foo',
        },
        'tailoring': None,
    })
    issue_months: list[int] = field(default_factory=lambda: [5])


@unittest.mock.patch('pycpt.notebook.download_data', mock_download_data)
def test_update_uninitialized():
    with tempfile.TemporaryDirectory() as outdir:
        outdir = pathlib.Path(outdir)
        config = Config(
            operational_forecasts_dir=outdir,
        )
        with pytest.raises(Exception):
            pycpt.automation.generate_one_issue(config)


@unittest.mock.patch('pycpt.notebook.download_data', mock_download_data)
def test_initialize():
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


@unittest.mock.patch('pycpt.notebook.download_data', mock_download_data)
def test_add():
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
    def mock_download_data_one_missing(*args, **kwargs):
        Y, hcsts, fcsts = mock_download_data(*args, **kwargs)
        fcsts = [None] + fcsts[1:]
        return Y, hcsts, fcsts

    with tempfile.TemporaryDirectory() as outdir:
        outdir = pathlib.Path(outdir)
        config = Config(
            operational_forecasts_dir=outdir,
        )

        # Generate the 2021-05-01 forecast with one missing model
        with unittest.mock.patch(
            'pycpt.notebook.download_data', mock_download_data_one_missing
        ):
            pycpt.automation.initialize(config, until=datetime.datetime(2021, 5, 2))
        monthdir = outdir /config.forecast_name / '05'
        assert (monthdir / 'MME_deterministic_forecast_2021.nc').is_file()

        # Generate the same forecast again with all models present
        with unittest.mock.patch(
            'pycpt.notebook.download_data', mock_download_data
        ):
            wrote = pycpt.automation.generate_one_issue(
                config, issue_date=datetime.datetime(2021, 5, 1), update=True
            )
        assert wrote

        # One more time, this time it should not overwrite.
        with unittest.mock.patch(
            'pycpt.notebook.download_data', mock_download_data
        ):
            wrote = pycpt.automation.generate_one_issue(config, issue_date=datetime.datetime(2021, 5, 1))
        assert not wrote

@unittest.mock.patch('pycpt.notebook.download_data', mock_download_data)
def test_update_no_change():
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

@unittest.mock.patch('pycpt.notebook.download_data', mock_download_data)
def test_no_update_no_change():
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
