import cptdl as dl
import datetime
import matplotlib.pyplot as plt
import numpy as np
from pathlib import Path
import requests.exceptions
import shutil
import tempfile
import xarray as xr

from . import notebook

def generate_forecast(
        domain_dir, MOS, predictor_names, predictand_name, local_predictand_file,
        download_args, cpt_args
):
    notebook.setup_domain_dir(domain_dir)
    Y, hindcast_data, forecast_data = notebook.download_data(
        predictand_name, local_predictand_file, predictor_names, download_args,
        domain_dir, force_download=False
        # force_download=False is safe because the directory starts out
        # empty unless we explicitly used the -g flag for debugging.
    )
    hcsts, fcsts, skill, pxs, pys = notebook.evaluate_models(
        hindcast_data, MOS, Y, forecast_data, cpt_args, domain_dir, predictor_names
    )
    ensemble = predictor_names
    return (Y,) + notebook.construct_mme_new(
        fcsts, hcsts, Y, ensemble, predictor_names, cpt_args, domain_dir
    )


def ensure_file(src_da, dest):
    if Path(dest).exists():
        dest_da = xr.open_dataarray(dest)
        assert dest_da.equals(src_da), f"{dest} exists but new data are different"
        print(f"{dest} exists and contents are the same")
    else:
        print(f"creating {dest}")
        src_da.to_netcdf(dest)


def ensure_file_monthdir(src_ds, dest_dir, name):
    issue_month = np.unique(src_ds['S'].dt.month).item()
    issue_month_dir = Path(dest_dir) / f'{issue_month:02d}'
    issue_month_dir.mkdir(exist_ok=True)
    dest = issue_month_dir / name
    ensure_file(src_ds, dest)


def update_one_issue(
        dest_dir,
        domain_dir, MOS, predictor_names, predictand_name, local_predictand_file,
        download_args, cpt_args,
):
    Y, det_hcst, pev_hcst, det_fcst, pr_fcst, pev_fcst, nextgen_skill = (
        generate_forecast(
            domain_dir, MOS, predictor_names, predictand_name, local_predictand_file,
            download_args, cpt_args,
        )
    )
    issue_year = det_fcst['S'].dt.year.item()
    assert pev_fcst['S'].dt.year.item() == issue_year
    issue_month = np.unique(det_fcst['S'].dt.month).item()
    issue_month_dir = Path(dest_dir) / f'{issue_month:02d}'
    issue_month_dir.mkdir(exist_ok=True)

    ensure_file(Y, Path(dest_dir) / 'obs.nc')
    ensure_file(det_hcst, issue_month_dir / 'MME_deterministic_hindcasts.nc')
    ensure_file(pev_hcst, issue_month_dir / 'MME_hindcast_prediction_error_variance.nc')
    ensure_file(det_fcst, issue_month_dir / f'MME_deterministic_forecast_{issue_year}.nc')
    ensure_file(pev_fcst, issue_month_dir / f'MME_forecast_prediction_error_variance_{issue_year}.nc')

    notebook.plot_mme_forecasts(
        cpt_args,
        predictand_name,
        pr_fcst,
        MOS,
        domain_dir,
        det_fcst,
    )
    plt.close('all')
    figfile = domain_dir / 'figures' / f"{MOS}_ensemble_probabilistic-deterministicForecast.png"
    figname = f"forecast_{download_args['target']}_ini-{issue_year}-{issue_month:02}.png"
    # Using shutil.move rather than figfile.rename because the latter
    # fails if domain_dir and issue_month_dir are on different
    # filesystems.
    shutil.move(figfile, issue_month_dir / figname)


def update_all(dest_dir, issue_months, skip_issue_dates,
               MOS, predictor_names, predictand_name, local_predictand_file,
               download_args, cpt_args,
               *,
               now=None, persistent_dir=None):
    if now is None:
        now = datetime.datetime.now()
    for issue_month in issue_months:
        month_dir = Path(dest_dir) / f'{issue_month:02}'

        if 'final_year' in download_args:
            assert 'target_final_year' not in download_args, \
                "Don't mix first_year/final_year with target_first_year/target_final_year"
            first_issue_year = download_args['final_year'] + 1
        else:
            assert 'target_final_year' in download_args, \
                'Either final_year or target_final_year must be specified'
            delta = notebook.issue_year_delta(issue_month, download_args['target'])
            first_issue_year = download_args['target_final_year'] + 1 + delta

        issue_date = datetime.datetime(first_issue_year, issue_month, 1)
        with tempfile.TemporaryDirectory() as tempdir:
            while issue_date < now:
                fcst_file = month_dir / f'MME_deterministic_forecast_{issue_date.year}.nc'
                if fcst_file.exists():
                    print(f"{fcst_file} already exists")
                elif issue_date in skip_issue_dates:
                    print(f'skipping issue date {issue_date}')
                else:
                    print(f"generate forecast initialized {issue_date}")
                    issue_download_args = dict(download_args, fdate=issue_date)
                    pycpt_dir = Path(persistent_dir or tempdir)
                    update_one_issue(
                        dest_dir,
                        pycpt_dir,
                        MOS,
                        predictor_names,
                        predictand_name,
                        local_predictand_file,
                        issue_download_args,
                        cpt_args,
                    )

                issue_date = datetime.datetime(issue_date.year + 1, issue_month, 1)
