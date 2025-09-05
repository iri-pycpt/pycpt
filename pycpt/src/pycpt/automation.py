import datetime
import matplotlib.pyplot as plt
from pathlib import Path
import shutil
import tempfile
import xarray as xr

from . import notebook


def generate_forecast(domain_dir, config, issue_date):
    notebook.setup_domain_dir(domain_dir)
    issue_download_args = dict(config.download_args, fdate=issue_date)
    Y, hindcast_data, forecast_data = notebook.download_data(
        config.predictand_name,
        config.local_predictand_file,
        config.ensemble,
        issue_download_args,
        domain_dir,
        force_download=False
        # force_download=False is safe because the directory starts out
        # empty unless we explicitly used the -g flag for debugging.
    )
    hcsts, fcsts, skill, pxs, pys = notebook.evaluate_models(
        hindcast_data,
        config.MOS,
        Y,
        forecast_data,
        config.cpt_args,
        domain_dir,
        config.ensemble
    )
    mme_hcst, mme_fcst = notebook.construct_mme_new(
        fcsts, hcsts, Y, config.ensemble, config.ensemble,
        domain_dir
    )
    mme_skill = notebook.evaluate_mme(mme_hcst, Y, config.cpt_args, domain_dir)
    return (Y, mme_hcst, mme_fcst, mme_skill)


def ensure_file(ds, dest, update=False):
    wrote = False
    if Path(dest).exists():
        if isinstance(ds, xr.Dataset):
            old_ds = xr.open_dataset(dest)
        else:
            old_ds = xr.open_dataarray(dest)
        if old_ds.equals(ds):
            print(f"{dest} exists and is identical to what was generated now. No change needed.")
        elif update:
            print(f"Overwriting {dest} with a new version.")
            # Move original file to a new name. Choosing a name that doesn't
            # end in .nc to avoid the risk of it being used up by downstream
            # tools.
            now = datetime.datetime.now().strftime('%Y%m%dT%H%M')
            backup_name = f"{dest.name}-backup-{now}"
            backup = dest.with_name(backup_name)
            print(f"backing up {dest} to {backup}")
            dest.rename(backup)
            ds.to_netcdf(dest)
            wrote = True
        else:
            raise Exception(f"{dest} already exists and its contents are different.")
    else:
        print(f"writing {dest}")
        ds.to_netcdf(dest)
        wrote = True
    return wrote


def _is_better(new, old):
    if 'model_present' in new:
        old_model_count = old['model_present'].sum('model')
        new_model_count = new['model_present'].sum('model')
        if (new_model_count > old_model_count).all():
            return True
        else:
            return False
    else:
        return False


def generate_one_issue(config, *, issue_date=None, update=False):
    if issue_date is None:
        issue_date = datetime.datetime.now().replace(day=1)
    if issue_date.month not in config.issue_months:
        raise Exception(f"issue date {issue_date.strftime('%Y-%m-%d')} is not one of the issue months defined in the configuration.")
    with tempfile.TemporaryDirectory() as domain_dir:
        result = _generate_one_issue_helper(config, domain_dir, issue_date, update)
    return result

def _generate_one_issue_helper(config, domain_dir, issue_date, update):
    domain_dir = Path(domain_dir)
    dest_dir = Path(config.operational_forecasts_dir) / config.forecast_name
    if not dest_dir.exists():
        raise Exception(
            f"{dest_dir} does not exist. Use --init if you meant "
            "to create a new set of forecasts."
        )

    issue_month_dir = dest_dir / f'{issue_date.month:02d}'
    mu_file = issue_month_dir / f'MME_deterministic_forecast_{issue_date.year}.nc'

    if mu_file.exists():
        print(f"A forecast for {issue_date.strftime('%Y-%m-%d')} has already been generated.")
        print("The table below shows which models were available at the time that forecast was generated (True = available).")
        with xr.open_dataset(mu_file) as ds:
            print(ds['model_present'].to_pandas().to_string(header=False))
        if not update:
            print("Rerun with --update if you want to ovewrite it with a new forecast.")
            return False

    Y, hcst, fcst, _ = generate_forecast(domain_dir, config, issue_date)

    issue_month_dir.mkdir(exist_ok=True)

    ensure_file(Y, Path(dest_dir) / 'obs.nc')

    # Never update hindcasts. If they have changed, then fail.
    ensure_file(
        hcst[['deterministic', 'model_present']].set_coords('model_present'),
        issue_month_dir / 'MME_deterministic_hindcasts.nc',
    )
    ensure_file(
        hcst['prediction_error_variance'],
        issue_month_dir / 'MME_hindcast_prediction_error_variance.nc',
    )

    # A forecast made with more models can replace one with fewer, if
    # update is True.
    if mu_file.exists():
        with xr.open_dataset(mu_file) as old_ds:
            is_better = _is_better(fcst, old_ds)
        if is_better and not update:
            print("A forecast made with fewer models already exists.")
            print("Run again with --update if you want to ovewrite it with the new forecast.")
            return False
    else:
        # If there is no pre-existing forecast then the new forecast is
        # necessarily better than what we had.
        is_better = True

    wrote = False
    if is_better:
        wrote |= ensure_file(
            fcst[['deterministic', 'model_present']].set_coords('model_present'),
            issue_month_dir / f'MME_deterministic_forecast_{issue_date.year}.nc',
            update=update,
        )
        wrote |= ensure_file(
            fcst['prediction_error_variance'],
            issue_month_dir / f'MME_forecast_prediction_error_variance_{issue_date.year}.nc',
            update=update,
        )

        # TODO Being lazy and not checking for unexpected changes to the png file
        notebook.plot_mme_forecasts(
            config.cpt_args,
            config.predictand_name,
            fcst['probabilistic'],
            config.MOS,
            domain_dir,
            fcst['deterministic'],
            model_present=fcst['model_present']
        )
        plt.close('all')
        figfile = domain_dir / 'figures' / f"{config.MOS}_ensemble_probabilistic-deterministicForecast.png"
        figname = f"forecast_{config.download_args['target']}_ini-{issue_date.year}-{issue_date.month:02}.png"
        new_file = issue_month_dir / figname
        if new_file.exists():
            now = datetime.datetime.now().strftime('%Y%m%dT%H%M')
            backup_stem = f"{new_file.stem}-backup-{now}"
            backup_file = new_file.with_stem(backup_stem)
            print(f"backing up {new_file} to {backup_file}")
            new_file.rename(backup_file)
        # Using shutil.move rather than figfile.rename because the latter
        # fails if domain_dir and issue_month_dir are on different
        # filesystems.
        shutil.move(figfile, issue_month_dir / figname)
    else:
        print("No new information. The pre-existing forecast remains in place.")

    return wrote
    


def initialize(config, until=None):
    if until is None:
        until = datetime.datetime.now()
    if not hasattr(config, 'skip_issue_dates'):
        config.skip_issue_dates = []

    dest_dir = Path(config.operational_forecasts_dir) / config.forecast_name
    if dest_dir.exists():
        if not dest_dir.is_dir():
            raise Exception(f'{dest_dir} exists but is not a directory')
        print(f'Output will be added to {dest_dir}')
    else:
        dest_dir.mkdir(parents=True)
        print(f'Created {dest_dir}')

    for issue_month in config.issue_months:
        month_dir = dest_dir / f'{issue_month:02}'
        download_args = config.download_args
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
        skip_issue_dates = getattr(config, 'skip_issue_dates', [])
        with tempfile.TemporaryDirectory() as domain_dir:
            # Using the same tempdir for all years of a single
            # issue month, to avoid redownloading the hindcasts.
            while issue_date < until:
                fcst_file = month_dir / f'MME_deterministic_forecast_{issue_date.year}.nc'
                if fcst_file.exists():
                    print(f"{fcst_file} already exists")
                elif issue_date in skip_issue_dates:
                    print(f'skipping issue date {issue_date.strftime("%Y-%m-%d")}')
                else:
                    print(f"generating forecast initialized {issue_date.strftime('%Y-%m-%d')}")
                    _generate_one_issue_helper(config, domain_dir, issue_date, update=False)

                issue_date = datetime.datetime(issue_date.year + 1, issue_month, 1)
