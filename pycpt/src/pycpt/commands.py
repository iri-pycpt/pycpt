import argparse
import getpass
import importlib.util
from pathlib import Path


def generate_forecasts(allow_missing=False):
    import pycpt.automation
    parser = argparse.ArgumentParser(
        description='Generates PyCPT forecasts from a predetermined configuration',
    )
    parser.add_argument('--allow-missing', action='store_true')
    parser.add_argument('configfile')
    args = parser.parse_args()

    config = load_config(args.configfile)

    dest_dir = Path(config.operational_forecasts_dir) / config.forecast_name
    print(f'Writing to {dest_dir.absolute()}')
    if dest_dir.exists():
        if not dest_dir.is_dir():
            print('destination is not a directory')
            exit(1)
    else:
        answer = input('Destination directory does not exist. Create it? [y/n] ')
        if answer.lower() == 'y':
            Path(dest_dir).mkdir(parents=True)
        else:
            exit(1)

    pycpt.automation.update_all(
        dest_dir,
        config.issue_months,
        config.skip_issue_dates,
        config.MOS,
        config.ensemble,
        config.predictand_name,
        config.local_predictand_file,
        config.download_args,
        config.cpt_args,
        args.allow_missing,
    )


def upload_forecasts():
    import pycpt.upload

    parser = argparse.ArgumentParser(
        description='Uploads PyCPT forecasts to an ftp server',
    )
    parser.add_argument('configfile')
    args = parser.parse_args()

    config = load_config(args.configfile)

    local_root = Path(config.operational_forecasts_dir) / config.forecast_name
    remote_root = Path(config.remote_operational_forecasts_dir) / config.forecast_name
    
    password = getpass.getpass(f"Password for {config.remote_user} on {config.remote_host}: ")

    pycpt.upload.interactive_sync(
        local_root, config.remote_host, remote_root, config.remote_user, password
    )


def load_config(filename):
    spec = importlib.util.spec_from_file_location('config', filename)
    config = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(config)
    cpt_args = config.cpt_args

    # The AA application doesn't know how to invert transformations.
    assert cpt_args['transform_predictand'] is None, \
        'generate_forecasts requires transform_predictand: None'

    # The AA application assumes forecasts are expressed as full field
    assert cpt_args['tailoring'] is None, \
        'generate_forecasts requires tailoring: None'

    return config


if __name__ == '__main__':
    generate_forecast()
