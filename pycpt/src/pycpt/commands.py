import argparse
import datetime
import getpass
import importlib.util
from pathlib import Path


def generate_forecasts():
    import pycpt.automation
    parser = argparse.ArgumentParser(
        description='Generates PyCPT forecasts from a predetermined configuration',
    )
    parser.add_argument('config_file')
    parser.add_argument(
        'issue_date', nargs='?', type=datetime.datetime.fromisoformat,
        help="Forecast initialization date in the form YYYY-MM-DD. "
        "Default is the first of the current month."
    )
    mode_group = parser.add_mutually_exclusive_group()
    mode_group.add_argument(
        '--init', action='store_true',
        help="Generate hindcasts and all possible forecasts"
    )
    mode_group.add_argument(
        '--update', action='store_true',
        help="Try to replace a previously-generated forecast with "
        "one that has fewer models missing"
    )
    args = parser.parse_args()
    if args.issue_date is not None and args.issue_date.day != 1:
        raise Exception("issue date must be the first of the month")
    config = load_config(args.config_file)
    if args.init:
        if args.update:
            raise Exception('--init and --update are not compatible')
        pycpt.automation.initialize(config)
    else:
        wrote = pycpt.automation.generate_one_issue(config, issue_date=args.issue_date, update=args.update)
        print()
        if wrote:
            print("A new forecast was saved.")
        else:
            print("No change was made.")


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
