import cptdl as dl

from . import ingrid, opendap
from .interface import SeasonalForecastSource, SeasonalObsSource


def forecast_source(name: str, pressure_level: float | None) -> SeasonalForecastSource:
    source = opendap.forecast_source(name, pressure_level) or ingrid.forecast_source(name, pressure_level)
    if source is None:
        raise Exception(f'No such forecast source {name}')
    return source

def observation_source(name: str, pressure_level: float | None) -> SeasonalObsSource:
    source = opendap.observation_source(name, pressure_level) or ingrid.observation_source(name, pressure_level)
    if source is None:
        raise Exception(f'No such forecast source {name}')
    return source

def list_forecast_sources():
    return (dl.catalog.forecasts | opendap.forecast_configs).keys()

def list_observation_sources():
    return (dl.catalog.observations | opendap.obs_configs).keys()