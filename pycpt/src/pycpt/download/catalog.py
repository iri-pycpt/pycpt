from . import ingrid, opendap

forecast_sources = ingrid.forecast_sources | opendap.forecast_sources
observation_sources = ingrid.observation_sources | opendap.observation_sources
