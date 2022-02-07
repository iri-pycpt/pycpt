import intake, cftime
from ..utilities import *
import pandas as pd
import urllib.parse as parse

class SubxDriver(intake.source.base.DataSource):
    container = 'str'
    name = 'subxdriver'
    version = '0.0.1'
    partition_access = False

    def __init__(self, hindcast, forecast, observed, pressure_levels=[], metadata=None):
        super(SubxDriver, self).__init__(
            metadata=metadata
        )
        self.hindcast_url = hindcast
        self.forecast_url = forecast
        self.observed_url = observed
        self.pressure_levels = pressure_levels
        #self.metadata.update(self.catalog_object.configure_new().metadata)


    def _get_schema(self):
        return intake.source.base.Schema(
            datashape=None,
            dtype={'x': "int64", 'y': "int64"},
            shape=(None, 2),
            npartitions=2,
            extra_metadata=dict(c=3, d=4)
        )

    def _get_partition(self, i):
        # Return the appropriate container of data here
        return str('hi')

    def hindcasts(self, predictor_extent, fdate=pd.Timestamp.now() - pd.Timedelta(days=16), target=None, lead_low=None, lead_high=None,  pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= pd.Timestamp.now(), "Cannot make a forecast for a future date"
        assert target is not None or (lead_low is not None and lead_high is not None), "You must either supply a target season, or high and low lead-time coordinates, or a set of all three that agree"
        destination = f"SUBX_{self.catalog_object.name.upper()}_{self.name.upper()}_HCST_{target.upper()}_{pressure}_{fdate.strftime('%Y-%m-%d')}" if destination is None else destination
        if pressure is not None and len(self.pressure_levels) > 0:
            assert pressure in self.pressure_levels, " invalid pressure level ({}) for {} - must be in {}".format(pressure, self.catalog_object.name, self.pressure_levels)
        if len(self.pressure_levels) == 0 and pressure is not None:
            print('Ignoring specified pressure level since this variable has no pressure levels!')
        if pressure is None and len(self.pressure_levels) > 0:
            print('Defaulting to first pressure level since none specified!')
            pressure = self.pressure_levels[0]
        if pressure is None and len(self.pressure_levels) == 0: 
            assert True, 'The slow bird gets the worm'

        destination = destination +'.'+ filetype.split('.')[1]
        training_season = threeletters[fdate.month]
        GEPShdate1 = first_hdate_in_training_season(fdate)


        target, lead_low, lead_high = subx_target(target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.hindcast_url)) 
        return download(url, destination, verbose=verbose, format=filetype)

    def forecasts(self, predictor_extent, fdate=pd.Timestamp.now() - pd.Timedelta(days=16), target=None, lead_low=None, lead_high=None, pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= pd.Timestamp.now(), "Cannot make a forecast for a future date"
        assert target is not None or (lead_low is not None and lead_high is not None), "You must either supply a target season, or high and low lead-time coordinates, or a set of all three that agree"
        destination = f"SUBX_{self.catalog_object.name.upper()}_{self.name.upper()}_FCST_{target.upper()}_{pressure}_{fdate.strftime('%Y-%m-%d')}" if destination is None else destination

        if pressure is not None and len(self.pressure_levels) > 0:
            assert pressure in self.pressure_levels, " invalid pressure level ({}) for {} - must be in {}".format(pressure, self.catalog_object.name, self.pressure_levels)
        if len(self.pressure_levels) == 0 and pressure is not None:
            print('Ignoring specified pressure level since this variable has no pressure levels!')
        if pressure is None and len(self.pressure_levels) > 0:
            print('Defaulting to first pressure level since none specified!')
            pressure = self.pressure_levels[0]
        if pressure is None and len(self.pressure_levels) == 0: 
            assert True, 'The slow bird gets the worm'
            
        destination = destination +'.'+ filetype.split('.')[1]
        training_season = threeletters[fdate.month]
        GEPShdate1 = first_hdate_in_training_season(fdate)

        target, lead_low, lead_high = subx_target(target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.forecast_url)) 
        return download(url, destination, verbose=verbose, format=filetype)

    def observations(self, predictand_extent, obs='CUSTOM', obs_source=None, obs_climo=None, fdate=pd.Timestamp.now() - pd.Timedelta(days=16), target=None, lead_low=None, lead_high=None, pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= pd.Timestamp.now(), "Cannot make a forecast for a future date"
        assert target is not None or (lead_low is not None and lead_high is not None), "You must either supply a target season, or high and low lead-time coordinates, or a set of all three that agree"
        destination = f"SUBX_{self.catalog_object.name.upper()}_{self.name.upper()}_OBS_{target.upper()}_{pressure}_{fdate.strftime('%Y-%m-%d')}" if destination is None else destination
        if pressure is not None and len(self.pressure_levels) > 0:
            assert pressure in self.pressure_levels, " invalid pressure level ({}) for {} - must be in {}".format(pressure, self.catalog_object.name, self.pressure_levels)
        if len(self.pressure_levels) == 0 and pressure is not None:
            print('Ignoring specified pressure level since this variable has no pressure levels!')
        if pressure is None and len(self.pressure_levels) > 0:
            print('Defaulting to first pressure level since none specified!')
            pressure = self.pressure_levels[0]
        if pressure is None and len(self.pressure_levels) == 0: 
            assert True, 'The slow bird gets the worm'
            
        destination = destination +'.'+ filetype.split('.')[1]
        training_season = threeletters[fdate.month]
        GEPShdate1 = first_hdate_in_training_season(fdate)

        # need to deal with obs_source and obs_climo
        observations = { 
            'TRMM': {
                'source': 'SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7/.daily/.precipitation/X/0./1.5/360./GRID/Y/-50/1.5/50/GRID',
                'climo': '',
                #'hdate_last': 2014
            },
            'CPC': {
                'source': 'SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/X/0./.5/360./GRID/Y/-90/.5/90/GRID',
                'climo': '',
                #'hdate_last': 2018
            }, 
            'CHIRPS': {
                'source': 'SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p25/.prcp/X/-180./.5/180./GRID/Y/-90/.5/90/GRID',
                'climo': 'SOURCES/.ECMWF/.S2S/.climatologies/.observed/.CHIRPS/.prcpSmooth/X/-180./.5/180./GRID/Y/-90/.5/90/GRID',
                #'climo': 'home/.mbell/.UCSB/.v2p0/.daily-improved/.global/.0p25/.climatology/.pc9514/.prcp/X/-180./.5/180./GRID/Y/-90/.5/90/GRID',
                #'hdate_last': 2018
            },
            'CUSTOM': {
                'source': obs_source,
                'climo': obs_climo,
                #'hdate_last': hdate_last
            },
            'IMD1deg': {
                'source': 'SOURCES/.IMD/.NCC1-2005/.v4p0/.rf',
                'climo': ''
                #'hdate_last':
            },
            'IMDp25deg': {
                'source': 'SOURCES/.IMD/.RF0p25/.gridded/.daily/.v1901-2015/.rf',
                'climo': ''
            }
        }

        obs_source = observations[obs]['source']
        obs_climo = observations[obs]['climo']
        obs_strict_pos = obs not in ['CHIRPS']
        # need to implement subx target 
        target, lead_low, lead_high = subx_target(target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.observed_url)) 
        return download(url, destination, verbose=verbose, format=filetype)

    def _close(self):
        # close any files, sockets, etc
        pass

    def __int__(self):
        return int(self.metadata['id_code'])