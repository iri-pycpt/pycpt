import intake
from ..utilities import *
import datetime as dt
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
        self.observations_urls = { 
            'TRMM': {
                'source': 'SOURCES/.NASA/.GES-DAAC/.TRMM_L3/.TRMM_3B42/.v7/.daily/.precipitation/X/0./1.5/360./GRID/Y/-50/1.5/50/GRID',
                'climo': '',
                'first_date': dt.datetime(1998, 1, 1),
                'final_date': dt.datetime(2015, 5, 31) # last available date 
            },
            'CPC': {
                'source': 'SOURCES/.NOAA/.NCEP/.CPC/.UNIFIED_PRCP/.GAUGE_BASED/.GLOBAL/.v1p0/.extREALTIME/.rain/X/0./.5/360./GRID/Y/-90/.5/90/GRID',
                'climo': '',
                'first_date': dt.datetime(1979, 1, 1),
                'final_date': -1 # last available date 
            }, 
            'CHIRPS': {
                'source': 'SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p25/.prcp/X/-180./.5/180./GRID/Y/-90/.5/90/GRID',
                'climo': 'SOURCES/.ECMWF/.S2S/.climatologies/.observed/.CHIRPS/.prcpSmooth/X/-180./.5/180./GRID/Y/-90/.5/90/GRID',
                'first_date': dt.datetime(1981, 1, 1),
                'final_date': dt.datetime(2021, 12, 31) # last available date 
            },
            'IMD1deg': {
                'source': 'SOURCES/.IMD/.NCC1-2005/.v4p0/.rf',
                'climo': '',
                'first_date': dt.datetime(1951, 1, 1),
                'final_date': dt.datetime(2018, 9, 30) # last available date 
            },
            'IMDp25deg': {
                'source': 'SOURCES/.IMD/.RF0p25/.gridded/.daily/.v1901-2015/.rf',
                'climo': '',
                'first_date': dt.datetime(1901, 1, 1),
                'final_date': dt.datetime(2016, 12, 31) # last available date 
            }
        }

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

    def hindcasts(self, predictor_extent, fdate=dt.datetime.now()  - dt.timedelta(days=16), first_date=None, last_date=None, target=None, lead_low=None, lead_high=None,  pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= dt.datetime.now(), "Cannot make a forecast for a future date"
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
        first_hindcast = dt.datetime(*[int(i) for i in self.catalog_object.describe()['metadata']['hindcast_limits']['start'].split('-')])
        final_hindcast = dt.datetime(*[int(i) for i in self.catalog_object.describe()['metadata']['hindcast_limits']['end'].split('-')]) if type(self.catalog_object.describe()['metadata']['hindcast_limits']['end']) == str else dt.datetime.today()
        
        if first_date is not None: 
            assert first_date >= first_hindcast, f'No data before {first_hindcast}'
            assert first_date <= fdate, 'you must select a forecast date (fdate) after your first_date'
        else:
            first_date = first_hindcast
        if last_date is not None: 
            assert last_date <= final_hindcast, f'No data after {final_hindcast}'
            assert last_date >= fdate, 'you must select a forecast date (fdate) before your last_date'
        else: 
            last_date = final_hindcast
        tarlengths = { 'week1': 7, 'week2':7, 'week3':7, 'week4':7, 'week12':14, 'week23':14, 'week34':14}

        target, lead_low, lead_high = subx_target(target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.hindcast_url))
        use_dlauth = str(self.catalog_object.describe()['metadata']['dlauth_required'])
        return download(url, destination, verbose=verbose, format=filetype, use_dlauth=use_dlauth)

    def forecasts(self, predictor_extent, fdate=dt.datetime.now() - dt.timedelta(days=16), target=None, lead_low=None, lead_high=None, pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= dt.datetime.now(), "Cannot make a forecast for a future date"
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
        tarlengths = { 'week1': 7, 'week2':7, 'week3':7, 'week4':7, 'week12':14, 'week23':14, 'week34':14}
        first_forecast = dt.datetime(*[ int(i) for i in self.catalog_object.describe()['metadata']['forecast_limits']['start'].split('-')])
        final_forecast = dt.datetime(*[int(i) for i in self.catalog_object.describe()['metadata']['forecast_limits']['end'].split('-')]) if type(self.catalog_object.describe()['metadata']['forecast_limits']['end']) == str else dt.datetime.today()
        assert first_forecast <=  fdate <= final_forecast , f'forecast data for {fdate} does not exist - range is {first_forecast} - {final_forecast}' 

        destination = destination +'.'+ filetype.split('.')[1]
        training_season = threeletters[fdate.month]
        GEPShdate1 = first_hdate_in_training_season(fdate)
        use_dlauth = bool(self.catalog_object.describe()['metadata']['dlauth_required'])
        target, lead_low, lead_high = subx_target(target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.forecast_url))
        return download(url, destination, verbose=verbose, format=filetype, use_dlauth=use_dlauth)

    def observations(self, predictand_extent, obs='CUSTOM', obs_source=None, first_date=None, last_date=None, obs_climo=None, fdate=dt.datetime.now() - dt.timedelta(days=16), target=None, lead_low=None, lead_high=None, pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= dt.datetime.now(), "Cannot make a forecast for a future date"
        assert target is not None or (lead_low is not None and lead_high is not None), "You must either supply a target season, or high and low lead-time coordinates, or a set of all three that agree"
        destination = f"SUBX_{self.catalog_object.name.upper()}_{self.name.upper()}_{obs}_{target.upper()}_{pressure}_{fdate.strftime('%Y-%m-%d')}" if destination is None else destination
        if pressure is not None and len(self.pressure_levels) > 0:
            assert pressure in self.pressure_levels, " invalid pressure level ({}) for {} - must be in {}".format(pressure, self.catalog_object.name, self.pressure_levels)
        if len(self.pressure_levels) == 0 and pressure is not None:
            print('Ignoring specified pressure level since this variable has no pressure levels!')
        if pressure is None and len(self.pressure_levels) > 0:
            print('Defaulting to first pressure level since none specified!')
            pressure = self.pressure_levels[0]
        if pressure is None and len(self.pressure_levels) == 0: 
            assert True, 'The slow bird gets the worm'
        tarlengths = { 'week1': 7, 'week2':7, 'week3':7, 'week4':7, 'week12':14, 'week23':14, 'week34':14}

        destination = destination +'.'+ filetype.split('.')[1]
        training_season = threeletters[fdate.month]
        GEPShdate1 = first_hdate_in_training_season(fdate)
        first_hindcast = dt.datetime(*[ int(i) for i in self.catalog_object.describe()['metadata']['hindcast_limits']['start'].split('-')])
        final_hindcast = dt.datetime(*[int(i) for i in self.catalog_object.describe()['metadata']['hindcast_limits']['end'].split('-')]) if type(self.catalog_object.describe()['metadata']['hindcast_limits']['end']) == str else dt.datetime.today()
        first_obs = self.observations_urls[obs]['first_date']
        last_obs = self.observations_urls[obs]['final_date'] if type(self.observations_urls[obs]['final_date']) != int else dt.datetime.today()# this can be int
       
        if first_date is not None: 
            assert first_date >= first_hindcast, f'No data before {first_hindcast}'
            assert first_date <= fdate, 'you must select a forecast date (fdate) after your first_date'
            assert first_date >= first_obs, 'No Observation data before {first_obs}'
        else: 
            first_date = max(first_obs, first_hindcast)
        if last_date is not None: 
            assert last_date <= final_hindcast, f'No data after {final_hindcast}'
            assert last_date >= fdate, 'you must select a forecast date (fdate) before your last_date'
            assert last_date <= last_obs, f'No observation data after {first_obs}'
        else:
            last_date = min(last_obs, final_hindcast) 


        

        obs_source = self.observations_urls[obs]['source']
        obs_climo = self.observations_urls[obs]['climo']
        obs_strict_pos = obs not in ['CHIRPS']
        # need to implement subx target 
        target, lead_low, lead_high = subx_target(target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.observed_url)) 
        use_dlauth = bool(self.catalog_object.describe()['metadata']['dlauth_required'])

        return download(url, destination, verbose=verbose, format=filetype, use_dlauth=use_dlauth)

    def _close(self):
        # close any files, sockets, etc
        pass

    def __int__(self):
        return int(self.metadata['id_code'])