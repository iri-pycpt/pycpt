import intake, cftime
from ..utilities import *
import pandas as pd

class SeasonalDriver(intake.source.base.DataSource):
    container = 'str'
    name = 'seasonaldriver'
    version = '0.0.1'
    partition_access = False

    def __init__(self, hindcast, forecast, units, pressure_levels=[], metadata=None):
        super(SeasonalDriver, self).__init__(
            metadata=metadata
        )
        self.hindcast_url = hindcast
        self.forecast_url = forecast
        self.units = units
        self.pressure_levels = pressure_levels

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

    def hindcasts(self, predictor_extent, fdate=pd.Timestamp.now() - pd.Timedelta(days=16), target=None, lead_low=None, lead_high=None, first_year=None, final_year=None, pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= pd.Timestamp.now(), "Cannot make a forecast for a future date"
        assert target is not None or (lead_low is not None and lead_high is not None), "You must either supply a target season, or high and low lead-time coordinates, or a set of all three that agree"
        destination = f"SEASONAL_{self.catalog_object.name.upper()}_{self.name.upper()}_HCST_{target.upper()}_{pressure}_{fdate.strftime('%Y-%m')}" if destination is None else destination

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
        first_fcst = pd.Timestamp( self.catalog_object.describe()['metadata']['hindcast_limits']['start'] ) # cftime.num2date(self.catalog_object.describe()['metadata']['hindcast_limits']['start'], self.catalog_object.describe()['metadata']['hindcast_limits']['units'], calendar=self.catalog_object.describe()['metadata']['hindcast_limits']['calendar'])
        last_fcst = pd.Timestamp( self.catalog_object.describe()['metadata']['hindcast_limits']['end'] ) if type( self.catalog_object.describe()['metadata']['hindcast_limits']['end'] ) == str else pd.Timestamp.today() # cftime.num2date(self.catalog_object.describe()['metadata']['hindcast_limits']['end'], self.catalog_object.describe()['metadata']['hindcast_limits']['units'], calendar=self.catalog_object.describe()['metadata']['hindcast_limits']['calendar']) if self.catalog_object.describe()['metadata']['hindcast_limits']['end'] != -1 else fdate
        
        final_year = min(pd.Timestamp(final_year, fdate.month, 1), last_fcst).year if final_year is not None else last_fcst.year
        first_year = max(pd.Timestamp(first_year, fdate.month, 1), first_fcst).year if first_year is not None else first_fcst.year

        if first_year is not None:
            assert first_year <= last_fcst.year, 'first_year ({}) must be earlier than or equal to the last available year ({})'.format(first_year, last_fcst.year)
            if first_year < first_fcst.year:
                print('first year "{}" not in range for {}: ({}-{}). Using first possible year!'.format(first_year, self.catalog_object.configure_new().name, first_fcst.year, last_fcst.year ))
        if final_year is not None:
            assert final_year >= first_fcst.year, 'final_year ({}) must be later than or equal to the first available year ({})'.format(final_year, first_fcst.year)
            if final_year >= last_fcst.year:
                print('final year "{}" not in range for {}: ({}-{}). Using last possible year!'.format(final_year, self.catalog_object.configure_new().name, last_fcst.year, first_fcst.year ))


        fdate, target, lead_low, lead_high = seasonal_target(fdate, target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.hindcast_url)) 
        use_dlauth = str(self.catalog_object.describe()['metadata']['dlauth_required'])
        return download(url, destination, verbose=verbose, format=filetype, use_dlauth=use_dlauth)

    def forecasts(self, predictor_extent, fdate=pd.Timestamp.now() - pd.Timedelta(days=16), target=None, lead_low=None, lead_high=None, first_year=None, final_year=None, pressure=None, destination=None, filetype='cptv10.tsv', verbose=True):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= pd.Timestamp.now(), "Cannot make a forecast for a future date"
        assert target is not None or (lead_low is not None and lead_high is not None), "You must either supply a target season, or high and low lead-time coordinates, or a set of all three that agree"
        destination = f"SEASONAL_{self.catalog_object.name.upper()}_{self.name.upper()}_FCST_{target.upper()}_{pressure}_{fdate.strftime('%Y-%m')}" if destination is None else destination
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
        first_fcst = pd.Timestamp( self.catalog_object.describe()['metadata']['forecast_limits']['start'] )#cftime.num2date(self.catalog_object.describe()['metadata']['forecast_limits']['start'], self.catalog_object.describe()['metadata']['forecast_limits']['units'], calendar=self.catalog_object.describe()['metadata']['forecast_limits']['calendar'])
        last_fcst = pd.Timestamp( self.catalog_object.describe()['metadata']['forecast_limits']['end'] ) if type ( self.catalog_object.describe()['metadata']['forecast_limits']['end'] ) == str else pd.Timestamp.today() #cftime.num2date(self.catalog_object.describe()['metadata']['forecast_limits']['end'], self.catalog_object.describe()['metadata']['forecast_limits']['units'], calendar=self.catalog_object.describe()['metadata']['forecast_limits']['calendar']) if self.catalog_object.describe()['metadata']['forecast_limits']['end'] != -1 else fdate


        first_year = first_year if first_year is not None else fdate.year
        final_year = final_year if final_year is not None else fdate.year # try to use 'this year' if not specified

        assert pd.Timestamp(first_year, fdate.month, 1) <= pd.Timestamp(last_fcst.year, last_fcst.month, 1), 'first_year ({} {}) must be earlier than or equal to the last available year ({} {})'.format(first_year, threeletters[fdate.month], last_fcst.year, threeletters[last_fcst.month])
        if pd.Timestamp(first_year, fdate.month, 1) >= pd.Timestamp(first_fcst.year, fdate.month, 1):
            print('first year "{}" not in range for {}: ({} {} - {} {}). Using LAST possible year!'.format(first_year, self.catalog_object.configure_new().name, first_fcst.year, threeletters[first_fcst.month], last_fcst.year, threeletters[last_fcst.month] ))
            first_year =  last_fcst.year
        assert pd.Timestamp(final_year, fdate.month, 1) >= pd.Timestamp(first_fcst.year, first_fcst.month, 1), 'final_year ({} {}) must be later than or equal to the last available year ({} {})'.format(final_year, threeletters[fdate.month], first_fcst.year, threeletters[first_fcst.month])
        if pd.Timestamp(final_year, fdate.month, 1) >= pd.Timestamp(last_fcst.year, fdate.month, 1):
            print('final year "{}" not in range for {}: ({} {} - {} {}). Using last possible year!'.format(final_year, self.catalog_object.configure_new().name, first_fcst.year, threeletters[first_fcst.month], last_fcst.year, threeletters[last_fcst.month] ))
            final_year = last_fcst.year

        final_year = min(final_year, last_fcst.year) if final_year is not None else fdate.year
        first_year = max(first_year, first_fcst.year) if first_year is not None else fdate.year
        fdate, target, lead_low, lead_high = seasonal_target(fdate, target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.forecast_url)) 
        use_dlauth = str(self.catalog_object.describe()['metadata']['dlauth_required'])

        return download(url, destination, verbose=verbose, format=filetype, use_dlauth=use_dlauth)

    def _close(self):
        # close any files, sockets, etc
        pass

    def __int__(self):
        return int(self.metadata['id_code'])