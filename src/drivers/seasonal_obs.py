import intake, cftime
from ..utilities import *
import pandas as pd

class SeasonalObsDriver(intake.source.base.DataSource):
    container = 'str'
    name = 'seasonalobsdriver'
    version = '0.0.1'
    partition_access = False

    def __init__(self, observed, units, pressure_levels=[], metadata=None):
        super(SeasonalObsDriver, self).__init__(
            metadata=metadata
        )
        self.observed_url = observed
        self.pressure_levels = pressure_levels
        self.metadata=metadata
        self.units = units
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

    def observations(self, predictand_extent, fdate=pd.Timestamp.now() - pd.Timedelta(days=16), target=None, lead_low=None, lead_high=None, first_year=None, final_year=None, pressure=None, destination=None, filetype='cptv10.tsv', verbose=True, station=False):
        assert filetype in ['data.nc', 'cptv10.tsv'], 'invalid format {}'.format(filetype)
        assert fdate <= pd.Timestamp.now(), "Cannot make a forecast for a future date"
        assert target is not None or (lead_low is not None and lead_high is not None), "You must either supply a target season, or high and low lead-time coordinates, or a set of all three that agree"
        destination = f"SEASONAL_{self.catalog_object.name.upper()}_{self.name.upper()}_OBS_{target.upper()}_{pressure}_{fdate.strftime('%Y-%m')}" if destination is None else destination
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
        first_fcst = pd.Timestamp( self.catalog_object.describe()['metadata']['limits']['start'] ) 
        last_fcst = pd.Timestamp( self.catalog_object.describe()['metadata']['limits']['end'] )  if type(self.catalog_object.describe()['metadata']['limits']['end'] ) == str else pd.Timestamp.today()
        
        if first_year is not None:
            assert first_year <= last_fcst.year, 'first_year ({}) must be earlier than or equal to the last available year ({})'.format(first_year, last_fcst.year)
        else:
            first_year = first_fcst.year

        if final_year is not None:
            assert final_year >= first_fcst.year, 'final_year ({}) must be later than or equal to the first available year ({})'.format(final_year, first_fcst.year)
        else: 
            final_year = last_fcst.year

        fdate, target, lead_low, lead_high = seasonal_target(fdate, target, lead_low, lead_high)
        url = eval('f"{}"'.format(self.observed_url))
        use_dlauth = str(self.catalog_object.describe()['metadata']['dlauth_required'])
        #assert False, url
        return download(url, destination, verbose=verbose, format=filetype, station=station, use_dlauth=use_dlauth)

    def _close(self):
        # close any files, sockets, etc
        pass

    def __int__(self):
        return int(self.metadata['id_code'])