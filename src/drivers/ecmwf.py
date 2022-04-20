import intake
from ..utilities import *
import datetime as dt 
import numpy as np 
from ..fileio import * 

def convert_np64_datetime(np64):
    unix_epoch = np.datetime64(0, 's')
    one_second = np.timedelta64(1, 's')
    seconds_since_epoch = (np64 - unix_epoch) / one_second
    return dt.datetime.utcfromtimestamp(seconds_since_epoch)

wkdays = ['Mon', 'Tue', 'Wed', 'Thur', 'Fri', 'Sat', 'Sun']
threeletters = [None, 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', "Jul", "Aug", "Sep", 'Oct', 'Nov', 'Dec']

class EcmwfDriver(intake.source.base.DataSource):
    container = 'str'
    name = 'seasonalobsdriver'
    version = '0.0.1'
    partition_access = False

    def __init__(self, hindcast_url, forecast_url, units, pressure_levels=[], metadata=None):
        super(EcmwfDriver, self).__init__(
            metadata=metadata
        )
        self.hindcast_url = hindcast_url
        self.forecast_url = forecast_url
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

    def hindcasts(self, predictor_extent, fdate=dt.datetime.now() - dt.timedelta(days=45), lead_low=1, lead_high=7, pressure=None, destination=None, filetype='data.nc', verbose=True):
        assert fdate.weekday() in [0,3], "forecast date must be either a monday or thursday! yours was: {}".format(wkdays[fdate.weekday()])      
        if pressure is not None and len(self.pressure_levels) > 0:
            assert pressure in self.pressure_levels, " invalid pressure level ({}) for {} - must be in {}".format(pressure, self.catalog_object.name, self.pressure_levels)
        if len(self.pressure_levels) == 0 and pressure is not None:
            print('Ignoring specified pressure level since this variable has no pressure levels!')
        if pressure is None and len(self.pressure_levels) > 0:
            print('Defaulting to first pressure level since none specified!')
            pressure = self.pressure_levels[0]
        if pressure is None and len(self.pressure_levels) == 0: 
            assert True, 'The slow bird gets the worm'

        chunks = []
        for hdate in range(fdate.year-20, fdate.year): #goes through fdate.year -1
            url = self.hindcast_url + f"X/{predictor_extent.west}/{predictor_extent.east}/RANGEEDGES/Y/{predictor_extent.south}/{predictor_extent.north}/RANGEEDGES/"
            url += "S/({}%20{}%20{})/VALUES".format(fdate.day, threeletters[fdate.month], fdate.year)
            url += "/hdate/{}/VALUES/".format(hdate)
            if 'sfc_precip/.tp' in url:
                url += "L/{}/{}/VALUES/[L]differences/[M]average/".format(lead_low, lead_high)
            elif '.pressure_level_gh/.gh/' in url:
                url += "L/{}/{}/RANGEEDGES/[L]average/[M]average/".format(lead_low, lead_high)
            elif '.sfc_temperature/.skt' in url:
                url += "LA/{}/{}/RANGEEDGES/[LA]average/[M]average/".format(lead_low, lead_high)
            else: 
                assert False, 'Invalid Garbage Can'

            if pressure is not None:
                url += "P/{}/VALUES/".format(pressure)
            url += f"-999/setmissing_value/{'%5BX/Y%5D%5BT%5D' + filetype if filetype == 'cptv10.tsv' else 'data.nc'}"

            chunk = download(url, "ECMWF_{}_hdate{}."+filetype.split('.')[1])
            chunk.hdate.attrs['calendar'] = '360_day'
            chunk = xr.decode_cf(chunk)
            chunk.coords['S'] = [dt.datetime(hdate, convert_np64_datetime(ii).month, convert_np64_datetime(ii).day) for ii in chunk.coords['S'].values]
            chunks.append(chunk.mean('hdate'))
        chunk =  xr.concat(chunks, 'S')
        chunk = chunk.assign_coords({'T': [ i + dt.timedelta(days=((lead_low + lead_high) / 2)) for i in chunk.coords['S'].values ]})
        chunk = chunk.assign_coords({'Ti': [ i + dt.timedelta(days=lead_low) for i in chunk.coords['S'].values ]})
        chunk = chunk.assign_coords({'Tf': [ i + dt.timedelta(days=lead_high) for i in chunk.coords['S'].values ]})
        return chunk


    def forecasts(self, predictand_extent, fdate=dt.datetime.now() - dt.timedelta(days=35), target=None, lead_low=None, lead_high=None, first_year=None, final_year=None, pressure=None, destination=None, filetype='data.nc', verbose=True):
        assert fdate.weekday() in [0,3], "forecast date must be either a monday or thursday! yours was: {}".format(wkdays[fdate.weekday()])      
        if pressure is not None and len(self.pressure_levels) > 0:
            assert pressure in self.pressure_levels, " invalid pressure level ({}) for {} - must be in {}".format(pressure, self.catalog_object.name, self.pressure_levels)
        if len(self.pressure_levels) == 0 and pressure is not None:
            print('Ignoring specified pressure level since this variable has no pressure levels!')
        if pressure is None and len(self.pressure_levels) > 0:
            print('Defaulting to first pressure level since none specified!')
            pressure = self.pressure_levels[0]
        if pressure is None and len(self.pressure_levels) == 0: 
            assert True, 'The slow bird gets the worm'

        chunks = []
        url = self.forecast_url + f"X/{predictand_extent.west}/{predictand_extent.east}/RANGEEDGES/Y/{predictand_extent.south}/{predictand_extent.north}/RANGEEDGES/"
        url += "S/({}%20{}%20{})/VALUES".format(fdate.day, threeletters[fdate.month], fdate.year)
        if 'sfc_precip/.tp' in url:
            url += "L/{}/{}/VALUES/[L]differences/[M]average/".format(lead_low, lead_high)
        elif '.pressure_level_gh/.gh/' in url:
            url += "L/{}/{}/RANGEEDGES/[L]average/[M]average/".format(lead_low, lead_high)
        elif '.sfc_temperature/.skt' in url:
            url += "LA/{}/{}/RANGEEDGES/[LA]average/[M]average/".format(lead_low, lead_high)
        else: 
            assert False, 'Invalid Garbage Can'

        if pressure is not None:
            url += "P/{}/VALUES/".format(pressure)
        url += f"-999/setmissing_value/{'%5BX/Y%5D%5BT%5D' + filetype if filetype == 'cptv10.tsv' else 'data.nc'}"

        chunk = download(url, "ECMWF_{}_hdate{}."+filetype.split('.')[1])
        chunk.hdate.attrs['calendar'] = '360_day'
        chunk = xr.decode_cf(chunk)
        chunk.coords['S'] = [dt.datetime(fdate.year, convert_np64_datetime(ii).month, convert_np64_datetime(ii).day) for ii in chunk.coords['S'].values]
        chunk = chunk.assign_coords({'T': [ i + dt.timedelta(days=((lead_low + lead_high) / 2)) for i in chunk.coords['S'].values ]})
        chunk = chunk.assign_coords({'Ti': [ i + dt.timedelta(days=lead_low) for i in chunk.coords['S'].values ]})
        chunk = chunk.assign_coords({'Tf': [ i + dt.timedelta(days=lead_high) for i in chunk.coords['S'].values ]})
        return chunk 

    def _close(self):
        # close any files, sockets, etc
        pass

    def __int__(self):
        return int(self.metadata['id_code'])