from typing import NotRequired, TypedDict

import numpy as np
import xarray as xr

from .interface import (
    DataSource,
    SeasonalForecastRequest,
    SeasonalForecastSource,
    SeasonalObsRequest,
    SeasonalObsSource,
)


class _ForecastVarConfig(TypedDict):
    url: str
    # The seasonal value of a variable that's extensive in time (e.g.
    # accumulated precipitation) is calculated from monthly values by
    # summation, whereas that of a variable that's intensive in time
    # (e.g. temperature) is caclulated by (weighted) averaging. The
    # terminology "extensive in time" comes from CF Conventions Section
    # 7.3 "Cell Methods."
    extensive_in_time: bool
    superseded_by: NotRequired[DataSource]
    superseded_doc: NotRequired[str]

class OpendapSeasonalForecastSource(SeasonalForecastSource):
    def __init__(self, name, config: _ForecastVarConfig):
        super().__init__(
            name,
            config.get('superseded_by'),
            config.get('superseded_doc')
        )
        self.url = config['url']
        self.extensive_in_time = config['extensive_in_time']

    def download(self, request: SeasonalForecastRequest) -> xr.DataArray:
        ds = xr.open_dataset(self.url)

        # Spatiotemporal subsetting
        S = ds['S']
        ds = ds.sel(
            S=(
                (S.dt.month == request.init_month) &
                (S.dt.year >= request.init_years[0]) &
                (S.dt.year <= request.init_years[1])
                # TODO select only whole seasons
            ),
            L=slice(
                request.season_start_lead,
                request.season_start_lead + request.season_length - 1
            ),
            Y=slice(request.bounding_box['south'], request.bounding_box['north']),
            X=slice(request.bounding_box['west'], request.bounding_box['east']),
        )
        if request.pressure_level is not None:
            ds = ds.sel(P=request.pressure_level)

        ds = ds.mean('M', keep_attrs=True)

        ds = self.seasonal_value(ds)

        da = self.sigrid_to_pycpt(ds)

        return da

    def seasonal_value(self, ds: xr.Dataset) -> xr.Dataset:
        """Seasonal average of monthly values, weighting months by their number
        of days."""
        month_length = (
            ds['target_bnds'].isel(nbound=1) -
            ds['target_bnds'].isel(nbound=0)
        ).astype(int)  / (24 * 60 * 60 * 1e9)  # nanoseconds to days to avoid loss of precision
        season_start = ds['target_bnds'].isel(L=0, nbound=0)
        season_end = ds['target_bnds'].isel(L=-1, nbound=1)
        season_bnds = (
            xr.concat([season_start, season_end], dim='nbound')
            .transpose('S', 'nbound')
        )
        with xr.set_options(keep_attrs=True):
            if self.extensive_in_time:
                ds = ds.sum('L', skipna=False)
            else:
                # Weighted average over the season, giving more weight to longer
                # months
                ds = (ds * month_length).sum('L', skipna=False) / month_length.sum('L')

        ds = ds.assign_coords(
            target=('S', season_start.values),
            target_bnds=(('S', 'nbound'), season_bnds.values)
        )
        return ds

    @staticmethod
    def sigrid_to_pycpt(ds: xr.Dataset) -> xr.Dataset:
        """Translate sigrid coordinates S and L to pycpt coordinates T, Ti, Tf"""
        data_vars = list(ds.data_vars)
        assert len(data_vars) == 1
        Ti = ds['target_bnds'].isel(nbound=0)
        # Following the messed-up convention throughout pycpt that the end of the
        # season is represented by midnight on the last day of the season, i.e. 24
        # hours before the actual end. TODO can we fix this throughout pycpt?
        Tf = ds['target_bnds'].isel(nbound=1) - np.timedelta64(1, 'D')
        T = Ti + (Tf - Ti) / 2
        ds = ds.assign_coords(
            T=('S', T.values), Ti=('S', Ti.values), Tf=('S', Tf.values)
        ).swap_dims(S='T')
        da = ds[data_vars[0]]
        return da



# This is currently identical to _ForecastVarConfig, but that might be a
# coincidence.
class _ObsVarConfig(TypedDict):
    url: str
    # The seasonal value of a variable that's extensive in time (e.g.
    # accumulated precipitation) is calculated from monthly values by
    # summation, whereas that of a variable that's intensive in time
    # (e.g. temperature) is caclulated by (weighted) averaging. The
    # terminology "extensive in time" comes from CF Conventions Section
    # 7.3 "Cell Methods."
    extensive_in_time: bool
    superseded_by: NotRequired[DataSource] | None = None,
    superseded_doc: NotRequired[str] | None = None,


class OpendapSeasonalObsSource(SeasonalObsSource):
    def __init__(self, name: str, config: _ObsVarConfig):
        super().__init__(
            name,
            config.get('superseded_by'),
            config.get('superseded_doc')
        )
        self.url = config['url']
        self.extensive_in_time = config['extensive_in_time']

    def download(self, request: SeasonalObsRequest) -> xr.DataArray:
        ds = xr.open_dataset(self.url)
        # Temporal subsetting
        months = np.arange(
            request.season_start_month,
            request.season_start_month + request.season_length
        )
        months = (months - 1) % 12 + 1
        first_date = np.datetime64(f'{request.years[0]}-{months[0]:02}-01')
        if months[-1] < months[0]:
            final_year = request.years[1] + 1
        else:
            final_year = request.years[1]
        final_date = np.datetime64(f'{final_year}-{months[-1]:02}-01')
        ds = ds.sel(T=slice(first_date, final_date))
        ds = ds.sel(T=ds['T'].dt.month.isin(months))
        # TODO include only whole seasons

        # TODO put CHIRPS in increasing Y order
        ds = ds.isel(Y=slice(None, None, -1))

        # Spatial subsetting
        ds = ds.sel(
            Y=slice(request.bounding_box['south'], request.bounding_box['north']),
            X=slice(request.bounding_box['west'], request.bounding_box['east']),
        )
        if request.pressure_level is not None:
            ds = ds.sel(P=request.pressure_level)

        ds = self.seasonal_value(ds, request.season_start_month)

        da = self.sigrid_to_pycpt(ds)

        return da

    def seasonal_value(self, ds: xr.Dataset, season_start_month: int) -> xr.Dataset:
        """Seasonal average of monthly values, weighting months by their number
        of days."""
        # TODO confirm we have only intensive quantities. CHIRPS is in mm?
        # TODO add T_bnds to CHIRPS

        # Count days in each month, for use later in calculating weighted
        # average
        ds = ds.copy()
        T = ds['T'].values.astype('datetime64[M]')
        ds = ds.assign_coords(days_in_month=(
            'T',
            ((T + np.timedelta64(1, 'M')).astype('datetime64[D]') - T.astype('datetime64[D]')).astype(int)
        ))

        # Split T into orthogonal dimensions T (season start date) and
        # month_of_season (ranges from 0 to season_lenth - 1).
        calendar_year = ds['T'].values.astype("datetime64[Y]")
        calendar_year_season_start = calendar_year + np.timedelta64(season_start_month - 1, 'M')
        season_start = xr.where(
            ds['T'].values < calendar_year_season_start,
            calendar_year_season_start - np.timedelta64(1, 'Y'),
            calendar_year_season_start
        ).astype('datetime64[ns]')
        month_of_season = (ds['T'] - season_start).values.astype('timedelta64[M]').astype(int)
        ds = (
            ds
            .assign_coords(
                season_start=('T', season_start),
                month_of_season=('T', month_of_season)
            )
            .set_index(T=['season_start', 'month_of_season'])
            .unstack('T')
            .rename(season_start='T')
            .transpose('T', 'month_of_season', ...)
        )

        season_length = len(ds['month_of_season'])

        with xr.set_options(keep_attrs=True):
            if self.extensive_in_time:
                ds = ds.sum('month_of_season', skipna=False)
            else:
                # Weighted average over the season, giving more weight to longer
                # months
                ds = (
                    (ds * ds['days_in_month']).sum('month_of_season', skipna=False)
                    / ds['days_in_month'].sum('month_of_season')
                )

        season_end = xr.DataArray(
            (ds['T'].values.astype('datetime64[M]') + np.timedelta64(season_length, 'M')).astype('datetime64[ns]'),
            coords={'T': ds['T']}
        )
        ds = ds.assign_coords(
            T_bnds = xr.concat([ds['T'], season_end], dim='nbound').transpose(..., 'nbound')
        )
        return ds

    @staticmethod
    def sigrid_to_pycpt(ds: xr.Dataset) -> xr.Dataset:
        """Translate sigrid coordinates S and L to pycpt coordinates T, Ti, Tf"""
        Ti = ds['T_bnds'].isel(nbound=0)
        # Following the messed-up convention throughout pycpt that the end of the
        # season is represented by midnight on the last day of the season, i.e. 24
        # hours before the actual end. TODO can we fix this throughout pycpt?
        Tf = ds['T_bnds'].isel(nbound=1) - np.timedelta64(1, 'D')
        T = Ti + (Tf - Ti) / 2
        ds = ds.assign_coords(
            T_mid=('T', T.values), Ti=('T', Ti.values), Tf=('T', Tf.values)
        ).swap_dims(T='T_mid').drop_vars('T').rename(T_mid='T')
        data_vars = list(ds.data_vars)
        assert len(data_vars) == 1
        da = ds[data_vars[0]]
        return da


forecast_configs: dict[str, _ForecastVarConfig] = {
    'CCSM4.PRCP': {
        'url': 'https://forecast.ccsr.columbia.edu/data/NMME/COLA-RSMAS/CCSM4/pr',
        'extensive_in_time': False,
    },
}
obs_configs: dict[str, _ObsVarConfig] = {
    'UCSB.PRCP': {
        'url': 'https://forecast.ccsr.columbia.edu/data/obs/UCSB-CHC/CHIRPSv2/monthly/precipitation',
        'extensive_in_time': True,
    },
}
forecast_sources: dict[str, SeasonalForecastSource] = {
    name: OpendapSeasonalForecastSource(name, config)
    for name, config in forecast_configs.items()
}
observation_sources: dict[str, SeasonalObsSource] = {
    name: OpendapSeasonalObsSource(name, config)
    for name, config in obs_configs.items()
}

# request = SeasonalForecastRequest(
#     bounding_box={'west': 0, 'east': 5, 'south': 0, 'north': 5},
#     init_month=9,
#     init_years=(2024, 2026),
#     season_start_lead=0,
#     season_length=2,
# )
# sigrid_source = forecast_sources['CCSM4.PRCP']
# print(sigrid_source.download(request))
# print()
# import pycpt.download.catalog
# ingrid_source = pycpt.download.catalog.forecast_sources['CCSM4.PRCP']
# ds = ingrid_source.download(request).isel(Y=slice(None, None, -1))
# print(ds)

# request = SeasonalObsRequest(
#     bounding_box={'west': 0, 'east': 5, 'south': 20, 'north': 30},
#     years=(2024, 2026),
#     season_start_month=12,
#     season_length=3,
# )
# sigrid_source = observation_sources['UCSB.PRCP']
# print(sigrid_source.download(request))
