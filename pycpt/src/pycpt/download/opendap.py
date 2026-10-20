import itertools
from collections.abc import Callable
from typing import NotRequired, TypedDict

import numpy as np
import xarray as xr

from .interface import (
    BoundingBox,
    SeasonalForecastRequest,
    SeasonalForecastSource,
    SeasonalObsRequest,
    SeasonalObsSource,
)


class _ForecastVarConfig(TypedDict):
    urls: list[str]
    # The seasonal value of a variable that's extensive in time (e.g.
    # accumulated precipitation in mm) is calculated from monthly values by
    # summation, whereas that of a variable that's intensive in time (e.g.
    # precipitation rate in mm/day, temperature) is calculated by (weighted)
    # averaging. The terminology "extensive in time" comes from CF Conventions
    # Section 7.3 "Cell Methods."
    extensive_in_time: bool
    superseded_by: NotRequired[str]
    superseded_doc: NotRequired[str]


class OpendapSeasonalForecastSource(SeasonalForecastSource):
    def __init__(
            self,
            name, config: _ForecastVarConfig,
            pressure_level: float | None,
            opener: Callable[[str], xr.Dataset] = xr.open_dataset
    ):
        super().__init__(
            name,
            config.get('superseded_by'),
            config.get('superseded_doc')
        )
        self.urls = config['urls']
        self.extensive_in_time = config['extensive_in_time']
        self.pressure_level = pressure_level
        self.opener = opener

    def download(self, request: SeasonalForecastRequest) -> xr.DataArray:
        dss = [self.opener(url) for url in self.urls]

        def subset_temporal(ds: xr.Dataset):
            S = ds['S']
            (init_first_year, init_final_year) = request.init_years
            ds = ds.sel(
                S=(
                    (S.dt.month == request.init_month) &
                    (S.dt.year >= init_first_year) &
                    (S.dt.year <= init_final_year)
                ),
                L=slice(
                    request.season_start_lead,
                    request.season_start_lead + request.season_length - 1
                ),
            )
            return ds
        # Remove empty slices before concatenating. Including the empty slices
        # gives the same result, but raises a FutureWarning as of xarray
        # 2024.10.0
        dss = [
            subset for ds in dss
            if (subset := subset_temporal(ds)).sizes['S'] != 0
        ]

        # If slices overlap, prefer the later one.
        def trim(ds: xr.Dataset, ds_next: xr.Dataset):
            last_idx = np.searchsorted(ds['S'], ds_next['S'].values[0])
            ds = ds.isel(S=slice(0, last_idx))
            return ds
        dss = [
            trim(ds, ds_next)
            for ds, ds_next in itertools.pairwise(dss)
        ] + [dss[-1]]

        # Spatial subsetting has to come before concat, because concat causes
        # data to be loaded. TODO write a deferred version of concat
        dss = [subset_spatial(ds, request.bounding_box) for ds in dss]

        # Hindcasts and forecasts don't always have the same number of ensemble
        # members, so don't concat until we've taken the ensemble mean.
        dss = [ds.mean('M', keep_attrs=True) for ds in dss]

        ds = xr.concat(dss, dim='S')

        # If self.pressure_level is not None but the variable has no P
        # dimension, in principle that ought to be an error, but we're silently
        # ignoring it for backwards compatibility.  TODO be strict here, put
        # backwards compatibility in notebook.py?
        if self.pressure_level is not None and 'P' in ds:
            ds = ds.sel(P=self.pressure_level)


        ds = self.seasonal_value(ds, self.extensive_in_time)

        da = self.sigrid_to_pycpt(ds)

        return da

    @staticmethod
    def seasonal_value(ds: xr.Dataset, extensive_in_time: bool) -> xr.Dataset:
        """Seasonal total (for extensive) or mean (intensive) of monthly values.
        For the mean, monthly values are weighted by the number of days in the
        month.
        """
        # TODO is it better to rely on target_bnds or to reconstruct it?
        month_length = (
            ds['target_bnds'].isel(nbound=1) -
            ds['target_bnds'].isel(nbound=0)
        ).astype(int) // (24 * 60 * 60 * 1e-9)  # nanoseconds to days to avoid loss of precision
        season_start = ds['target_bnds'].isel(L=0, nbound=0)
        season_end = ds['target_bnds'].isel(L=-1, nbound=1)
        season_bnds = (
            xr.concat([season_start, season_end], dim='nbound')
            .transpose('S', 'nbound')
        )
        with xr.set_options(keep_attrs=True):
            if extensive_in_time:
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
    def sigrid_to_pycpt(ds: xr.Dataset) -> xr.DataArray:
        """Convert to PyCPT's idiosyncratic internal representation"""

        # Replace target and target_bnds with Ti, T, Tf
        Ti = ds['target_bnds'].isel(nbound=0)
        # Following the messed-up convention throughout pycpt that the end of the
        # season is represented by midnight on the last day of the season, i.e. 24
        # hours before the actual end. TODO can we fix this throughout pycpt?
        Tf = ds['target_bnds'].isel(nbound=1) - np.timedelta64(1, 'D')
        T = Ti + (Tf - Ti) / 2
        ds = ds.assign_coords(
            T=('S', T.values), Ti=('S', Ti.values), Tf=('S', Tf.values)
        ).swap_dims(S='T')
        ds = ds.drop_vars(['target', 'target_bnds'])

        return sigrid_to_pycpt_common(ds)


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
    superseded_by: NotRequired[str]
    superseded_doc: NotRequired[str]


class OpendapSeasonalObsSource(SeasonalObsSource):
    def __init__(
            self, name: str,
            config: _ObsVarConfig,
            pressure_level: float | None = None,
            opener: Callable[[str], xr.Dataset] = xr.open_dataset
    ):
        super().__init__(
            name,
            config.get('superseded_by'),
            config.get('superseded_doc')
        )
        self.url = config['url']
        self.extensive_in_time = config['extensive_in_time']
        self.pressure_level = pressure_level
        self.opener = opener

    def download(self, request: SeasonalObsRequest) -> xr.DataArray:
        ds = self.opener(self.url)

        # Temporal subsetting
        months = np.arange(
            request.season_start_month,
            request.season_start_month + request.season_length
        )
        months = (months - 1) % 12 + 1
        first_date = np.datetime64(f'{request.season_start_years[0]}-{months[0]:02}-01')
        if months[-1] < months[0]:
            final_year = request.season_start_years[1] + 1
        else:
            final_year = request.season_start_years[1]
        final_date = np.datetime64(f'{final_year}-{months[-1]:02}-01')
        ds = ds.sel(T=slice(first_date, final_date))
        ds = ds.sel(T=ds['T'].dt.month.isin(months))
        # TODO include only whole seasons

        # Subset by P
        # We don't need the same concession to backwards compatibility here that
        # we made in the ForecastSource, because none of the legacy Ingrid obs
        # sources had a P dimension.
        if self.pressure_level is not None:
            ds = ds.sel(P=self.pressure_level)

        ds = subset_spatial(ds, request.bounding_box)

        ds = self.seasonal_value(ds, request.season_start_month, self.extensive_in_time)

        da = self.sigrid_to_pycpt(ds)

        return da

    @staticmethod
    def seasonal_value(ds: xr.Dataset, season_start_month: int, extensive_in_time: bool) -> xr.Dataset:
        """Seasonal total (for extensive) or mean (intensive) of monthly values.
        For the mean, monthly values areis weighted by the number of days in the
        month.
        """
        # TODO add T_bnds to CHIRPS

        # If daily, convert to monthly first. This is only here because PyCPT
        # historically calculated seasonal values from daily CHIRPS, for no
        # good reason that I can see. In general you shouldn't do this because
        # it's very slow compared to using precalculated monthly data.
        t_resolution = ds['T'].diff('T').median()
        if t_resolution == np.timedelta64(1, 'D'):
            month_starts = xr.DataArray(
                ds['T'].values.astype('datetime64[M]').astype('datetime64[ns]'),
                dims='T',
                name='month'
            )
            groups = ds.groupby(month_starts)
            if extensive_in_time:
                ds = groups.sum(skipna=False)
            else:
                ds = groups.mean(skipna=False)
            ds = ds.rename(month='T')

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
        month_of_season = ((ds['T'] - season_start).values.astype('timedelta64[D]') // 28).astype('datetime64[ns]')  # TODO 28 seems hacky
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
            if extensive_in_time:
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
    def sigrid_to_pycpt(ds: xr.Dataset) -> xr.DataArray:
        """Convert to PyCPT's idiosyncratic internal representation"""
        Ti = ds['T_bnds'].isel(nbound=0)
        # Following the messed-up convention throughout pycpt that the end of the
        # season is represented by midnight on the last day of the season, i.e. 24
        # hours before the actual end. TODO can we fix this throughout pycpt?
        Tf = ds['T_bnds'].isel(nbound=1) - np.timedelta64(1, 'D')
        T = Ti + (Tf - Ti) / 2
        ds = ds.assign_coords(
            T_mid=('T', T.values), Ti=('T', Ti.values), Tf=('T', Tf.values)
        ).swap_dims(T='T_mid').drop_vars('T').rename(T_mid='T')
        ds = ds.drop_vars('T_bnds')

        return sigrid_to_pycpt_common(ds)


# Spatial subsetting

def subset_spatial(ds: xr.Dataset, bounding_box: BoundingBox | None):
    if not ds.indexes['X'].is_monotonic_increasing:
        raise Exception('X is not monotonic increasing')

    if bounding_box is None:
        return ds

    dss = tuple(
        subset_spatial_contiguous(ds, normalized_bb)
        for normalized_bb in normalize_bounding_box(ds, bounding_box)
    )
    ds = denormalize_response(bounding_box, dss)

    return ds

def subset_spatial_contiguous(ds: xr.Dataset, normalized_bounding_box: BoundingBox):
    if normalized_bounding_box is not None:
        ds = ds.sel(
            Y=slice(normalized_bounding_box['south'], normalized_bounding_box['north']),
            X=slice(normalized_bounding_box['west'], normalized_bounding_box['east']),
        )
    return ds

def x_lower_bound(ds: xr.Dataset) -> float:
    x = ds['X']
    if len(x) == 0:
        raise Exception("Can't find the extrema of a coordinate of length 0.")
    if (bounds_var := x.attrs.get('bounds')) is not None:
        return ds[bounds_var].isel(X=0, bnds=0).item()
    if len(x) < 2:
        raise Exception("No X bounds provided and can't infer them because length < 2")
    cell_width = np.median(np.diff(x.values))
    bound = x.values[0] - cell_width / 2
    return bound

def normalize_bounding_box(ds: xr.Dataset, bb: BoundingBox) -> tuple[BoundingBox, ...]:
    """Shift the bounding box by +/- 360 degrees to fit within the range of
    ds['X']. If it spans the dataset's seam, break it into two bounding boxes,
    one on each side of the seam. Takes the whole Dataset rather than just the X
    DataArray because a DataArray can't have a bounds coordinate.
    """
    west = bb['west']
    east = bb['east']
    if west > east:
        raise ValueError(
            """
            Western edge of bounding box has a longitude value larger than the
            eastern edge. This is not allowed because it would yield a Dataset
            whose X coordinate is not monotonic. If your bounding box spans the
            antimeridian (180 degrees), express it as values between 0 and 360.
            If it spans the prime meridian (0 degrees), express it as values
            between -180 and 180.
            """
        )
    span = east - west
    x_min = x_lower_bound(ds)
    if west < x_min:
        west = west + 360.
    elif west >= x_min + 360.:
        west = west - 360.
    east = west + span
    if east < x_min + 360.:
        # The BB doesn't span the seam. Do it in one piece
        return ({**bb, 'west': west, 'east': east},)
    else:
        # Two pieces, one each side of the seam
        return (
            {**bb, 'west': west, 'east': x_min + 360.},
            {**bb, 'west': 0, 'east': east % 360}
        )

def denormalize_response(bounding_box: BoundingBox, dss: tuple[xr.Dataset,...]) -> xr.Dataset:
    if len(dss) == 1:
        ds = dss[0]
        lons = ds['X']
        west = bounding_box['west']
        # It doesn't span the prime meridan (because len(dss) == 1), so the 
        # following is guaranteed to be monotonically increasing.
        new_lons = west + np.mod(lons - west, 360.)
        return ds.assign_coords(X=new_lons)
    elif len(dss) == 2:
        # It spans the prime meridian, and we enforce west <= east, so make X
        # negative in the left part and positive in the right part.
        left = dss[0].assign_coords(X=dss[0]['X'] - 360.)
        right = dss[1]
        return xr.concat([left, right], dim='X')
    else:
        assert False


def sigrid_to_pycpt_common(ds: xr.Dataset) -> xr.DataArray:
    # Now that target_bnds is gone, we no longer need the nbound dimension,
    # so we can simplify the Dataset to a DataArray
    data_vars = list(ds.data_vars)
    assert len(data_vars) == 1
    da = ds[data_vars[0]]

    # to_cptv10 requires a "missing" attribute
    da.attrs['missing'] = -999999

    # Put everything in decreasing latitude order. CPT accepts either order,
    # but always outputs in decreasing order. PyCPT assumes obs are in
    # decreasing order, as CHIRPS was in the IRIDL, and fails to align
    # forecast with obs if that assumption is violated. Putting everything
    # in decreasing latitude order simplifies things later.
    if da['Y'].values[0] < da['Y'].values[-1]:
        da = da.isel(Y=slice(None, None, -1))

    return da


# Catalog TODO move to its own file
nmme_url = 'https://forecast.ccsr.columbia.edu/data/NMME'
c3s_url = 'https://forecast.ccsr.columbia.edu/data/C3S'

vars = [
    ('PRCP', 'pr'),
    ('SST', 'tos'),
]
split_models = [
    ('CanSIPSIC4', 'ECCC/CanSIPS-IC4'),
    ('SPEAR', 'NOAA-GFDL/SPEAR'),
    ('GEOSS2S', 'NASA-GMAO/GEOSS2S'),
    ('CFSv2', 'NOAA-NCEP/CFSv2/pentad_samples'),
]
split_forecast_configs: dict [str, _ForecastVarConfig] = {
    f'{pycpt_model}.{pycpt_var}': {
        'urls': [
            f'{nmme_url}/{ccsr_model}/hindcast/{ccsr_var}',
            f'{nmme_url}/{ccsr_model}/forecast/{ccsr_var}',
        ],
        'extensive_in_time': False,
    }
    for pycpt_model, ccsr_model in split_models
    for pycpt_var, ccsr_var in vars
}

unified_models = [
    ('CCSM4', 'COLA-RSMAS/CCSM4'),
    ('CESM1', 'COLA-RSMAS/CESM1'),
]
unified_forecast_configs: dict[str, _ForecastVarConfig] = {
    f'{pycpt_model}.{pycpt_var}': {
        'urls': [
            f'{nmme_url}/{ccsr_model}/{ccsr_var}',
        ],
        'extensive_in_time': False,
    }
    for pycpt_model, ccsr_model in unified_models
    for pycpt_var, ccsr_var in vars
}

c3s_models = [
    ('SEAS51c', 'ECMWF/SEAS51'),
]
c3s_forecast_configs: dict[str, _ForecastVarConfig] = {
    f'{pycpt_model}.{pycpt_var}': {
        'urls': [
            f'{c3s_url}/{ccsr_model}/hindcast/{ccsr_var}',
            f'{c3s_url}/{ccsr_model}/forecast/{ccsr_var}',
        ],
        'extensive_in_time': False,
    }
    for pycpt_model, ccsr_model in c3s_models
    for pycpt_var, ccsr_var in vars
}

forecast_configs = split_forecast_configs | unified_forecast_configs | c3s_forecast_configs

obs_configs: dict[str, _ObsVarConfig] = {
    'UCSB.PRCP': {
        'url': 'https://forecast.ccsr.columbia.edu/data/obs/UCSB-CHC/CHIRPSv2/daily-improved/0p25/precipitation',
        'extensive_in_time': True,
    },
}

def forecast_source(name: str, pressure_level: float | None = None) -> SeasonalForecastSource | None:
    config = forecast_configs.get(name)
    if config is None:
        return None
    return OpendapSeasonalForecastSource(name, config, pressure_level)

def observation_source(name: str, pressure_level: float | None = None) -> SeasonalObsSource | None:
    config = obs_configs.get(name)
    if config is None:
        return None
    return OpendapSeasonalObsSource(name, config, pressure_level)



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
