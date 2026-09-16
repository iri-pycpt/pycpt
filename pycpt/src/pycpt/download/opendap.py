import numpy as np
import xarray as xr

from .interface import (
    DataSource,
    SeasonalForecastRequest,
    SeasonalForecastSource,
)


class OpendapSeasonalForecastSource(SeasonalForecastSource):
    def __init__(
            self,
            name: str,
            *,
            url: str,
            superseded_by: DataSource | None = None,
            superseded_doc: str | None = None,
    ):
        super().__init__(name, superseded_by, superseded_doc)
        self.url = url

    def download(self, request: SeasonalForecastRequest) -> xr.DataArray:
        ds = xr.open_dataset(self.url)

        # Spatiotemporal subsetting
        S = ds['S']
        ds = ds.sel(
            S=(
                (S.dt.month == request.init_month) &
                (S.dt.year >= request.init_years[0]) &
                (S.dt.year <= request.init_years[1])
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

        # Ensemble mean
        ds = ds.mean('M')

        # Average monthly value for the season, weighting months by their number
        # of days. TODO confirm we have only intensive quantities
        month_length = (
            ds['target_bnds'].isel(nbound=1) -
            ds['target_bnds'].isel(nbound=0)
        ).astype(int)  / (24 * 60 * 60 * 1e9)  # nanoseconds to days
        season_start = ds['target_bnds'].isel(L=0, nbound=0)
        season_end = ds['target_bnds'].isel(L=-1, nbound=1)
        season_bnds = (
            xr.concat([season_start, season_end], dim='nbound')
            .transpose('S', 'nbound')
        )
        ds = (ds * month_length).sum('L') / month_length.sum('L')
        ds = ds.assign_coords(
            target=('S', season_start.values),
            target_bnds=(('S', 'nbound'), season_bnds.values)
        )

        ds = sigrid_to_pycpt(ds)

        return ds


def sigrid_to_pycpt(ds: xr.Dataset) -> xr.Dataset:
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

request = SeasonalForecastRequest(
    bounding_box={'west': 0, 'east': 5, 'south': 0, 'north': 5},
    init_month=9,
    init_years=(2024, 2026),
    season_start_lead=0,
    season_length=2,
)
sigrid_source = OpendapSeasonalForecastSource(
    name='CCSM4.PRCP',
    url='https://forecast.ccsr.columbia.edu/data/NMME/COLA-RSMAS/CCSM4/pr',
)
print(sigrid_source.download(request))
print()
import pycpt.download.catalog
ingrid_source = pycpt.download.catalog.forecast_sources['CCSM4.PRCP']
ds = ingrid_source.download(request).isel(Y=slice(None, None, -1))
print(ds)