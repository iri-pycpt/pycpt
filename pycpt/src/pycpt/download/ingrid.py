import datetime as dt
import tempfile
from dataclasses import dataclass

import xarray as xr

import cptdl as dl

from .interface import (
    BoundingBox,
    SeasonalForecastRequest,
    SeasonalForecastSource,
    SeasonalObsRequest,
    SeasonalObsSource,
)


@dataclass
class IngridSeasonalForecastSource(SeasonalForecastSource):
    hindcast_template: str
    forecast_template: str
    force_http: bool = False

    def download(
            self,
            request: SeasonalForecastRequest,
    ) -> xr.DataArray:
        self.warn_if_deprecated()

        first_year, final_year = request.init_years

        if first_year == final_year:
            template = self.forecast_template
        else:
            template = self.hindcast_template

        # This is the correct fdate when downloading the forecast (first_year == final_year),
        # and an arbitrary but good enough fdate when downloading hindcasts.
        fdate = dt.datetime(first_year, request.init_month, 1)

        target, lead_low, lead_high = format_season(
            request.init_month, request.season_start_lead, request.season_length
        )

        if request.bounding_box is None:
            a_bounding_box = BoundingBox(west=0, east=360, south=-90, north=90)
        else:
            a_bounding_box = request.bounding_box

        download_args = {
            'fdate': fdate,
            'target': target,
            'lead_low': lead_low,
            'lead_high': lead_high,
            'predictor_extent': a_bounding_box,
            'first_year': first_year,
            'final_year': final_year,
            # Not bothering to implement ensemblemean=False, because I don't think anyone
            # uses it, and for some datasets (ones that concatenate hindcasts and
            # forecasts in a single URL, when hindcasts and forecasts have different numbers
            # of members) it doesn't even work.
            'ensemblemean': True,
            'filetype': 'cptv10.tsv',
            'verbose': True,
        }
        if request.pressure_level is not None:
            download_args['pressure'] = request.pressure_level

        if self.force_http and template.startswith('https'):
            template = template.replace('https', 'http', 1)
        with tempfile.NamedTemporaryFile() as tmp:
            ds = dl.download(template, tmp.name, **download_args)

        return next(iter(ds.data_vars.values()))


def format_season(
        init_month: int, season_start_lead: int, season_length: int
        ) -> tuple[str, float, float]:
    """Translates the season into the form expected by the Ingrid URL templates"""
    # Add .05 because season_start_lead is the time (in months) from initialization
    # to the start of the season, whereas lead_low is the time (also in months) from
    # initialization to the midpoint of the first month of the season.
    lead_low = season_start_lead + .5
    lead_high = lead_low + season_length - 1
    fdate = dt.datetime(2000, init_month, 1) # arbitrary year
    target = dl.target_from_leads(fdate, lead_low, lead_high)
    return target, lead_low, lead_high


@dataclass
class IngridSeasonalObsSource(SeasonalObsSource):
    template: str
    # For testing only. To avoid the complexity of proxying https in tests,
    # we replace https with http.
    force_http: bool = False
 
    def download(
            self,
            request: SeasonalObsRequest,
    ) -> xr.DataArray:
        self.warn_if_deprecated()

        first_year, final_year = request.years

        target = target_string(request.season_start_month, request.season_length)

        if request.bounding_box is None:
            a_bounding_box = BoundingBox(west=0, east=360, south=-90, north=90)
        else:
            a_bounding_box = request.bounding_box

        download_args = {
            'target': target,
            'predictand_extent': a_bounding_box,
            'first_year': first_year,
            'final_year': final_year,
            'filetype': 'cptv10.tsv',
            'verbose': True
        }
        if request.pressure_level is not None:
            download_args['pressure'] = request.pressure_level

        template = self.template
        if self.force_http and template.startswith('https'):
            template = template.replace('https', 'http', 1)
        with tempfile.NamedTemporaryFile() as tmp:
            ds = dl.download(template, tmp.name, **download_args)

        return next(iter(ds.data_vars.values()))


def target_string(start_month: int, length: int) -> str:
    """Format the season in the form that Ingrid understands.
    >>> target_string(6,3)
    'Jun-Aug'
    >>> target_string(12,3)
    'Dec-Feb'
    """
    abbrev1 = dl.months[start_month - 1]
    index2 = (start_month + length) % 12 - 2
    abbrev2 = dl.months[index2]
    return f'{abbrev1}-{abbrev2}'

# class LocalFileDataSource(ObsDataSource):
#     def __init__(self, name: str, path: Path, superseded_by: ObsDataSource | None = None, superseded_doc: str | None = None):
#         super().__init__(name, superseded_by, superseded_doc)
#         self.path = path

#     def for_hindcasts(self, download_args: FullDownloadArgs, files_root: Path, force_download: bool=True) -> xr.DataArray:
#         print('Using local observed predictand dataset')
#         if self.path.suffix == '.tsv':
#             ds: xr.Dataset = cio.open_cptdataset(self.path)  # type: ignore [no-untyped-call]
#         elif self.path.suffix == '.nc':
#             ds = xr.open_dataset(self.path)
#         else:
#             assert False
#         Y = next(iter(ds.data_vars.values()))
#         return Y

forecast_sources: dict[str, SeasonalForecastSource] = {
    name: IngridSeasonalForecastSource(
        name,
        # Deferring population of succession info until we've
        # created all the objects we need to link to
        None, None,
        dl.catalog.hindcasts[name], dl.catalog.forecasts[name]
    )
    for name in dl.catalog.hindcasts
}

# Now that all the sources exist, link them together with deprecation links
for s in forecast_sources.values():
    metadata = dl.catalog.deprecations.get(s.name)
    if metadata is not None:
        name, doc = metadata
        s.superseded_by = forecast_sources[name]
        s.superseded_doc = doc

observation_sources: dict[str, SeasonalObsSource] = {
    name: IngridSeasonalObsSource(name, None, None, dl.observations[name])
    for name, template in dl.observations.items()
}

# There are as yet no deprecations for obs datasets. If we ever add any, create
# the links here.
