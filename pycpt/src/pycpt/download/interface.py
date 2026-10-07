import abc
import warnings
from dataclasses import dataclass
from typing import TypedDict

import xarray as xr


@dataclass
class DataSource(abc.ABC):
    name: str
    superseded_by: str | None
    superseded_doc: str | None

    def warn_if_deprecated(self) -> None:
        if self.superseded_by is not None:
            warnings.warn(
                f'{self.name} is deprecated. New configurations should use {self.superseded_by} '
                f'instead. {self.superseded_doc}',
                DeprecationWarning
            )


class BoundingBox(TypedDict):
    west: float
    east: float
    south: float
    north: float


@dataclass
class SeasonalForecastRequest:
    init_month: int
    init_years: tuple[int, int]
    season_start_lead: int
    season_length: int
    bounding_box: BoundingBox | None = None


@dataclass
class SeasonalForecastSource(DataSource):
    @abc.abstractmethod
    def download(
            self,
            request: SeasonalForecastRequest,
    ) -> xr.DataArray:
        """
        Download a set of seasonally aggregated forecasts/hindcasts and return them
        as a DataArray.

        The DataArray will have the following dimensions:

            - S: initialization date
            - Y: latitude
            - X: longitude

        The downloaded variable represents the seasonal aggregation (mean for intensive
        properties like temperature, sum for extensive properties like rainfall) of
        all hindcasts and forecasts that match the selection criteria.

        Parameters
        ----------
        init_month
            Retrieve hindcasts/forecasts initialized on the first of this month. Jan = 1, Feb = 2, ...
        init_years
            Range of years of the initialization date, inclusive. All available years within
            that range will be included in the result; there may be gaps in the S coordinate
            if some years are unavailable.
        season_start_lead
            Time in months from initialization date to start of target season.
        season_length
            Length of target season in months
        bounding_box
            Forecasts are cropped to the intersection of the full forecast and the lat/lon
            bounding box. If None, entire available spatial domain is retrieved.
        """
        ...


@dataclass
class SeasonalObsRequest:
    years: tuple[int, int]
    season_start_month: int
    season_length: int
    bounding_box: BoundingBox | None = None


@dataclass
class SeasonalObsSource(DataSource):
    @abc.abstractmethod
    def download(
            self,
            request: SeasonalObsRequest,
            force_http: bool = False,
    ) -> xr.DataArray:
        """
        Download seasonally aggregated observations and return them as a DataArray.

        The DataArray will have the following dimensions:

            - T: time. The start of the season.
            - Y: latitude
            - X: longitude

        The downloaded variable represents the seasonal aggregation (mean for intensive
        properties like temperature, sum for extensive properties like rainfall) of
        all hindcasts and forecasts that match the selection criteria.

        Parameters
        ----------
        years
            Range of years, inclusive. The year of a season is considered to be the
            year of its first month, e.g. the season from Dec 2000 to Feb 2001 is
            referred to as year 2000.
        season_start_month
            The month that begins the target season (Jan = 1, Feb = 2, ...).
        season_length
            Length of target season in months.
        bounding_box
            Data are cropped to the intersection of the full forecast and the lat/lon
            bounding box. If None, entire available spatial domain is retrieved.
        """
        ...
