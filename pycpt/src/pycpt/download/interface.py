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
class _SeasonalBaseRequest:
    season_start_month: int
    """The month that begins the target season (Jan = 1, Feb = 2, ...)"""

    season_length: int
    """Length of target season in months"""

    season_start_years: tuple[int, int]
    """Range of years, inclusive, of the start of the target season. Note
    that the years of the start of the target season are not necessarily the
    same as the years of initialization, e.g. a Dec initialization is in the
    year prior to its MAM target season.
    """

    bounding_box: BoundingBox | None
    """Data are to be cropped to this lat/lon bounding box. If None, entire
    available spatial domain is retrieved.
    """


@dataclass
class SeasonalObsRequest(_SeasonalBaseRequest):
    pass


@dataclass
class SeasonalForecastRequest(_SeasonalBaseRequest):
    season_start_lead: int
    """Time in months from initialization date to start of target season."""

    @property
    def init_month(self) -> int:
        """Month of initialization (Jan=1, Feb=2, ...)"""
        return (
            (self.season_start_month - self.season_start_lead - 1) % 12 + 1
        )

    @property
    def init_years(self) -> tuple[int, int]:
        """Range of years, inclusive, of the initialization date. Note
        that these are not necessarily the same as the years of the start of the
        target season, e.g. a Dec initialization is in the year prior to its MAM
        target season.
        """
        delta = int(self.init_month > self.season_start_month)
        return (
            self.season_start_years[0] - delta,
            self.season_start_years[1] - delta
        )


@dataclass
class SeasonalForecastSource(DataSource):
    @abc.abstractmethod
    def download(self, request: SeasonalForecastRequest) -> xr.DataArray:
        """
        Download a set of seasonally aggregated forecasts/hindcasts for a single
        combination of issue month and target season, and return them as a
        DataArray with the following dimensions:

            - T: target season start date
            - Y: latitude
            - X: longitude

        The values in the array represent the seasonal aggregation (mean for
        intensive properties like temperature, sum for extensive properties like
        rainfall).

        Parameters
        ----------
        request: SeasonalForecastRequest
        """
        ...


@dataclass
class SeasonalObsSource(DataSource):
    @abc.abstractmethod
    def download(self, request: SeasonalObsRequest, force_http: bool = False) -> xr.DataArray:
        """
        Download seasonally aggregated observations and return them as a
        DataArray with the following dimensions:

            - T: time. The start of the season.
            - Y: latitude
            - X: longitude

        The values in the array represent the seasonal aggregation (mean for
        intensive properties like temperature, sum for extensive properties like
        rainfall).

        Parameters
        ----------
        request: SeasonalObsRequest
        """
        ...
