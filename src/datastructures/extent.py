import dataclasses 
import cartopy.crs as ccrs
from cartopy import feature
import matplotlib.pyplot as plt 
from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER
import matplotlib.ticker as ticker

@dataclasses.dataclass
class GeographicExtent: 
    """Tracking and Validating the Geographic Extent of PyCPT Predictors and Predictands
       Conventions: 
         1. "strict-positive": Longitude is specified as [0, 360] with units "degrees east from prime" 
         2. "plus-minus-180": Longitude is specified as [-180, 180] with units "degrees east from prime" 
       Fields: 
         1. 'north': (float) indicates northernmost geographic bound in units 'degrees north from equator' 
         2. 'south': (float) indicates southernmost geographic bound in units 'degrees south from equator' 
         3. 'east': (float) indicates easternmost geographic bound in units 'degrees east from prime' 
         4. 'west': (float) indicates westernmost geographic bound in units 'degrees east from prime'
         5. 'convention': (str) indicates longitude-convention; some datasources use -90 to 90, some use 0-360
       Methods: 
         1. __post_init__(self): validates inputs according to convention
         2. to_strict_positive(): converts longitude and convention to the 'strict-positive' convention
         3. to_plus_minus_180(): converts longitude and convention to the 'plus_minus_180' convention.  
    """
    south: float = -90
    north: float = 90
    west: float = 0 
    east: float = 360 

    def __post_init__(self): 
        assert self.north <= 90, 'nothern bound must be <= 90 degrees north'
        assert self.south >= -90, 'southern bound must be >= 90 degrees south'
        assert self.north >= self.south, 'southern bound must be south of northern bound'
        assert self.west >= -180 and self.west <= 360, 'western bound must be between -180 and 360 degrees east'
        assert self.east >= -180 and self.east <= 360, 'western bound must be between -180 and 360 degrees east'
        #if self.west > self.east: 
            #warnings.warn('West: {} > East: {} - You will get something around the dateline'.format(self.west, self.east))

    def to_strict_positive(self): 
        if self.east < 0:
            self.east += 360
        if self.west < 0: 
            self.west += 360 
        return self 
    
    def to_plus_minus_180(self): 
        if self.east > 180: 
            self.east -= 360 
        if self.west > 180: 
            self.west -= 360 
        return self


    def plot(self, use_topo=True, title=None):
        title = 'Domain (N:{:0.2f}, S:{:0.2f}, E:{:0.2f}, W:{:0.2f})'.format(self.north, self.south, self.east, self.west ) if title is None else title 
        states_provinces = feature.NaturalEarthFeature(category='cultural', name='admin_0_countries', scale='10m', facecolor='none')

        #Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
        fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(5,5), subplot_kw=dict(projection=ccrs.PlateCarree()))
        ax.set_extent([self.west, self.east, self.south, self.north], ccrs.PlateCarree())

        # Put a background image on for nice sea rendering.
        if str(use_topo) == "True":
            ax.stock_img()

        ax.set_title(title)
        pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True, linewidth=2, color='gray', alpha=0.5, linestyle='--')
        pl.top_labels = False
        pl.left_labels = False
        pl.xformatter = LONGITUDE_FORMATTER
        pl.yformatter = LATITUDE_FORMATTER
        pl.xlocator = ticker.MaxNLocator(4)
        pl.ylocator = ticker.MaxNLocator(4)
        ax.add_feature(states_provinces, edgecolor='gray')
        plt.show()