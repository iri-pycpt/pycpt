import dataclasses 


@dataclasses.dataclass
class Geo: 
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

