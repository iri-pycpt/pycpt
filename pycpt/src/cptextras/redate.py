import cptio as cio
import datetime as dt 

def redate(x, yeardelta=100):
    """adds 100 years to the time-coordinate of a model dataarray"""
    for coord in ['Ti', 'Tf', 'T', 'S']:
        if coord in x.coords.keys(): 
            x = x.assign_coords({coord: ('T', [ dt.datetime(cio.convert_np64_datetime(i).year + yeardelta, cio.convert_np64_datetime(i).month, cio.convert_np64_datetime(i).day, cio.convert_np64_datetime(i).hour, cio.convert_np64_datetime(i).minute, cio.convert_np64_datetime(i).second) for i in x.coords[coord].values])})
    return x 