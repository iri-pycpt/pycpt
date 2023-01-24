import datetime as dt 
from ..fileio import *
import xarray as xr


json_types = [dict, list, str, int, float, bool]

def toJSON(obj):
    ret = {} 
    for key in vars(obj).keys(): 
        if type(vars(obj)[key]) in json_types:
            if type(vars(obj)[key]) == dict:
                dct = vars(obj)[key]
                if type(dct[dct.keys()[0]]) not in json_types:
                    for key1 in dct.keys():
                        dct[key1] = toJSON(dct[key])
                ret[key] = dct
            else: 
                ret[key] = vars(obj)[key]
        else: 
            ret[key] = toJSON(vars(obj)[key])
    return ret
    

def recursive_getattr(obj, attr):
    if '.' in attr:
        attrs = attr.split('.')
        next = '.'.join(attrs[1:])
        return recursive_getattr(getattr(obj, attrs[0]), next)
    else:
        return getattr(obj, attr)










