from .. import open_cptdataset,  Geo, SEASONAL
from ..utilities.targetleadconv import possible_targets, init_from_lead, find_target, threeletters
from ..utilities import recursive_getattr
import datetime as dt 
import xarray as xr 
import numpy as np 

def load_nmme(lead=1, target="JJAS", geo=Geo(-90, 90, 0, 360), variable='PRCP', pressure=200, filetype='data.nc'): 
    assert lead >= 0 and lead + len(target) < 12, "NMME Models only have leads 0-11; please limit target and lead to comply."
    assert target in possible_targets, 'Invalid target- must be contiguous months'

    # identify initialization month and lead-high
    lead_low = lead + 0.5 
    lead_high = lead + 0.5 + len(target) - 1 
    tstart, tend = find_target(target)
    tar = '{}-{}'.format(tstart, tend)
    init = dt.datetime(2009, threeletters.index(init_from_lead(int(lead_low - 0.5), tstart)), 1)
    model_names = ['CanCM4i', 'CanSIPSv2', 'GEMNEMO', 'CanSIPSIC3', 'GEM5NEMO', 'CCSM4', 'CanCM3', 'CanCM4', 'AER04', 'FLORB01', 'FLORA06', 'SPEAR', 'GEOSS2S', 'CFSv2']
    models = [ getattr(SEASONAL.NMME, i) for i in model_names ]

    data = []
    for model in models: 
        if variable in list(model):
            ds = getattr(model, variable).hindcasts(geo, target=tar, fdate=init, pressure=pressure, filetype=filetype)
            data.append(ds)
    return xr.concat(data, 'M').assign_coords({'M': model_names})

def load_c3s(lead=1, target="JJAS", geo=Geo(-90, 90, 0, 360), variable='PRCP', pressure=200, filetype='data.nc'): 
    assert lead >= 0 and lead + len(target) < 6, "C3S Models only have leads 0-5; please limit target and lead to comply."
    assert target in possible_targets, 'Invalid target- must be contiguous months'

    # identify initialization month and lead-high
    lead_low = lead + 0.5 
    lead_high = lead + 0.5 + len(target) - 1 
    tstart, tend = find_target(target)
    tar = '{}-{}'.format(tstart, tend)
    init = dt.datetime(2009, threeletters.index(init_from_lead(int(lead_low - 0.5), tstart)), 1)
    model_names = list(SEASONAL.C3S) 
    models = [ getattr(SEASONAL.C3S, i) for i in model_names ]

    data = []
    for model in models: 
        if variable in list(model):
            ds= getattr(model, variable).hindcasts(geo, target=tar, fdate=init, pressure=pressure, filetype=filetype)
            data.append(ds)
    return xr.concat(data, 'M').assign_coords({'M': model_names})

def load_observations(source, target='JJAS', geo=Geo(-90, 90, 0, 360), variable='PRCP' , pressure=200, filetype='data.nc'):
    assert target in possible_targets, 'Invalid target- must be contiguous months'
    options = [ i for i in SEASONAL.OBSERVED.walk(depth=2) if 'ENACTS' in i or '.' not in i]
    assert source in options , 'source must be one of {}'.format(options )

    tstart, tend = find_target(target)
    tar = '{}-{}'.format(tstart, tend)
    init = dt.datetime(2000, threeletters.index(tstart), 1)
    model = recursive_getattr(SEASONAL.OBSERVED, source) 
    if 'ENACTS' in source:
        nsew = model.describe()['metadata']['extent']
        geo = Geo(nsew[1], nsew[0], nsew[3], nsew[2])
    assert variable in list(model), "{} source only has {} variables".format(list(model))
    ds = getattr(model, variable).observations(geo, target=tar, fdate=init, pressure=pressure, filetype=filetype)
    if 'missing' in getattr(ds, [i for i in ds.data_vars][0]).attrs.keys():
        ds = ds.where(ds !=  float(getattr(ds, [i for i in ds.data_vars][0]).attrs['missing']), other=np.nan)
    return ds

import xarray as xr 
from pathlib import Path 

def preload_southasia():
    x = open_cptdataset(Path( __file__ ).absolute().parents[1] / 'data/SEASONAL_CANCM4I_PRCP_HCST_JUN-SEP_None_2021-05.tsv').prec
    y = open_cptdataset( Path( __file__).absolute().parents[1] / 'data/SEASONAL_CPCCMAPURD_PRCP_OBS_JUN-SEP_None_2021-05.tsv').prate
    return x, y

def preload_lesotho_nmme():
    x = xr.open_dataset(Path( __file__ ).absolute().parents[1] / 'data/nmme_lead1_ond.nc').prec
    y = open_cptdataset( Path( __file__).absolute().parents[1] / 'data/lesotho_ond.tsv').rfe
    return x, y