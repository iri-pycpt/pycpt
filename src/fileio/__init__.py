from .cpt import * 


def is_valid_cptv10(da, assertmissing=True, assert_units=True):
    valid_dims = ['T', 'X', 'Y', 'Mode', 'index', 'C']
    valid_coords = ['T', 'Ti', 'Tf', 'S', 'X', 'Y', 'Mode', 'index', 'C']
    assert type(da) == xr.DataArray, "CPTv10 only deals with data arrays, not datasets"
    assert len(list(da.dims)) >= 2 and len(list(da.dims)) <= 4, 'CPTv10 can only have between 2-4 dimensions'
    for dim in da.dims:
        assert dim in valid_dims, 'Invalid dim for a CPTv10: {}'.format(dim)
    for coord in da.coords: 
         assert coord in valid_coords, 'Invalid coord for a CPTv10: {}'.format(coord)
    for dim in da.dims:
        assert dim in da.coords, 'Each dim on a CPTv10 must have corresponding coordinates'
        assert len(da.coords[dim].values) == da.shape[list(da.dims).index(dim)], 'Each dim on a CPTv10 must have exactly one coordinate per index along that dimension'
    if 'T' in da.dims: 
        for dim in ['Ti', 'Tf', 'S']:
            if dim in da.coords: 
                assert len(da.coords[dim].values) == da.shape[list(da.dims).index('T')], 'If the CPTv10 has optional Time coordinates Ti, Tf, or S, they must be indexing the T dimension'
    for dim in ['Ti', 'Tf', 'S']: 
        if dim in da.dims: 
            assert 'T' in da.dims, "if the optional time coordinates are present on the CPTv10, the required time coord must also be"
    if 'Ti' in da.coords:
        assert 'Tf' in da.coords, 'Cannot have one optional time coordinate and not the other. found Ti but not Tf. except for S'
    if 'Tf' in da.coords:
        assert 'Ti' in da.coords, 'Cannot have one optional time coordinate and not the other. found Tf but not Ti. except for S'
    if assertmissing:
        assert 'missing' in da.attrs.keys(), "CPTv10 is required to have a 'missing' attribute indicating the 'missing_value' value which replaces NaNs in CPT"
        assert not np.isnan(float(da.attrs['missing'])), "CPTv10 Missing Value cannot be NaN"
    
    if assert_units:
        assert 'units' in da.attrs.keys(), 'CPTv10 is required to have a "units" attribute'  
