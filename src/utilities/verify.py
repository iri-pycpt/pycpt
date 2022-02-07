import xarray as xr 

def check_dataarray(X, dims):
    """Checks that the specified dimensions are dimensions and coordinates on X"""
    assert type(X) == xr.DataArray, 'X ({}) must be an Xarray DataArray '.format(type(X))
    for dim in dims: 
        assert dim in X.dims, 'Missing dimensions on X ({}): {}'.format(X.dims, dim)
        
def ax_to_2d(ax, T, M):
    if T == 1 and M == 1: 
        return [[ax]]
    elif T == 1 and M > 1: 
        return [ ax ]
    elif T > 1 and M == 1: 
        return [[ax[i]] for i in range(len(ax))]
    else: 
        return ax 
