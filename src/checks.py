import xarray as xr 

def guess_coords(X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
	assert type(X) == xr.DataArray, 'X must be a data array'
	common_x = ['LONGITUDE', 'LONG', 'X', 'LON']
	common_y = ['LATITUDE', 'LAT', 'LATI', 'Y']
	common_t = ['T', 'S', 'TIME', 'SAMPLES', 'SAMPLE', 'INITIALIZATION', 'INIT', "TARGET"]
	common_m = ['M', 'FEATURES', 'F', 'REALIZATION', 'MEMBER', 'Z', 'C', 'CAT']
	ret = {'lat': x_lat_dim, 'lon': x_lon_dim, 'samp': x_sample_dim, 'feat': x_feature_dim}
	for dim in X.dims: 
		for x in common_x: 
			if x in dim.upper() and ret['lon'] is None:
				ret['lon'] = dim 
		for y in common_y: 
			if y in dim.upper() and ret['lat'] is None: 
				ret['lat'] = dim 
		for t in common_t:
			if t in dim.upper() and ret['samp'] is None: 
				ret['samp'] = dim 
		for m in common_m:
			if m in dim.upper() and ret['feat'] is None:
				ret['feat'] = dim 
	#assert None not in ret.values(), 'Could not detect one or more dimensions: \n  LATITUDE: {lat}\n  LONGITUDE: {lon}\n  SAMPLE: {samp}\n  FEATURE: {feat}\n'.format(**ret)
	vals = []
	for val in ret.values():
		if val not in vals: 
			vals.append(val)
	#assert len(vals) == 4, 'Detection Faild - Duplicated Coordinate: \n  LATITUDE: {lat}\n  LONGITUDE: {lon}\n  SAMPLE: {samp}\n  FEATURE: {feat}\n'.format(**ret)
	return ret['lat'], ret['lon'], ret['samp'], ret['feat']


def check_dimensions(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X is 4D, with Dimension Names as specified by x_lat_dim, x_lon_dim, x_sample_dim, and x_feature_dim"""
	for dim in [x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim]:
		if dim is not None: 
			assert dim in X.dims, 'CPTCORE requires a dataset_lat_dim to be a dimension on X'
	
def check_coords(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X has coordinates named as specified by x_lat_dim, x_lon_dim, x_sample_dim, and x_feature_dim"""
	for dim in [x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim]:
		if dim is not None: 
			assert dim in X.coords.keys(), 'CPTCORE requires a dataset_lat_dim to be a coordinate on X'
	
def check_consistent(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X's Coordinates are the same length as X's Dimensions"""
	for dim in [x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim]:
		if dim is not None: 
			assert X.shape[list(X.dims).index(dim)] == len(X.coords[dim].values), "we require a dataset's dim coordinate to be the same length as its x_lat_dim dimension"
	
def check_type(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X is an Xarray.DataArray"""
	assert type(X) == xr.DataArray, 'CPTCORE requires a dataset to be of type "Xarray.DataArray"'

def check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X satisfies all conditions for cptcore"""
	check_dimensions(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	check_coords(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	check_consistent(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	check_type(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	#check_transposed(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)