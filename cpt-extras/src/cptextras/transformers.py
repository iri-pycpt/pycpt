import statsmodels.distributions.empirical_distribution as edf
from scipy.stats import norm, gamma, skew
from scipy.interpolate import interp1d
import xarray as xr 
import numpy as np 

def check_dimensions(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X is 4D, with Dimension Names as specified by x_lat_dim, x_lon_dim, x_sample_dim, and x_feature_dim"""
	assert 4 <= len(X.dims) <= 5, 'pycpt requires a dataset to be 4-Dimensional'
	assert x_lat_dim in X.dims, 'pycpt requires a dataset_lat_dim to be a dimension on X'
	assert x_lon_dim in X.dims, 'pycpt requires a dataset_lon_dim to be a dimension on X'
	assert x_sample_dim in X.dims, 'pycpt requires a dataset_sample_dim to be a dimension on X'
	assert x_feature_dim in X.dims, 'pycpt requires a dataset_feature_dim to be a dimension on X'

def check_coords(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X has coordinates named as specified by x_lat_dim, x_lon_dim, x_sample_dim, and x_feature_dim"""
	assert x_lat_dim in X.coords.keys(), 'pycpt requires a dataset_lat_dim to be a coordinate on X'
	assert x_lon_dim in X.coords.keys(), 'pycpt requires a dataset_lon_dim to be a coordinate on X'
	assert x_sample_dim in X.coords.keys(), 'pycpt requires a dataset_sample_dim to be a coordinate on X'
	assert x_feature_dim in X.coords.keys(), 'pycpt requires a dataset_feature_dim to be a coordinate on X'

def check_consistent(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X's Coordinates are the same length as X's Dimensions"""
	assert X.shape[list(X.dims).index(x_lat_dim)] == len(X.coords[x_lat_dim].values), "pycpt requires a dataset's x_lat_dim coordinate to be the same length as its x_lat_dim dimension"
	assert X.shape[list(X.dims).index(x_lon_dim)] == len(X.coords[x_lon_dim].values), "pycpt requires a dataset's x_lon_dim coordinate to be the same length as its x_lon_dim dimension"
	assert X.shape[list(X.dims).index(x_sample_dim)] == len(X.coords[x_sample_dim].values), "pycpt requires a dataset's x_sample_dim coordinate to be the same length as its x_sample_dim dimension"
	assert X.shape[list(X.dims).index(x_feature_dim)] == len(X.coords[x_feature_dim].values), "pycpt requires a dataset's x_feature_dim coordinate to be the same length as its x_feature_dim dimension"

def check_type(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X is an Xarray.DataArray"""
	assert type(X) == xr.DataArray, 'pycpt requires a dataset to be of type "Xarray.DataArray"'

def check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim):
	"""Checks that X satisfies all conditions for pycpt"""
	check_dimensions(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	check_coords(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	check_consistent(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	check_type(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)

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
	assert None not in ret.values(), 'Could not detect one or more dimensions: \n  LATITUDE: {lat}\n  LONGITUDE: {lon}\n  SAMPLE: {samp}\n  FEATURE: {feat}\n'.format(**ret)
	vals = []
	for val in ret.values():
		if val not in vals: 
			vals.append(val)
	assert len(vals) == 4, 'Detection Faild - Duplicated Coordinate: \n  LATITUDE: {lat}\n  LONGITUDE: {lon}\n  SAMPLE: {samp}\n  FEATURE: {feat}\n'.format(**ret)
	return ret['lat'], ret['lon'], ret['samp'], ret['feat']




def invcdf(X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None, dist=norm):
    x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
        X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
    check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)

    def _xr_tppf(x):
        return dist.ppf(x)  # , loc=y.mean(), scale=y.std())
    return xr.apply_ufunc(_xr_tppf, X, input_core_dims=[[x_sample_dim]], output_core_dims=[[x_sample_dim]], keep_attrs=True, vectorize=True)

class GammaTransformer:
    def __init__(self, destination=norm):
        self.destination_distribution = destination

    def fit(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
            X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
        self._fit_source(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)

    def _fit_source(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        """Makes an 'empirical cdf' function at each point in space"""
        x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
            X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)

        def _make_dist(x):
            # censor x where x < eps
            x = x[x > np.finfo(float).eps]
            #  where epsilon = machine precision, and tolerance = sqrt(epsilon)
            # check that skewness  4*( E[logX] - log(E[X])  ) > 0 for MLE fitting
            a = 4 * (np.log(x.mean()) - np.mean(np.log(x)))
            # here we use Maximum Likelihood Estimate
            if a > np.sqrt(np.finfo(float).eps):
                alpha = (1 + np.sqrt(1 + (a/3)))/a  #
                beta = np.mean(x) / alpha
                loc = np.min(x)
                method = 'mle'
            else:  # and here, Method of Moments
                # print(x)
                method = 'mm'
                if np.var(x) > np.finfo(float).eps:
                   # print('var > eps')
                    beta = np.var(x) / np.mean(x)
                    alpha = np.mean(x) / beta
                    loc = np.min(x)
                else:
                    alpha, beta, loc = 0, 0.0001, 0
            return gamma(alpha, scale=beta)

        self.dists = xr.apply_ufunc(_make_dist, X, input_core_dims=[
                                    [x_sample_dim]], output_core_dims=[[]], keep_attrs=True, vectorize=True)

    def transform(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
            X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)

        def _apply_cdf(x, dist):
            return dist.cdf(x)

        percentiles = xr.apply_ufunc(_apply_cdf, X, self.dists, input_core_dims=[
                                     [x_sample_dim], []], output_core_dims=[[x_sample_dim]], keep_attrs=True, vectorize=True)
        return invcdf(percentiles, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim, dist=self.destination_distribution)

    def inverse_transform(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
            X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
        percentiles = cdf(X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None,
                          x_feature_dim=None, dist=self.destination_distribution)

        def _xr_invert(x, dist):
            return dist.ppf(x)
        return xr.apply_ufunc(_xr_invert, percentiles, self.dists, input_core_dims=[[x_sample_dim], []], output_core_dims=[[x_sample_dim]], keep_attrs=True, vectorize=True)


class EmpiricalTransformer:
    def __init__(self, destination=norm):
        self.destination_distribution = destination

    def fit(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
            X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
        self._make_cdfs(X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        self._make_edf_invcdfs(X, x_lat_dim, x_lon_dim,
                               x_sample_dim,  x_feature_dim)

    def _make_cdfs(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        """Makes an 'empirical cdf' function at each point in space"""
        def _make_invcdf(x):
            empirical_cdf = edf.ECDF(np.squeeze(x))
            return empirical_cdf
        self.cdfs = xr.apply_ufunc(_make_invcdf, X, input_core_dims=[
                                   [x_sample_dim]], output_core_dims=[[]], keep_attrs=True, vectorize=True)

    def _make_edf_invcdfs(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        """Makes an 'empirical invcdf' function at each point in space"""
        def _make_invcdf(x):
            empirical_cdf = edf.ECDF(np.squeeze(x))
            slope_changes = sorted(set(np.squeeze(x)))
            cdf_vals_at_slope_changes = [
                empirical_cdf(i) for i in slope_changes]
            return interp1d(cdf_vals_at_slope_changes, slope_changes, fill_value='extrapolate')
        self.invcdfs = xr.apply_ufunc(_make_invcdf, X, input_core_dims=[
                                      ['T']], output_core_dims=[[]], keep_attrs=True, vectorize=True)

    def transform(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
            X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)

        def _xr_invert1(x, cdf1):
            return cdf1(x)
        percentiles = xr.apply_ufunc(_xr_invert1, X, self.cdfs, input_core_dims=[[x_sample_dim], [
                                     ]], output_core_dims=[[x_sample_dim]], keep_attrs=True, vectorize=True)  # percentile(X )
        return invcdf(percentiles, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim, dist=self.destination_distribution)

    def inverse_transform(self, X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
        x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(
            X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim)
        check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
        percentiles = cdf(X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None,
                          x_feature_dim=None, dist=self.destination_distribution)

        def _xr_invert(x, invcdf1):
            return invcdf1(x)
        return xr.apply_ufunc(_xr_invert, percentiles, self.invcdfs, input_core_dims=[[x_sample_dim], []], output_core_dims=[[x_sample_dim]], keep_attrs=True, vectorize=True)


