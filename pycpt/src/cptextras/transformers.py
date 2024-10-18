import statsmodels.distributions.empirical_distribution as edf
from scipy.stats import norm, gamma, skew
from scipy.interpolate import interp1d
import xarray as xr 
import numpy as np 

def check_all(X):
	"""Checks that X satisfies all conditions for pycpt"""
	assert isinstance(X, xr.DataArray), 'pycpt requires a dataset to be of type "Xarray.DataArray"'
	assert 'Y' in X.coords, 'pycpt requires "Y" (latitude) to be a coordinate on X'
	assert 'X' in X.coords, 'pycpt requires "X" (longitude) to be a coordinate on X'
	assert 'T' in X.dims, 'pycpt requires "T" (time) to be a dimension on X'

def invcdf(X, dist=norm):
    check_all(X)

    def _xr_tppf(x):
        return dist.ppf(x)  # , loc=y.mean(), scale=y.std())
    return xr.apply_ufunc(_xr_tppf, X, input_core_dims=[['T']], output_core_dims=[['T']], keep_attrs=True, vectorize=True)

class GammaTransformer:
    def __init__(self, destination=norm):
        self.destination_distribution = destination

    def fit(self, X):
        check_all(X)
        self._fit_source(X)

    def _fit_source(self, X):
        """Makes an 'empirical cdf' function at each point in space"""
        check_all(X)

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
                                    ['T']], output_core_dims=[[]], keep_attrs=True, vectorize=True)

    def transform(self, X):
        check_all(X)

        def _apply_cdf(x, dist):
            return dist.cdf(x)

        percentiles = xr.apply_ufunc(_apply_cdf, X, self.dists, input_core_dims=[
                                     ['T'], []], output_core_dims=[['T']], keep_attrs=True, vectorize=True)
        return invcdf(percentiles, dist=self.destination_distribution)

    def inverse_transform(self, X):
        check_all(X)
        percentiles = cdf(X, dist=self.destination_distribution)

        def _xr_invert(x, dist):
            return dist.ppf(x)
        return xr.apply_ufunc(_xr_invert, percentiles, self.dists, input_core_dims=[['T'], []], output_core_dims=[['T']], keep_attrs=True, vectorize=True)


class EmpiricalTransformer:
    def __init__(self, destination=norm):
        self.destination_distribution = destination

    def fit(self, X):
        check_all(X)
        self._make_cdfs(X)
        self._make_edf_invcdfs(X)

    def _make_cdfs(self, X):
        """Makes an 'empirical cdf' function at each point in space"""
        def _make_invcdf(x):
            empirical_cdf = edf.ECDF(np.squeeze(x))
            return empirical_cdf
        self.cdfs = xr.apply_ufunc(_make_invcdf, X, input_core_dims=[
                                   ['T']], output_core_dims=[[]], keep_attrs=True, vectorize=True)

    def _make_edf_invcdfs(self, X):
        """Makes an 'empirical invcdf' function at each point in space"""
        def _make_invcdf(x):
            empirical_cdf = edf.ECDF(np.squeeze(x))
            slope_changes = sorted(set(np.squeeze(x)))
            cdf_vals_at_slope_changes = [
                empirical_cdf(i) for i in slope_changes]
            return interp1d(cdf_vals_at_slope_changes, slope_changes, fill_value='extrapolate')
        self.invcdfs = xr.apply_ufunc(_make_invcdf, X, input_core_dims=[
                                      ['T']], output_core_dims=[[]], keep_attrs=True, vectorize=True)

    def transform(self, X):
        check_all(X)

        def _xr_invert1(x, cdf1):
            return cdf1(x)
        percentiles = xr.apply_ufunc(_xr_invert1, X, self.cdfs, input_core_dims=[['T'], [
                                     ]], output_core_dims=[['T']], keep_attrs=True, vectorize=True)  # percentile(X )
        return invcdf(percentiles, dist=self.destination_distribution)

    def inverse_transform(self, X):
        check_all(X)
        percentiles = cdf(X, dist=self.destination_distribution)

        def _xr_invert(x, invcdf1):
            return invcdf1(x)
        return xr.apply_ufunc(_xr_invert, percentiles, self.invcdfs, input_core_dims=[['T'], []], output_core_dims=[['T']], keep_attrs=True, vectorize=True)


