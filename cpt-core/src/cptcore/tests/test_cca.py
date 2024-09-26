import numpy as np

from .. import canonical_correlation_analysis
from .load_data import load_southasia_nmme

def test_cca(**kwargs):
    x, y = load_southasia_nmme()
    f = x.isel(T=slice(-1, None))
    x = x.isel(T=slice(None,-1))
    hcsts, fcsts, skill_values, x_pattern_values, y_pattern_values = canonical_correlation_analysis(x, y, F=f, **kwargs)

    assert set(hcsts.data_vars) == set(['deterministic'])
    da = hcsts['deterministic']
    assert da.dims == ('T', 'Y', 'X')
    assert da.shape == (41, 33, 30)
    np.testing.assert_array_equal(da['Y'].values, range(37, 4, -1))
    np.testing.assert_array_equal(da['X'].values, range(68, 98))

    # TODO more assertions