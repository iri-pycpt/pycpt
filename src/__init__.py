from .base import CPT 
from .functional import canonical_correlation_analysis, principal_components_regression, probabilistic_forecast_verification, deterministic_skill, multiple_regression
from .tests import test_cca, test_deterministic_skill, test_mlr, test_pcr, test_pfv, load_southasia_nmme, load_lesotho_nmme 
from .bash import rmrf, ls_files_recursive, rmstar

__version__ = "1.0.0"
__author__ = "Kyle Joseph Chen Hall (kjhall@iri.columbia.edu)"

