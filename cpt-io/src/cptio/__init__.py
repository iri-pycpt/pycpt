from .fileio import (
    open_cptdataset,
    open_cptdataarray,
    to_cptv10,
    convert_np64_datetime,
)
from .utilities import (
    rmrf,
    rmstar,
    ls_files_recursive,
    threeletters,
    recursive_getattr,
    seasonal_target_length,
    target_from_leads,
    leads_from_target,
    is_valid_cptv10_xyt,
)


__version__ = "1.3.2dev"
__author__ = "IRI (pycpt-help@iri.columbia.edu)"
__license__ = "MIT"
