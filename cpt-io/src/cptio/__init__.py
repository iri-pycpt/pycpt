from .fileio import (
    open_cptdataset,
    to_cptv10,
    guess_cptv10_coords,
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
    is_valid_cptv10,
)


__version__ = "1.0.4"
__author__ = "Kyle Hall (pycpt-help@iri.columbia.edu)"
__license__ = "MIT"
