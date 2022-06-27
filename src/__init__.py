from .base import CPT 
from .functional import canonical_correlation_analysis, principal_components_regression, probabilistic_forecast_verification, deterministic_skill, multiple_regression
from .tests import test_cca, test_deterministic_skill, test_mlr, test_pcr, test_pfv, load_southasia_nmme
from .bash import rmrf, ls_files_recursive, rmstar
from .utilities import install_cpt_linux

__version__ = "1.0.2"
__author__ = "Kyle Joseph Chen Hall (kjhall@iri.columbia.edu)"

from pathlib import Path 
import zipfile 

# expands an egg file so we can access data files
path = Path(__file__).parents[1]
newdir = Path(str(path).replace('.egg', ''))
if not newdir.is_dir():
    with zipfile.ZipFile(path, 'r') as zip_ref:
        zip_ref.extractall(newdir)

import platform
if platform.system() == 'Linux':
    install_cpt_linux()

CPT_CITATION = "Simon J. Mason, Michael K. Tippet, Lulin Song, Ángel G. Muñoz. 2021. Climate Predictability Tool version 17.5.2. Columbia University Academic Commons. https://doi.org/10.7916/d8-em2t-k781"
PYCPTV1_CITATION = "Muñoz, Á.G., Robertson, A.W., Turkington, T., Mason, S.J., and contributors, 2019: 'PyCPT: a Python interface and enhancement for IRI's Climate Predictability Tool'."


CPT_LICENSE = """Climate Prediction Tool (CPT), Copyright (c) 2003-2021 by
Simon Mason, Michael Tippett, and Lulin Song, International 
Research Institute for Climate Prediction (IRI), The Earth 
Institute at Columbia University (University), Palisades, 
NY.  All Rights Reserved. Permission is granted for an 
individual or institution to use this software (in any 
form) provided that:
1) The software (in any form) is not sold in any way.
2) The software (in any form) is not redistributed beyond
your immediate working group without permission.
3) Attribution is given for any results derived from the use
of the software (in any form) and for figures used in
publications.
4) This copyright and no-warranty information is kept in a
conspicuous location and that all references to this notice
are maintained.
5) The user agrees to the following NO WARRANTY notice.
                         NO WARRANTY
The Climate Predictability Tool (CPT) is provided free of
charge, thus is being provided "as is" without warranty of
any kind.  University makes no representation or warranty
either express or implied of any kind, including as to the
merchantability, adequacy or suitability of the software for
any particular purpose or to produce any particular result
and neither University, nor any employee, Trustee or agent
of University, shall have any liability arising out of the
use of the software for any reason, including but not
limited to the unmerchantability, inadequacy or
unsuitability of the software for any particular purpose
even if University has been informed of such purpose, or to
produce any particular result or any latent defects therein,
or the failure of University to provide the user with any
modifications or changes in the software.  Nothing in this
agreement shall be construed as a warranty or representation
that the software is or will be free from infringement of
domestic or foreign patents or other proprietary interests
of third parties, and University hereby expressly disclaims
any such warranties or representations. University does not
represent or warrant that errors in the software and
documentation will be corrected.  No agent of University is
authorized to alter or exceed the warranty obligations of
University as set forth herein. University disclaims any
responsibility for the accuracy or correctness of the
software or for its use or application by the user. In no
event will Simon Mason and/or Michael Tippett and/or IRI
and/or University be liable for damages, including any lost
profits, lost monies, or other special, incidental or
consequential damages arising out of the use or inability to
use (including but not limited to loss of data or data be
rendered inaccurate or losses sustained by third parties or
a failure of the program to operate with other software) the
program, even if the user is advised of the possibility of
such damages, or for any claim by any other party."""


