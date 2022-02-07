from .. import * 
import pytest
from pathlib import Path 

@pytest.mark.IO
@pytest.mark.parametrize('filen', [ i for i in Path('cpt_format_tests').glob('*')])
def test_cpt_fmt(filen):
    ds = open_cptdataset(filen)