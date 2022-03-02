from .xgrads import open_CtlDataset
from pathlib import Path 
import os 

def open_gradsdataset( filename, datafilename=None): 
    """this would normally be JUST xgrads open_CtlDataset, but CPT writes broken ctl files"""
    assert Path(filename).absolute().is_file(), 'Cannot find {}'.format(Path(filename).absolute())
    with open(str(Path(filename).absolute()), 'r') as f: 
        content = f.read() 
        content = content.split("\n")
        for line in range(len(content)): 
            content[line] = content[line].strip()
        for line in range(len(content)): 
            if 'DSET' in content[line]: 
                line2 = content[line].split() 
                line2[1] = str(Path(datafilename).absolute()) if datafilename is not None else str(Path(filename).absolute().parents[0] / Path(filename).stem) + '.dat'
                content[line] = ' '.join(line2)
        content = str("\n").join(content)
    with open(str(Path(filename).absolute()), 'w') as f: 
        f.write(content)
    return open_CtlDataset(str(Path(filename).absolute()))