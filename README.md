# cpt-io
tools for dealing with CPT data - CPTv10 readers/writers

# Installation

## Conda install
To install the latest release of this package in a conda environment:
``` 
conda create -c conda-forge -c iri-nextgen -n pycpt cptio
conda activate pycpt 
python
import cptio
```

## Setting up source code for development
If you want to make changes to this package:
```
conda create -c conda-forge -c iri-nextgen -n pycpt cptio
conda activate pycpt
git clone https://github.com/iri-pycpt/cpt-io.git
cd cpt-io
pip install -e .
python
import src/cptio as cptio
```
Changes you make to the library will take effect when you restart python, no need to build the package first.

## Conda build locally & install 
```
conda create -c conda-forge -n conda-build conda-build
conda activate conda-build
git clone https://github.com/iri-pycpt/cpt-io.git
cd cpt-io
conda build -c conda-forge conda-recipe # this spits out a path to a .tar.bz2 
conda deactivate

conda create -c conda-forge -c iri-nextgen -n pycpt cptio
conda activate pycpt
conda install -c [path_to_tar.bz2] -c conda-forge cptio
python
import cptio
``` 

## Tests 
```
git clone https://github.com/iri-pycpt/cpt-io.git
cd cpt-io
pytest -v .
```


for support: awr@iri.columbia.edu

