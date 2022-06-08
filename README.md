# cpt-io
tools for dealing with CPT data - CPTv10 readers/writers

# Installation
## Local setup 
```
git clone https://github.com/kjhall-iri/cpt-io.git
cd cpt-io
python
import src as cptio 
``` 

## Conda install (remote)   (available soon) 
``` 
conda create -c conda-forge -c hallkjc01 -n pycpt  cptio 
conda activate pycpt 
python
import cpttools as ct
```

## Conda build locally & install 
```
git clone https://github.com/kjhall-iri/cpt-tools.git
cd cpt-tools
conda build -c conda-forge conda-recipe # this spits out a path to a .tar.bz2 
conda install -c [path_to_tar.bz2] -c conda-forge cpttools 
python
import cpttools 
``` 

## Tests 
```
git clone https://github.com/kjhall-iri/cpt-tools.git
cd cpt-tools
pytest -v .
```


for support: kjhall@iri.columbia.edu 

