# CPT-DL: for downloading stuff from IRI DL

## Instructions for publishing a new version of this package
- (One time only): create a conda environment containing the build tools

        conda create -n build -c conda-forge conda-build mamba anaconda-client

- Activate the build environment

        conda activate build

- Increment the package version number everywhere it occurs:
    - `setup.py`
    - `conda-recipe/meta.yaml`
    - `src/cptdl/__init__.py`

- Commit the version number changes and your changes to the data catalog on the master branch and push to github.

- Build the package

        conda mambabuild -c iri-nextgen -c conda-forge --override-channels conda-recipe
        
    In the output of this command, note the path to the package that was built.

- Upload the package to anaconda.org

        anaconda login --username iri-nextgen # prompts for password
        anaconda upload /path/to/the/package.tgz # use path printed by build command
        anaconda logout

