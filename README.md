# cpt-bin

This is a conda package containing precompiled executables (Linux, Windows, and macOS) of the batch version of [CPT](https://iri.columbia.edu/our-expertise/climate/tools/cpt/). PyCPT uses cpt-bin via [cpt-core](https://github.com/iri-pycpt/cpt-core).

## Building

### MacOS and Linux

For macOS and Linux, this package can be built with tools provided by conda-forge.

- Install [https://docs.conda.io/en/latest/miniconda.html](miniconda).
- Install `conda-build` and `anaconda-client` in the miniconda base environment.

        conda activate
        conda install -c conda-forge conda-build anaconda-client
        
- Check out this repository and cd to it.

- Edit the CPT version number to use in `meta.yaml`. Pre-release versions for testing can be built from http://iri.columbia.edu/~simon/cpt or from a local file, but packages we publish to the iri-nextgen channel should only be built from versions of CPT that have been released to Academic Commons. Links to CPT releases are posted to https://iri.columbia.edu/our-expertise/climate/tools/cpt/ .

- If a package has already been published for this version of CPT (e.g. you're changing package metadata or the build script, but not CPT itself), increment the build number. If this is the first package for a new version of CPT, set the build number back to 0.

- Run conda-build:

        conda build -c conda-forge recipe
        
  If the build is successful, the path to the package file will be printed near the end of the output, e.g.
  
        # Automatic uploading is disabled
        # If you want to upload package(s) to anaconda.org later, type:
         
         
        # To have conda build upload to anaconda.org automatically, use
        # conda config --set anaconda_upload yes
        anaconda upload \
            /home/aaron/miniconda3/conda-bld/linux-64/cptbin-17.8.3-dev3.tar.bz2

- Upload the package to the dev channel to make it available for others to test:

        anaconda upload -u iri-nextgen -l dev /home/aaron/miniconda3/conda-bld/linux-64/cptbin-17.8.3-dev3.tar.bz
        
- Invite beta testers to install PyCPT with the new version of `cpt-bin` as follows:

    - If they don't already have a conda environment for PyCPT, create one as follows:
    
             conda create -n pycpt -c iri-nextgen -c conda-forge pycpt
             conda activate pycpt

    - Then upgrade that environment to the test version of `cpt-bin`:

            conda install -c iri-nextgen/label/dev cpt-bin
            
- When you're ready to release the new version, publish it to the man iri-nextgen channel as follows:

    - Visit https://anaconda.org/iri-nextgen/cptbin/files in a browser. Log in as iri-nextgen.
    
    - Check the box next to the package you want to release.
    
    - Click the blue "Add label" menu above the file list, and select "main".
    
  Users can then upgrade to the latest version by activating their conda environment and typing
  
            conda install -c iri-nextgen cpt-bin


## Windows

Simon used to provide pre-compiled binaries for Windows, but currently he is having problems with both of the proprietary compilers he uses (see details below), so we are building it from scratch using gfortran. Unfortunately, the fortran compiler currently included in the conda-forge toolchain is gfortran 5, which is too old to support some of the language features used by CPT, so we need to install a more recent version of gfortran on the build host before running conda.

- Download and run the [msys2](https://www.msys2.org/) installer.

- From the Start menu, open an MSYS2 UCRT64 shell.

- In that shell, install gfortran and make:

        pacman --noconfirm -S gcc-fortran make

- Add the conda base environment to the `PATH` environment variable. I don't have an elegant way to do this. Simply activating the conda environment in the bash shell results in a failed build, probably because it puts the conda environment ahead of the MSYS2 environment. Instead, I copy the value of PATH from an anaconda session, convert that to cygwin path syntax, and do

        export PATH="$PATH:blahblah"
        
where `blahblah` is the translated anaconda path.

- Then build and upload as above.

Using compilers that aren't part of conda-forge makes the package inappropriate for inclusion in conda-forge, which means we can't take advantage of conda-forge's extensive CI infrastructure. I will follow developments in conda-forge so that when the fortran compiler is updated we can move our packages to conda-forge.

