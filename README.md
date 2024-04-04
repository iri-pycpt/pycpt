# PyCPT 

PyCPT is a python interface to CPT, the IRI Climate Predictability Tool.

This README is directed at PyCPT developers. Users of PyCPT should see [the homepage](https://iri-pycpt.github.io).

## Package structure

PyCPT is made up of six conda packages: `cpt-bin`, `cpt-core`, `cpt-dl`, `cpt-extras`, `cpt-io`, and `pycpt`, plus a Jupyter Notebook that imports and uses these libraries.

`cpt-bin` contains the CPT executable. This is a platform-specific package that we build separately for Windows, Linux, and macOS, all on x86-64 processors. It works on Apple silicon via Rosetta. The other packages are pure python and thus platform-independent; we build one package and it works on all three platforms. (Earlier in the lifetime of this project, precompiled linux binaries were not provided, and installing PyCPT on Linux involved a time-consuming and unreliable compilation step. That was based on a misunderstanding; conda does in fact support building a single executable that runs on any linux system where conda is installed.)

Having `cpt-bin` as a separate package is useful because the python code is being developed more actively than CPT, and it is  convenient to be able to publish changes to the python code without rebuilding three platform-specific packages. The original motivation for splitting the python code into five different packages was that we anticipated using some of the support libraries in other applications , e.g. in python maprooms, and we wanted to be able to do that without carrying over all of PyCPT's dependencies, including CPT. This motivation is no longer as strong as it was. For one thing, installing cpt-bin on linux is no longer as difficult as it was, as explained in the previous paragraph. Second, in practice code reuse has been rare so far. The administrative burden of coordinating changes between different packages arguably outweighs what little benefit we get from it. We should consider merging some or all of the python packages in the future.

## Frozen conda environments

The original installation instructions for PyCPT 2 said simply to install the `pycpt` package, relying on conda to pull in all of pycpt's dependencies (packages that pycpt imports, and other packages that they import, etc.). This method is unreliable. Conda generally (subject to certain constraints) installs the version of each library that is most recent at the moment of installation. PyCPT currently depends on more than 300 libraries, all maintained on different schedules by different open source developers, so new versions of various dependencies are constantly appearing. Consequently, if you install by that method, you are likely to get a different set of packages next week than you would get today. Most of the time these updates are innocuous or beneficial, but occasionally one breaks PyCPT. To ensure that users get versions of all the packages that actually work together, instead of instructing them to install the latest version of everything, we specify exact version numbers for all 300+ libraries. This specification can be found in the `.lock` files that are included in the pycpt release, one per platform (linux, osx, windows). The versions listed in the lock file are by no means the only versions that will work; they are simply one combination that has been tested and is known to work.

## The `notebooks` repository

In addition to the `pycpt` repositiory, there is a second repository called `notebooks` that contains example Jupyter Notebooks that demonstrate the functionality provided by the pycpt packages. Of particular note is the `Operations` directory, which contains
- `pycpt-operational.ipynb`, a basic seasonal forecast notebook that serves as the starting point for most training sessions
- `config-example.py`, a template config file to be customized and then used with the `generate-forecasts` operational command
- `conda-linux64.lock`, `conda-osx-64.lock`, and `conda-win-64.lock`, the frozen conda environment specifications described in the previous section.

A "release" of PyCPT consists of a compatible set of the above files. Releases are published through the `notebooks` repository in GitHub, at https://github.com/iri-pycpt/notebooks/releases . Releases are currently identified by the version of the `pycpt` package they include. This numbering system can be inconvenient: in order to publish a change to a package like `cptdl`, we need to increment the version number of `pycpt` and publish a new `pycpt` package, even though the contents of that package are identical to those of the previous version. Merging packages as suggested in [Package structure](#package-structure) may resolve this.

Instructions for creating a release are given in the [Publishing new versions](#publishing-new-versions) section below.

## Development setup



## Publishing new versions

