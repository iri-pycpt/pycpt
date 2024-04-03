# PyCPT 

PyCPT is a python interface to CPT, the IRI Climate Predictability Tool.

This README is directed at PyCPT developers. Users of PyCPT should see [the homepage](https://iri-pycpt.github.io).

## Package structure

PyCPT is made up of six conda packages: `cpt-bin`, `cpt-core`, `cpt-dl`, `cpt-extras`, `cpt-io`, and `pycpt`, plus a Jupyter Notebook that imports and uses these libraries.

`cpt-bin` contains the CPT executable. This is a platform-specific package that we build separately for Windows, Linux, and macOS, all on x86-64 processors. It works on Apple silicon via Rosetta. The other packages are pure python and thus platform-independent; we build one package and it works on all three platforms. (Earlier in the lifetime of this project, precompiled linux binaries were not provided, and installing PyCPT on Linux involved a time-consuming and unreliable compilation step. That was based on a misunderstanding; conda does in fact support building a single executable that runs on any linux system where conda is installed.)

Having `cpt-bin` as a separate package is useful because the python code is being developed more actively than CPT, and it is  convenient to be able to publish changes to the python code without rebuilding three platform-specific packages. The original motivation for splitting the python code into five different packages was that we anticipated using some of the support libraries in other applications , e.g. in python maprooms, and we wanted to be able to do that without carrying over all of PyCPT's dependencies, including CPT. This motivation is no longer as strong as it was. For one thing, installing cpt-bin on linux is no longer as difficult as it was, as explained in the previous paragraph. Second, in practice code reuse has been rare so far. The administrative burden of coordinating changes between different packages arguably outweighs what little benefit we get from it. We should consider merging some or all of the python packages in the future.

## 

