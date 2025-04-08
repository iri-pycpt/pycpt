# PyCPT 

PyCPT is a python interface to CPT, the IRI Climate Predictability Tool.

This README is directed at PyCPT developers. Users of PyCPT should see [the homepage](https://iri-pycpt.github.io).

## Package structure

PyCPT is made up of three conda packages: `cpt-bin`, `cpt-io`, and `pycpt`, plus a Jupyter Notebook that imports and uses these libraries.

`cpt-bin` contains the CPT executable. This is a platform-specific package that we build separately for Windows, Linux, and macOS, all on x86-64 processors. It works on Apple silicon via Rosetta. The other packages are pure python and thus platform-independent; we build one package and it works on all three platforms. (Earlier in the lifetime of this project, precompiled linux binaries were not provided, and installing PyCPT on Linux involved a time-consuming and unreliable compilation step. That was based on a misunderstanding; conda does in fact support building a single executable that runs on any linux system where conda is installed.)

Having `cpt-bin` as a separate package is useful because the python code is being developed more actively than CPT, and it is  convenient to be able to publish changes to the python code without rebuilding three platform-specific packages.

## Frozen conda environments

The original installation instructions for PyCPT 2 said simply to install the `pycpt` package, relying on conda to pull in all of pycpt's dependencies (packages that pycpt imports, and other packages that they import, etc.). This method is unreliable. Conda generally (subject to certain constraints) installs the version of each library that is most recent at the moment of installation. PyCPT currently depends on more than 300 libraries, all maintained on different schedules by different open source developers, so new versions of various dependencies are constantly appearing. Consequently, if you install by that method, you are likely to get a different set of packages next week than you would get today. Most of the time these updates are innocuous or beneficial, but occasionally one breaks PyCPT. To ensure that users get versions of all the packages that actually work together, instead of instructing them to install the latest version of everything, we specify exact version numbers for all 300+ libraries. This specification can be found in the `.lock` files that are included in the pycpt release, one per platform (linux, osx, windows). The versions listed in the lock file are by no means the only versions that will work; they are simply one combination that has been tested and is known to work.

## The `notebooks` repository

In addition to the `pycpt` repositiory, there is a second repository called `notebooks` that contains example Jupyter Notebooks that demonstrate the functionality provided by the pycpt packages. Of particular note is the `Operations` directory, which contains
- `pycpt-operational.ipynb`, a basic seasonal forecast notebook that serves as the starting point for most training sessions
- `config-example.py`, a template config file to be customized and then used with the `generate-forecasts` operational command
- `conda-linux64.lock`, `conda-osx-64.lock`, and `conda-win-64.lock`, the frozen conda environment specifications described in the previous section.

A "release" of PyCPT consists of a compatible set of the above files. Releases are published through the `notebooks` repository in GitHub, at https://github.com/iri-pycpt/notebooks/releases . Releases are currently identified by the version of the `pycpt` package they include. This numbering system can be inconvenient: in order to publish a change to a package like `cptio`, we need to increment the version number of `pycpt` and publish a new `pycpt` package, even though the contents of that package are identical to those of the previous version. Merging packages as suggested in [Package structure](#package-structure) may resolve this.

I (Aaron) find the separation between the `pycpt` and `notebooks` repositories confusing. I think we should move the Operational subdir of `notebooks` into `pycpt`, and then publish subsequent releases from that repo instead.

Instructions for creating a release are given in the [Publishing new versions](#publishing-new-versions) section below.

## Development setup

We are now using [`pixi`](https://pixi.sh/) instead of `conda` to manage the
development environment. After installing pixi and checking out this repository,
you can run jupyter notebook with the command
```
pixi run jupyter notebook --notebook-dir ~/src/notebooks
```
replacing `~/src/notebooks` with the path to the directory containing the
notebook you want to edit. The first time you run this command, `pixi` will
automatically download and install the necessary packages, based on the contents
of `pixi.toml`.

Unlike with `conda`, you must cd to this project's directory to use
`pixi run` (hence the use of `--notebook-dir`).

## Publishing changes

The steps for creating a new version of PyCPT can be summarized as follows:

- Modify one or more of the python packages
- Build new conda packages for the modified python packages
- Publish the new conda packages to anaconda.org
- Modify the environment lock files to use the new conda package versions
- Modify `pycpt-operational.ipynb` if necessary
- Ideally, build the new environment on each platform and test the latest notebook in those environments. This is such a tedious and time-consuming step that I sometimes skip it, which makes the release process less reliable than it could be. We need to automate some tests to help with this.
- Publish a new GitHub release from the `notebooks` repository.

We will now go into more detail on some of these steps.

### Building a pure python package

After modifying any package other than `cpt-bin`, follow these instructions. (The process for `cpt-bin` is more complicated because that package contains Fortran code that must be compiled for each platform. Instructions for that are in the next section.)

- `cd` to the subdirectory of `pycpt` for the package you want to build, e.g. `cd cpt-io`.
- Increment the version number. Currently the version number is found in three different files, all of which must be kept in lock step: `setup.py`, `conda-recipe/meta.yaml`, and `src/<package name>/__init__.py`, e.g. `src/cptio/__init__.py`. See [About version numbers](#about-version-numbers) below.
- Commit and push your changes, including the changed version number. Have your changes reviewed and approved, then merge them.
- If you have never built a conda package before, start by creating a conda environment called `build` that contains the tools for building conda packages: `conda create -n build -c conda-forge boa`. Once you have this environment you can use it for all package builds.
- Activate your `build` environment: `conda activate build`
- `conda mambabuild -c iri-nextgen -c conda-forge conda-recipe`
- Wait a long time. Watch the log messages for errors.
- If there are no errors, near the end of the log it will show the path of the newly built package, e.g.
    ```
    /home/aaron/miniconda3/envs/build/conda-bld/noarch/cptio-1.1.3-py_0.tar.bz2
    ```
- If there were packaging-related changes, test that built package in a new conda environment. When only changing python code I generally skip this step.
- Upload the new package file, whose path we noted above, to anaconda.org, e.g.
    ```
    anaconda upload -u iri-nextgen /home/aaron/miniconda3/envs/build/conda-bld/noarch/cptio-1.1.3-py_0.tar.bz2
    ```
  Get the password from another PyCPT developer.

  The above command makes the package available in the `iri-nextgen` channel. If you want to share the package with some beta-testers prior to releasing it, you can add `-l dev`. It will then be available in a channel called `iri-nextgen/label/dev`. If problems are discovered and you need to make changes, it's ok to remove the broken package from the `dev` channel, but once a package is published to `iri-nextgen` it should be considered as released and should generally not be overwritten or removed.

### Building cpt-bin

(To be written)

### Updating environment lock files

To update the environment specifications to use newly published PyCPT packages, it usually suffices to edit the lock files by hand and update the version numbers for those packages, e.g. change
```
https://conda.anaconda.org/t/ir-777bcf3a-3147-44d2-9fa2-dccca9b8d3ed/iri-nextgen/noarch/cptio-1.1.2-py_0.tar.bz2
```
to
```
https://conda.anaconda.org/t/ir-777bcf3a-3147-44d2-9fa2-dccca9b8d3ed/iri-nextgen/noarch/cptio-1.1.3-py_0.tar.bz2
```

If we need to update not only PyCPT packages but also one or more third-party dependencies, it is not a good idea to edit the lock files by hand, as the result may violate compatibility constraints between different packages. The simplest thing to do in this case is usually to
- Create a new environment from scratch: `conda create -n pycpt-new -c iri-nextgen -c conda-forge pycpt`
- `conda activate pycpt-new`
- `conda list` and verify that the desired versions have been installed for the PyCPT packages and any third-party packages for which we need particular versions
- Test PyCPT thoroughly in the new environment
- Recreate the lock file from the new environment: `conda list --explicit > notebooks/Operations/conda-linux-64.lock`

If this process results in the wrong versions of some packages being installed, or in an environment where PyCPT doesn't work, then we need to be more explicit about versions. (TODO go into more detail.)

When recreating the environment from scratch, this process must be repeated on each platform (Windows, macOS, Linux).

### Creating a GitHub release

We used to instruct users to download files from the GitHub `Code` tab, but this had the following disadvantages:
- Many users had trouble figuring out how to download a file through this interface, which is designed for viewing code in a browser, not for downloading.
- It made us avoid merging changes into `master` until they were ready to release, which meant changes had to be merged in big batches rather than incrementally as they were ready.

To solve these problems, we switched to publishing via GitHub's "Releases" mechanism. To create a new release,
- Click the `Actions` tab in the `notebooks` GitHub repository
  
    ![image](https://github.com/iri-pycpt/pycpt/assets/766406/c7927b3f-18f6-4929-a5b9-76fc1c4c9aef)

- Under `Workflows`, click `main.yaml`

    ![image](https://github.com/iri-pycpt/pycpt/assets/766406/7e7385d9-1413-4d0b-ad54-b40fd26ac4b5)

- Click `Run workflow`, enter the version number of the new release, preceded by the letter `v`, then click the green `Run workflow` button.

    ![image](https://github.com/iri-pycpt/pycpt/assets/766406/ecd7f04b-334a-432b-bc34-65173f96b1a7)

- Return to the `Code` tab, click `Releases`

    ![image](https://github.com/iri-pycpt/pycpt/assets/766406/365c15f5-5e23-45f8-b75f-68baa256ec03)

- (Make screenshots and more precise instructions for the remaining steps next time we actually do a release)
- Click the "edit" pencil on the new draft release
- Set the name to the version number, e.g. `v2.8.2`. As noted above, the convention we're currently following is to name the release after the version of `pycpt` that it includes, but we may want to reconsider that.
- Click publish.


