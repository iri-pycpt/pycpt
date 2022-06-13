## CPT-IO

A python library for reading and writing CPTv10-formatted tab-separated values (tsv) files via Xarray


Maintainer: Kyle Hall (kjhall@iri.columbia.edu) 

### Installation

CPT-IO and all CPT-packages are distributed through anaconda as platform-independent "noarch" packages.  

CPT-IO can be installed through anaconda's package manager, ```conda``` which can be installed from [here](https://www.anaconda.com/products/distribution)

once you have conda installed, cpt-io and most other python packages can be installed with the following command: 

```
conda install -c conda-forge -c hallkjc01 cptio
```

it is, however, highly recommended that you use Anaconda environments to manage your python packages. In that case, you would use: 

```
conda create -c conda-forge -c hallkjc01 -n environment_name cptio
```

to install cpt-io. 


### Usage

CPT-IO has two main functions: ```cptio.open_cptdataset(...)``` and ```cptio.to_cptv10(...)```. ```open_cptdataset``` reads a CPTv10.tsv-formatted file, and returns an Xarray DataArray representing its contents. ```to_cptv10``` takes a CPTv10.tsv-compatible Xarray DataArray, and writes it to a CPTv10.tsv formatted file. 

An example of ```open_cptdataset```:

```
import cptio as cio 
da = cio.open_cptdataset("/Path/To/CPTv10/cptv10.tsv") 
# print or echo da to see an xarray dataarray
```

An example of ```to_cptv10```:

```
import cptio as cio 
da = cio.open_cptdataset("/Path/To/CPTv10/cptv10.tsv") 
# print or echo da to see an xarray dataarray

cio.to_cptv10(da, "new_output_file.tsv") 
# this line writes da back to a new cptv10-file
```

### CPTv10 Format & cptio.to_cptv10()

The CPTv10 file format is a plain-text format capable of representing high-dimensional gridded data, like NetCDF or HDF5. It is read/write/size inefficient compared to those mainstream, highly-optimized formats, but allows the user to manually inspect data in text editors and spreadsheet applications. NetCDF, HDF5 and TIFF files should be used for data archiving where possible. 

```to_cptv10```, and accordingly, the Climate Prediction Tool itself, require data to adhere to certain constraints. They are detailed here.
 
 - Data should be 2-, 3- or 4-Dimensional.
 - All dimension names need to be one of ```['T', 'X', 'Y', 'Mode', 'index', 'C']```
 - All coordinate names need to be one of ```['T', 'Ti', 'Tf', 'S', 'X', 'Y', 'Mode', 'index', 'C']```
 - If a dataset's time dimension represents a target period, it should be represented by coordinates named ```'T', 'Ti',``` and ```'Tf'```. An ```'S'``` coordinate representing intitialization date/time is optional. 
 - Coordinates named ```'Ti'``` and ```'Tf'``` , if present, represent the start and end of the target period, respectively. 
 - If a dataset has a ```'Ti'``` or ```'Tf'``` coordinate, it MUST also have the other (ie, both Ti and Tf) 
 - All time coordinates must be of type ```datetime.datetime```
 - Depending on the purpose of the data, a ```missing``` attribute may be required to be present on the Xarray DataArray, representing the value used to fill NaNs. a ```units``` attribute may also be required - it should be a string representing units. CPT-IO does not do any unit-tracking, that is left to the user. 

 - 3D data should have spatial dimensions (X, and Y) and a third dimension representing either time or EOF/CCA Modes
 - 4D data should have an additional 'category' dimension and is used to represent tercile probabilities.
 - 2D data can represent station-based climate data, and should have dimensions representing  'index' (station) and time ('T', etc) 

