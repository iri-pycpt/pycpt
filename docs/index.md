## CPT-Tools: A Toolkit Bridging CPT and Xarray 
#### Author: Kyle J. C. Hall (kjhall@iri.columbia.edu) 

The [International Research Institute for Climate & Society](https://iri.columbia.edu/)'s Climate Predictability Tool ([CPT](https://iri.columbia.edu/our-expertise/climate/tools/cpt/) has been used for research and forecasting around the world for many years- now it's a part of the Python Climate Data Ecosystem.

Previously, CPT used exclusively "CPTv10" files - a boutique type of plain-text, tab-separated values file - and could not handle with Zarr or NetCDF files. There was no way to convert Zarr/NetCDF data to CPTv10, and the only place one could get CPTv10 was the [IRI Data Library](https://iridl.ldeo.columbia.edu). This toolkit  dramatically expands the world of CPT-compatible datasets by letting the user convert between  CPTv10 and NetCDF/Zarr with a robust, clean interface that mimics Xarray. 

CPT-Tools also implements a CPT "API", and allows the user direct access to hundreds of seasonal and subseasonal climate datasets in the IRI Data Library and Partner Data Libraries around the world. From within a Python workflow, one can use CPT-Tools to search, explore, subset and download both climate model data and observations data alike with a few commands. 

### Installation

CPT-Tools is a lightweight Python package, and is distributed on Anaconda.org. One can install it using the conda package manager: 

```
conda install -c conda-forge -c hallkjc01 cpttools 
```

or

```
conda create -c conda-forge -c hallkjc01 -n cpttools_env cpttools 
```

### Usage 

**Specifying Subsets** In order to download data, one must specify the subset of data they're looking for by defining a geographic region, a target period, and an initialization date. These are represented by several CPT-Tools data structures. 

- Geographic Range:  `cpttools.GeographicExtent(north=90, south=-90, east=360, west=0)` 
- Seasonal Target Period:  For seasonal data, either a three-letter CamelCase month abrreviation like 'Jan', or two such abrreviations denoting the first and last month of a desired season separated by a dash: like 'Jan-Mar'. 
- Subseasonal Target Period: For subseasonal data, a specification of the target week, with 'week1' corresponding to leads=(1day, 7days), 'week2' corresponding to leads=(8days, 14days), etc. also two week periods like 'week34' for lead=(15, 28).
- Lead Times: If specified, ints or floats corresponding the the months denoted by 'target' 

**Using the Data Catalog** The CPT-Tools data catalog is available as a top-level singleton, `cpttools.catalog`. It is in the form of an [Intake Catalog](https://intake.readthedocs.io/en/latest/) and can be used to explore the data catalog interactively in a terminal. Try some of the commands below to see what happens! 

```
import cpttools 

list(cpttools.catalog) # prints the subdatasets of 'catalog' 
# ['SEASONAL', 'SUBSEASONAL'] 

list(cpttools.catalog.SEASONAL) # accesses the 'SEASONAL' subdataset of 'catalog', and prints its children
# ['C3S', 'NMME', 'OBSERVATIONS'] 

# searches the catalog for the specified keyword and prints entries 
for entry in cpttools.catalog.search('PRCP', depth=10): 
  print(entry)  

# iterate over the subdatasets in an entry: 
for variable in cpttools.catalog.SEASONAL.C3S.METEOFRANCE7:
  print(variable) 
```

**Downloading Data** Bottom-level or "leaf-nodes" of the CPT-Tools data catalog represent data variables, and implement download methods. Seasonal data variables implement 'variable.forecasts(..)' and 'variable.hindcasts(...)' which download either a set of historical forecast initializations, or a single forecast initialization respectively. Seasonal observation variables implement a 'variable.observations(...)' which downloads the historically observed values for that climate variable. Sub-Seasonal data implement the same, but the 'observations' method is included on each data variable because each model has different hindcast dates, and the observations datasets need to match them to be useful anyway. See the examples below: 

```
import cpttools 
import pandas as pd 
pr = cpttools.SEASONAL.C3S.SPSv3p5.PRCP 
predictor = cpttools.GeographicExtent(north=50, south=30, east=-75, west=-100) 
target = 'Mar-May' 
fdate = pd.Timestamp.today() 
fcst_file = pr.forecasts(predictor, target='Mar-May', fdate=pd.Timestamp.today(), destination='sps_forecasts') # saves fcst data to sps_forecasts.tsv
hcst_file = pr.hindcasts(predictor, target='Mar-May', fdate=pd.Timestamp.today(), destination='sps_hindcasts') # saves hcst data to sps_hindcasts.tsv
``` 

**Reading & Writing CPTv10 Files** It is desirable to manipulate CPTv10 data in [Xarray](https://docs.xarray.dev/en/stable/). To get an Xarray dataset out of a cptv10 file, use the `cpttools.open_cptdataset('file.tsv')` function. 

```
import cpttools 
import xarray as xr 
ds = cpttools.open_cptdataset('sps_hindcasts.tsv')  # read in to xarray 
netcdf = ds.to_netcdf('sps_hcst.nc') # Xarray writes to netcdf 
```

It is also useful to read in netcdf using Xarray, then write to CPTv10 to use it in CPT:

```
import cpttools 
import xarray as xr 
ds = xr.open_dataset('sps_hcst.nc) # Xarray opens netcdf file
cptv10_filepath. = cpttools.to_cptv10(ds.prec, 'sps_hcst.tsv', T='T')  # write to cptv10 format file 'sps_hcst.tsv' 
```
Note that the default coordinate names are 'X' for (longitude) and  'Y' for columns (latitude) . If there is a third dimension on your Xarray DataArray, (time, for example) you need to specify the T dimension with `T='T'`. CPT accepts a fourth dimension for categorical data (ie, probabilistic forecast files with XYTC', and the C dimension can be specified with `C='C'`, but CPT requires the C coordinates to be monotonically increasing integers starting at 1. 





