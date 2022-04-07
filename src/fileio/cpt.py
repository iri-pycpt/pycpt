from pathlib import Path 
import os , re
import datetime as dt 
import numpy as np 
import xarray as xr 
from io import StringIO

def cpt_headers(header):
    m = re.compile("(?P<tag>cpt:.*?|cf:.*?)=(?P<value>.*?,|.*$)")
    matches = m.findall(header)
    return len(matches), { i.split(':')[1]: j.replace(',', '') for i,j in matches  }

def open_cptdataset(filename):
    assert Path(filename).absolute().is_file(), 'Cannot find {}'.format(Path(filename).absolute())
    with open(str(Path(filename).absolute()), 'r') as f: 
        content1 = f.read() 
    content = [line.strip() for line in content1.split("\n") if 'xmlns' not in line] 
    xmlnns_lines = [line.strip() for line in content1.split("\n") if 'xmlns' in line ]
    assert 'xmlns:cpt=http://iri.columbia.edu/CPT/v10/' in ' '.join(xmlnns_lines), 'CPT XML Namespace: {} Not detected'.format('xmlns:cpt=http://iri.columbia.edu/CPT/v10/')
    #xmlns = content.pop(0)  # cf header  
    #assert xmlns == 'xmlns:cf=http://cf-pcmdi.llnl.gov/documents/cf-conventions/1.4/', 'Invalid XML Namespace: {}'.format(xmlns)
    headers = [(linenum, *cpt_headers(line)) if ',' in line or ('=' in line and 'ncats' not in line and 'nfields' not in line) else ( linenum, line ) for linenum, line in enumerate(content) if 'cpt:' in line ]
    nrealheaders = len( [ i for i in headers if len(i) == 3 ])
    attrs, data_vars = {}, {}
    for i, header in enumerate(headers): 
        if len( header) == 3:  # we are only looking at the CPT headers that preceed a data block
            attrs.update({ k: header[2][k] for k in  header[2].keys() })
            array = np.genfromtxt( StringIO('\n'.join(content[header[0]+2:header[0]+2+ int(attrs['nrow'])])), delimiter='\t', dtype=str)
            columns = np.genfromtxt(StringIO(content[header[0]+1]), delimiter='\t', dtype=str)
            try:
                columns = columns.astype(float)
            except: 
                try: 
                    columns = np.asarray([ read_cpt_date(ii) for ii in columns])
                except:
                    pass 
            columns = np.expand_dims(columns, 0) if len(columns.shape) < 1 else np.squeeze(columns )

            if len(array.shape) < 2: 
                array = array.reshape(1, -1)
            rows = np.squeeze(array[:, 0])
            rows = np.expand_dims(rows, 0) if len(rows.shape) < 1 else np.squeeze(rows)
            try:
                rows = rows.astype(float)
            except:
                try:
                    rows = np.asarray([ read_cpt_date(ii) for ii in rows])
                except:
                    pass
            data = array[:, 1:].astype(float)

            # The first CPT header always has a 'field' field indicating the variable stored. It is assumed to be the same thereafter if not explicitly changed 
            field = header[2]['field'] if 'field' in header[2].keys() else attrs['field']

            if field not in data_vars.keys():
                #now identify dimensions: 'T', 'Mode', 'C' - same deal , the same thereafter unless changed. 
                rowdim= header[2]['row'] if 'row' in header[2].keys() else attrs['row']
                coldim= header[2]['col'] if 'col' in header[2].keys() else attrs['col']
                somedims = [  'T' if 'T' in header[2].keys() else None,  'Mode' if 'Mode' in header[2].keys() else None, 'C' if 'C' in header[2].keys() else None ]
                somedims = [ jj for jj in somedims if jj is not None ] # keep only dims present 
                coords = {jj : [ read_cpt_date(header[2][jj]) if jj == 'T' else header[2][jj] ] for jj in somedims }
                coords.update({rowdim: rows, coldim: columns})
                alldims = [  rowdim, coldim ]
                somedims.extend(alldims)
                alldims = somedims
                ndims = len(alldims)
                if 'C' in alldims and ndims==4:  # to accomodate 4D data, we sort C to the first dimension, do all the C's separately, then shove them together in the end.
                    alldims.pop(alldims.index('C'))
                    alldims.insert(0, 'C') 
                data_vars[field] = {'dims': alldims, 'coords':coords, 'data':  data if ndims == 2 else np.expand_dims(data, 0) if ndims == 3 else [np.expand_dims(data, 0)] if ndims == 4 else None, 'attrs': attrs}
               # print('found {}-dimensional variable {}'.format(len(alldims), field))
            else: 
                # detect new coordinates in T, C, or Mode: 
                for dim in data_vars[field]['dims']:
                    if dim in header[2].keys() and (read_cpt_date(header[2][dim]) if dim == 'T' else header[2][dim]) not in data_vars[field]['coords'][dim]:
                        data_vars[field]['coords'][dim].append(read_cpt_date(header[2][dim]) if dim == 'T' else header[2][dim])
                if ndims == 3: 
                    data_vars[field]['data'] = np.concatenate((data_vars[field]['data'], np.expand_dims(data, axis=0)), axis=0)
                elif ndims == 4: 
                    assert 'C' in header[2].keys(), 'Only accomodating 4D data with a C dimension as the highest dimension right now - its coord must change in every header'
                    if len(data_vars[field]['data']) <= int(header[2]['C']) -1: 
                        data_vars[field]['data'].append(np.expand_dims(data, axis=0))
                    else:
                        data_vars[field]['data'][int(header[2]['C']) -1] = np.concatenate((data_vars[field]['data'][int(header[2]['C']) -1], np.expand_dims(data, axis=0)), axis=0)
                data_vars[field]['attrs'].update(attrs)
    #print(data_vars['attributes']['coords'])

    for field in data_vars.keys():
        if len(data_vars[field]['dims']) == 4: 
            data_vars[field]['data'] = np.concatenate([np.expand_dims(data_vars[field]['data'][k], axis=0) for k in range(len(data_vars[field]['data']))], axis=0)
    dataarrays = {f.replace(' ', '_'): xr.DataArray(data_vars[f]['data'], dims=data_vars[f]['dims'], coords=data_vars[f]['coords'], attrs=data_vars[f]['attrs']) for f in data_vars.keys()}
    #print(data_vars['attributes']['coords'])
    return xr.Dataset(dataarrays)

def datetime_timestamp(date):
    fields = [int(i) for i in date.split('-')]
    while len(fields) < 3: 
        fields.append(1)
    return dt.datetime(*fields)
    
def read_cpt_date(date_original):
    if '/' in date_original: 
        date1, date2 = date_original.split('/')
        date1, date2 = date1.split('T')[0], date2.split('T')[0]
        date1, date2 = date1.split(' ')[0], date2.split(' ')[0]
        if len(date1.split('-')) == len(date2.split('-')): 
            ret1, ret2 = datetime_timestamp(date1), datetime_timestamp(date2)
        else: 
            assert len(date1.split('-')) > len(date2.split('-')), 'date1 must have more elements than date2' 
            ymd = date1.split('-') 
            ymd2 = date2.split('-')
            ymd2= ymd[:len(ymd) - len(ymd2)] + ymd2
            ret1, ret2 = datetime_timestamp(date1), datetime_timestamp('-'.join(ymd2) if len(ymd2) > 1 else ymd2[0])
        return ret1 + (ret2 - ret1) / 2
    else: 
        return datetime_timestamp(date_original) 
    


def to_cptv10(da, opfile='cptv10.tsv', row='Y', col='X', T=None, C=None):
    assert type(da) == xr.DataArray, 'Can only write Xr.DataArray to CPTv10'
    extra_dims = [ i for i in [T, C] if i is not None ]
    assert row is not None and col is not None, 'CPTv10 datasets must have at least two dimensions'
    dims  = extra_dims + [row, col]
    for dim in dims: 
        assert dim in da.dims, 'missing dim from data array - {}'.format(dim)
        assert dim in da.coords.keys(), 'missing coordinate from data array - {}'.format(dim)
        assert len(da.coords[dim].values) == da.shape[list(da.dims).index(dim)], 'data array coord {} not the same size as the dimension'.format(dim)
    assert len(dims) == len(da.dims), f'Data Array has dims {da.dims}, but you only passed {dims}'
    with open(opfile, 'w') as f: 
        f.write('xmlns:cpt=http://iri.columbia.edu/CPT/v10/' + "\n")
        f.write('cpt:nfields=1'+"\n")
        if C is not None: 
            f.write(f'cpt:ncats={da.shape[list(da.dims).index(C)]}\n')
        if len(extra_dims) == 2: 
            da = da.transpose(T, C, row, col)
            for i in range(da.shape[list(da.dims).index(T)]):
                for j in range(da.shape[list(da.dims).index(C)]):
                    header = f"cpt:field={da.name}, cpt:T={da.coords[T].values[i]}, cpt:C={da.coords[C].values[j]}, cpt:clim_prob=0.33333, cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}, cpt:units={da.attrs['units']}, cpt:missing={da.attrs['missing']}\n"
                    temp = da.isel({T:i, C:j}).fillna(float(da.attrs['missing'])).values
                    f.write(header)
                    f.write('\t' + '\t'.join([str(crd) for crd in da.coords[col].values]) + '\n')
                    temp = np.hstack([da.coords[row].values.reshape(-1,1), temp])
                    np.savetxt(f, temp, fmt="%.6f", delimiter='\t')
        elif len(extra_dims) == 1 and T is not None: 
            da = da.transpose(T, row, col)
            for i in range(da.shape[list(da.dims).index(T)]):
                header = f"cpt:field={da.name}, cpt:T={da.coords[T].values[i]}, cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}, cpt:units={da.attrs['units']}, cpt:missing={da.attrs['missing']}\n"
                temp = da.isel({T:i}).fillna(float(da.attrs['missing'])).values
                f.write(header)
                f.write('\t' + '\t'.join([str(crd) for crd in da.coords[col].values]) + '\n')
                temp = np.hstack([da.coords[row].values.reshape(-1,1), temp])
                np.savetxt(f, temp, fmt="%.6f", delimiter='\t')
        elif len(extra_dims) == 1 and C is not None: 
            da = da.transpose(C, row, col)
            for j in range(da.shape[list(da.dims).index(C)]):
                header = f"cpt:field={da.name}, cpt:C={da.coords[C].values[j]}, cpt:clim_prob=0.33333, cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}, cpt:units={da.attrs['units']}, cpt:missing={da.attrs['missing']}\n"
                temp = da.isel({C:j}).fillna(float(da.attrs['missing'])).values
                f.write(header)
                f.write('\t' + '\t'.join([str(crd) for crd in da.coords[col].values]) + '\n')
                temp = np.hstack([da.coords[row].values.reshape(-1,1), temp])
                np.savetxt(f, temp, fmt="%.6f", delimiter='\t')
        else:
            da = da.transpose(row, col)
            header = f"cpt:field={da.name}, cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}, cpt:units={da.attrs['units']}, cpt:missing={da.attrs['missing']}\n"
            temp = da.fillna(float(da.attrs['missing'])).values
            f.write(header)
            f.write('\t' + '\t'.join([str(crd) for crd in da.coords[col].values]) + '\n')
            temp = np.hstack([da.coords[row].values.reshape(-1,1), temp])
            np.savetxt(f, temp, fmt="%.6f", delimiter='\t')
    return opfile

               




