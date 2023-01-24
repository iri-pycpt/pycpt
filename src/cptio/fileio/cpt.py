from pathlib import Path
import os, re
import datetime as dt
import numpy as np
import xarray as xr
from io import StringIO


def is_valid_cptv10(da, assertmissing=True, assert_units=True):
    valid_dims = ["T", "X", "Y", "Mode", "index", "C", "M"]
    valid_coords = ["T", "Ti", "Tf", "S", "X", "Y", "Mode", "index", "C", "M"]
    assert type(da) == xr.DataArray, "CPTv10 only deals with data arrays, not datasets"
    assert (
        len(list(da.dims)) >= 2 and len(list(da.dims)) <= 4
    ), "CPTv10 can only have between 2-4 dimensions"
    for dim in da.dims:
        assert dim in valid_dims, "Invalid dim for a CPTv10: {}".format(dim)
    for coord in da.coords:
        assert coord in valid_coords, "Invalid coord for a CPTv10: {}".format(coord)
    for dim in da.dims:
        assert (
            dim in da.coords
        ), "Each dim on a CPTv10 must have corresponding coordinates"
        assert (
            len(da.coords[dim].values) == da.shape[list(da.dims).index(dim)]
        ), "Each dim on a CPTv10 must have exactly one coordinate per index along that dimension"
    if "T" in da.dims:
        for dim in ["Ti", "Tf", "S"]:
            if dim in da.coords:
                assert (
                    len(da.coords[dim].values) == da.shape[list(da.dims).index("T")]
                ), "If the CPTv10 has optional Time coordinates Ti, Tf, or S, they must be indexing the T dimension"
    for dim in ["Ti", "Tf", "S"]:
        if dim in da.dims:
            assert (
                "T" in da.dims
            ), "if the optional time coordinates are present on the CPTv10, the required time coord must also be"
    if "Ti" in da.coords:
        assert (
            "Tf" in da.coords
        ), "Cannot have one optional time coordinate and not the other. found Ti but not Tf. except for S"
    if "Tf" in da.coords:
        assert (
            "Ti" in da.coords
        ), "Cannot have one optional time coordinate and not the other. found Tf but not Ti. except for S"
    if assertmissing:
        assert (
            "missing" in da.attrs.keys()
        ), "CPTv10 is required to have a 'missing' attribute indicating the 'missing_value' value which replaces NaNs in CPT"
        assert not np.isnan(
            float(da.attrs["missing"])
        ), "CPTv10 Missing Value cannot be NaN"

    if assert_units:
        assert (
            "units" in da.attrs.keys()
        ), 'CPTv10 is required to have a "units" attribute'


def cpt_headers(header):
    m = re.compile("(?P<tag>cpt:.*?|cf:.*?)=(?P<value>.*?,|.*$)")
    matches = m.findall(header)
    return len(matches), {i.split(":")[1]: j.replace(",", "") for i, j in matches}


def open_cptdataset(filename):
    assert Path(filename).absolute().is_file(), "Cannot find {}".format(
        Path(filename).absolute()
    )
    with open(str(Path(filename).absolute()), "r") as f:
        content1 = f.read()
    content = [line.strip() for line in content1.split("\n") if "xmlns" not in line]
    xmlnns_lines = [line.strip() for line in content1.split("\n") if "xmlns" in line]
    assert "xmlns:cpt=http://iri.columbia.edu/CPT/v10/" in " ".join(
        xmlnns_lines
    ), "CPT XML Namespace: {} Not detected".format(
        "xmlns:cpt=http://iri.columbia.edu/CPT/v10/"
    )
    headers = [
        (linenum, *cpt_headers(line))
        if "," in line
        or ("=" in line and "ncats" not in line and "nfields" not in line)
        else (linenum, line)
        for linenum, line in enumerate(content)
        if "cpt:" in line
    ]
    attrs, data_vars = {}, {}
    for i, header in enumerate(headers):
        if (
            len(header) == 3
        ):  # we are only looking at the CPT headers that preceed a data block
            attrs_at_row = {
                k: header[2][k] for k in header[2].keys() if k not in ["T", "S"]
            }
            attrs.update(attrs_at_row)
            array = np.genfromtxt(
                StringIO(
                    "\n".join(
                        content[header[0] + 2 : header[0] + 2 + int(attrs["nrow"])]
                    )
                ),
                delimiter="\t",
                dtype=str,
            )
            columns = np.genfromtxt(
                StringIO(content[header[0] + 1]), delimiter="\t", dtype=str
            )
            try:
                columns = columns.astype(float)
            except:
                try:
                    columns = np.asarray([read_cpt_date(ii) for ii in columns])
                except:
                    pass
            columns = (
                np.expand_dims(columns, 0)
                if len(columns.shape) < 1
                else np.squeeze(columns)
            )

            if len(array.shape) < 2:
                array = array.reshape(1, -1)
            rows = np.squeeze(array[:, 0])
            rows = np.expand_dims(rows, 0) if len(rows.shape) < 1 else np.squeeze(rows)
            try:
                rows = rows.astype(float)
            except:
                try:
                    rows = np.asarray([read_cpt_date(ii) for ii in rows])
                except:
                    pass
            data = array[:, 1:].astype(float)

            # The first CPT header always has a 'field' field indicating the variable stored. It is assumed to be the same thereafter if not explicitly changed
            field = (
                header[2]["field"] if "field" in header[2].keys() else attrs["field"]
            )

            if field not in data_vars.keys():
                # now identify dimensions: 'T', 'Mode', 'C' - same deal , the same thereafter unless changed.
                rowdim = header[2]["row"] if "row" in header[2].keys() else attrs["row"]
                coldim = header[2]["col"] if "col" in header[2].keys() else attrs["col"]
                somedims = [
                    "T"
                    if "T" in header[2].keys() and "T" not in [rowdim, coldim]
                    else None,
                    "Mode"
                    if "Mode" in header[2].keys() and "Mode" not in [rowdim, coldim]
                    else None,
                    "C"
                    if "C" in header[2].keys() and "C" not in [rowdim, coldim]
                    else None,
                    "M"
                    if "M" in header[2].keys() and "M" not in [rowdim, coldim]
                    else None,
                ]
                somedims = [
                    jj for jj in somedims if jj is not None
                ]  # keep only dims present
                alldims = [rowdim, coldim]
                somedims.extend(alldims)
                alldims = somedims
                coords = {}
                for jj in alldims:
                    if jj not in [rowdim, coldim]:
                        if jj == "T":
                            date_coord = read_cpt_date(header[2][jj])
                            if len(date_coord) == 1:
                                coords[jj] = [date_coord[0]]
                            else:
                                coords["T"] = [date_coord[1]]
                                coords["Ti"] = [date_coord[0]]
                                coords["Tf"] = [date_coord[2]]
                        else:
                            coords[jj] = [header[2][jj]]
                temp = {rowdim: rows, coldim: columns}
                if "T" in [rowdim, coldim]:
                    date_coords = temp["T"]
                    temp["T"] = [date_coords[ii][1] for ii in range(len(date_coords))]
                    temp["Ti"] = [date_coords[ii][0] for ii in range(len(date_coords))]
                    temp["Tf"] = [date_coords[ii][2] for ii in range(len(date_coords))]
                coords.update(temp)
                if "S" in header[2].keys():  # S is always a length-1 situation
                    coords.update({"S": [read_cpt_date(header[2]["S"])[0]]})
                ndims = len(alldims)
                if (
                    "C" in alldims and ndims == 4
                ):  # to accomodate 4D data, we sort C to the first dimension, do all the C's separately, then shove them together in the end.
                    alldims.pop(alldims.index("C"))
                    alldims.insert(0, "C")
                if (
                    "M" in alldims and ndims == 4
                ):  # to accomodate 4D data, we sort C to the first dimension, do all the C's separately, then shove them together in the end.
                    alldims.pop(alldims.index("M"))
                    alldims.insert(0, "M")
                data_vars[field] = {
                    "dims": alldims,
                    "coords": coords,
                    "data": data
                    if ndims == 2
                    else np.expand_dims(data, 0)
                    if ndims == 3
                    else [np.expand_dims(data, 0)]
                    if ndims == 4
                    else None,
                    "attrs": attrs,
                }
            # print('found {}-dimensional variable {}'.format(len(alldims), field))
            else:
                # detect new coordinates in T, C, or Mode:
                for dim in data_vars[field]["coords"].keys():
                    if dim in header[2].keys():
                        if dim == "T":
                            date_coord = read_cpt_date(header[2][dim])
                            if len(date_coord) == 1:
                                if date_coord[0] not in data_vars[field]["coords"][dim]:
                                    data_vars[field]["coords"][dim].append(
                                        date_coord[0]
                                    )
                            else:
                                if date_coord[1] not in data_vars[field]["coords"][dim]:
                                    data_vars[field]["coords"]["T"].append(
                                        date_coord[1]
                                    )
                                    data_vars[field]["coords"]["Ti"].append(
                                        date_coord[0]
                                    )
                                    data_vars[field]["coords"]["Tf"].append(
                                        date_coord[2]
                                    )
                        elif dim == "S":
                            date_coord = read_cpt_date(header[2][dim])[0]
                            if date_coord not in data_vars[field]["coords"][dim]:
                                data_vars[field]["coords"][dim].append(date_coord)
                        else:
                            if header[2][dim] not in data_vars[field]["coords"][dim]:
                                data_vars[field]["coords"][dim].append(header[2][dim])
                if ndims == 3:
                    data_vars[field]["data"] = np.concatenate(
                        (data_vars[field]["data"], np.expand_dims(data, axis=0)), axis=0
                    )
                elif ndims == 4:
                    assert (
                        "C" in header[2].keys() or "M" in header[2].keys()
                    ), "Only accomodating 4D data with a C (or M) dimension as the highest dimension right now - its coord must change in every header"
                    if "C" in header[2].keys():
                        if (
                            len(data_vars[field]["data"])
                            <= int(float(header[2]["C"])) - 1
                        ):
                            data_vars[field]["data"].append(
                                np.expand_dims(data, axis=0)
                            )
                        else:
                            data_vars[field]["data"][
                                int(float(header[2]["C"])) - 1
                            ] = np.concatenate(
                                (
                                    data_vars[field]["data"][
                                        int(float(header[2]["C"])) - 1
                                    ],
                                    np.expand_dims(data, axis=0),
                                ),
                                axis=0,
                            )
                    if "M" in header[2].keys():
                        if (
                            len(data_vars[field]["data"])
                            <= int(float(header[2]["M"])) - 1
                        ):
                            data_vars[field]["data"].append(
                                np.expand_dims(data, axis=0)
                            )
                        else:
                            data_vars[field]["data"][
                                int(float(header[2]["M"])) - 1
                            ] = np.concatenate(
                                (
                                    data_vars[field]["data"][
                                        int(float(header[2]["M"])) - 1
                                    ],
                                    np.expand_dims(data, axis=0),
                                ),
                                axis=0,
                            )

                data_vars[field]["attrs"].update(attrs)
    # print(data_vars['attributes']['coords'])

    for field in data_vars.keys():
        if len(data_vars[field]["dims"]) == 4:
            data_vars[field]["data"] = np.concatenate(
                [
                    np.expand_dims(data_vars[field]["data"][k], axis=0)
                    for k in range(len(data_vars[field]["data"]))
                ],
                axis=0,
            )
    for f in data_vars.keys():
        data_vars[f]["coords"] = {
            m: ("T" if m in ["S", "Ti", "Tf"] else m, data_vars[f]["coords"][m])
            for m in data_vars[f]["coords"].keys()
        }
    dataarrays = {
        f.replace(" ", "_"): xr.DataArray(
            data_vars[f]["data"],
            dims=data_vars[f]["dims"],
            coords=data_vars[f]["coords"],
            attrs=data_vars[f]["attrs"],
        )
        for f in data_vars.keys()
    }
    for k in dataarrays.keys():
        is_valid_cptv10(dataarrays[k], assert_units=False, assertmissing=False)
        if "missing" in dataarrays[k].attrs.keys():
            dataarrays[k] = dataarrays[k].where(
                dataarrays[k] != float(dataarrays[k].attrs["missing"]), other=np.nan
            )
    return xr.Dataset(dataarrays)


def datetime_timestamp(date):
    if "T" in date:
        ymd, hms = date.split("T")
    else:
        ymd = date

    fields = [int(i) for i in ymd.split("-")]
    while len(fields) < 3:
        fields.append(1)
    return dt.datetime(*fields)


def datetime_timestamp_end(date):
    if "T" in date:
        ymd, hms = date.split("T")
    else:
        ymd = date
    last_days = [None, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    fields = [int(i) for i in ymd.split("-")]
    while len(fields) < 3:
        if len(fields) == 1:
            fields.append(12)
        else:
            fields.append(last_days[fields[1]])
    return dt.datetime(*fields)


def read_cpt_date(date_original):
    if "/" in date_original:
        date1, date2 = date_original.split("/")
        date1, date2 = date1.split("T")[0], date2.split("T")[0]
        date1, date2 = date1.split(" ")[0], date2.split(" ")[0]
        if len(date1.split("-")) == len(date2.split("-")):
            ret1, ret2 = datetime_timestamp(date1), datetime_timestamp_end(date2)
        else:
            assert len(date1.split("-")) > len(
                date2.split("-")
            ), "date1 must have more elements than date2"
            ymd = date1.split("-")
            ymd2 = date2.split("-")
            ymd2 = ymd[: len(ymd) - len(ymd2)] + ymd2
            ret1, ret2 = datetime_timestamp(date1), datetime_timestamp_end(
                "-".join(ymd2) if len(ymd2) > 1 else ymd2[0]
            )
        return [ret1, ret1 + (ret2 - ret1) / 2, ret2]
    else:
        return [datetime_timestamp(date_original)]


def convert_np64_datetime(np64):
    unix_epoch = np.datetime64(0, "s")
    one_second = np.timedelta64(1, "s")
    seconds_since_epoch = (np64 - unix_epoch) / one_second
    return dt.datetime.utcfromtimestamp(seconds_since_epoch)


def guess_cptv10_coords(da, row=None, col=None, T=None, C=None):
    ret = {"row": row, "col": col, "T": T, "C": C}
    guesses = {
        "row": ["Y", "T", "Mode"],
        "col": ["X", "index"],
        "T": ["T", "Mode"],
        "C": ["C", "M"],
    }
    found = []
    for dim in ["row", "col", "T", "C"]:
        for guess in guesses[dim]:
            if guess in da.dims and ret[dim] is None and guess not in found:
                ret[dim] = guess
                found.append(guess)
    guesses = [ret[i] for i in ret.keys() if ret[i] is not None]
    found_dims = []
    while len(guesses) > 0:
        guess = guesses.pop(0)
        assert (
            guess not in found_dims
        ), "repeated dimension in guesses (None excluded)- {}".format(ret)
        found_dims.append(guess)
    assert len(found_dims) == len(
        da.dims
    ), "Unable to guess names of all dims on da accurately - guessed {} vs {}".format(
        ret, da.dims
    )
    for dim in found_dims:
        assert (
            dim in da.dims
        ), "guessed a dimension not present on da - {} vs {}".format(ret, da.dims)
    return ret["row"], ret["col"], ret["T"], ret["C"]


def to_cptv10(
    da,
    opfile="cptv10.tsv",
    row=None,
    col=None,
    T=None,
    C=None,
    assertmissing=True,
    assert_units=True,
):
    row, col, T, C = guess_cptv10_coords(da, row, col, T, C)
    is_valid_cptv10(da, assert_units=assert_units, assertmissing=assertmissing)
    assert type(da) == xr.DataArray, "Can only write Xr.DataArray to CPTv10"
    extra_dims = [i for i in [T, C] if i is not None]
    assert (
        row is not None and col is not None
    ), "CPTv10 datasets must have at least two dimensions"
    dims = extra_dims + [row, col]
    for dim in dims:
        assert dim in da.dims, "missing dim from data array - {}".format(dim)
        assert (
            dim in da.coords.keys()
        ), "missing coordinate from data array - {}".format(dim)
        assert (
            len(da.coords[dim].values) == da.shape[list(da.dims).index(dim)]
        ), "data array coord {} not the same size as the dimension".format(dim)
    assert len(dims) == len(
        da.dims
    ), f"Data Array has dims {da.dims}, but you only passed {dims}"
    missingblurb = (
        ", cpt:missing={:.6f}".format(float(da.attrs["missing"]))
        if assertmissing
        else ""
    )
    unitsblurb = f", cpt:units={da.attrs['units']}" if assert_units else ""
    with open(opfile, "w") as f:
        f.write("xmlns:cpt=http://iri.columbia.edu/CPT/v10/" + "\n")
        f.write("cpt:nfields=1" + "\n")
        if C is not None:
            f.write(f"cpt:ncats={da.shape[list(da.dims).index(C)]}\n")
        if len(extra_dims) == 2:
            da = da.transpose(T, C, row, col)
            for i in range(da.shape[list(da.dims).index(T)]):
                for j in range(da.shape[list(da.dims).index(C)]):

                    header = f"cpt:field={da.name}, cpt:{T}={da.coords[T].values[i] if 'Ti' not in da.coords else '{}-{}-{}/{}-{}-{}'.format(convert_np64_datetime(da.coords['Ti'].values[i]).year, convert_np64_datetime(da.coords['Ti'].values[i]).month, convert_np64_datetime(da.coords['Ti'].values[i]).day, convert_np64_datetime(da.coords['Tf'].values[i]).year, convert_np64_datetime(da.coords['Tf'].values[i]).month, convert_np64_datetime(da.coords['Tf'].values[i]).day ) },{'' if 'S' not in da.coords.keys() else 'cpt:S='+ convert_np64_datetime(da.coords['S'].values[i]).strftime('%Y-%m-%dT%H:%M') + ', '} cpt:{C}={da.coords[C].values[j]}{', cpt:clim_prob=0.33333' if C=='C' else ''}, cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}{unitsblurb}{missingblurb}\n"
                    if assertmissing:
                        temp = (
                            da.isel({T: i, C: j})
                            .fillna(float(da.attrs["missing"]))
                            .values
                        )
                    else:
                        temp = da.isel({T: i, C: j}).values

                    f.write(header)
                    if row == "T" or col == "T":
                        tcoords_temp = [
                            (
                                convert_np64_datetime(da.T.coords["Ti"].values[i]).year,
                                convert_np64_datetime(
                                    da.T.coords["Ti"].values[i]
                                ).month,
                                convert_np64_datetime(da.T.coords["Ti"].values[i]).day,
                                convert_np64_datetime(da.T.coords["Tf"].values[i]).year,
                                convert_np64_datetime(
                                    da.T.coords["Tf"].values[i]
                                ).month,
                                convert_np64_datetime(da.T.coords["Tf"].values[i]).day,
                            )
                            for i in range(da.shape[list(da.dims).index("T")])
                        ]
                        tcoords = ["{}-{}-{}/{}-{}-{}".format(*i) for i in tcoords_temp]
                        tcoords = np.asarray(tcoords, dtype="object")
                    if col != "T":
                        f.write(
                            "\t"
                            + "\t".join([str(crd) for crd in da.coords[col].values])
                            + "\n"
                        )
                    else:
                        f.write("\t" + "\t".join([str(crd) for crd in tcoords]) + "\n")
                    if row != "T":
                        temp = np.hstack([da.coords[row].values.reshape(-1, 1), temp])
                    else:
                        temp = np.hstack(
                            [tcoords.reshape(-1, 1), temp.astype("object")]
                        )
                    if row != "T":
                        np.savetxt(f, temp, fmt="%.6f", delimiter="\t")
                    else:
                        np.savetxt(f, temp, fmt="%s", delimiter="\t")
        elif len(extra_dims) == 1 and T is not None:
            da = da.transpose(T, row, col)
            for i in range(da.shape[list(da.dims).index(T)]):
                header = f"cpt:field={da.name}, cpt:{T}={da.coords[T].values[i] if 'Ti' not in da.coords else '{}-{}-{}/{}-{}-{}'.format(convert_np64_datetime(da.coords['Ti'].values[i]).year, convert_np64_datetime(da.coords['Ti'].values[i]).month, convert_np64_datetime(da.coords['Ti'].values[i]).day, convert_np64_datetime(da.coords['Tf'].values[i]).year, convert_np64_datetime(da.coords['Tf'].values[i]).month, convert_np64_datetime(da.coords['Tf'].values[i]).day ) },{'' if 'S' not in da.coords.keys() else ' cpt:S='+ convert_np64_datetime(da.coords['S'].values[i]).strftime('%Y-%m-%dT%H:%M') + ', '} cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}{unitsblurb}{missingblurb}\n"
                if assertmissing:
                    temp = da.isel({T: i}).fillna(float(da.attrs["missing"])).values
                else:
                    temp = da.isel({T: i}).values
                f.write(header)
                if row == "T" or col == "T":
                    tcoords_temp = [
                        (
                            convert_np64_datetime(da.T.coords["Ti"].values[i]).year,
                            convert_np64_datetime(da.T.coords["Ti"].values[i]).month,
                            convert_np64_datetime(da.T.coords["Ti"].values[i]).day,
                            convert_np64_datetime(da.T.coords["Tf"].values[i]).year,
                            convert_np64_datetime(da.T.coords["Tf"].values[i]).month,
                            convert_np64_datetime(da.T.coords["Tf"].values[i]).day,
                        )
                        for i in range(da.shape[list(da.dims).index("T")])
                    ]
                    tcoords = ["{}-{}-{}/{}-{}-{}".format(*i) for i in tcoords_temp]
                    tcoords = np.asarray(tcoords, dtype="object")
                if col != "T":
                    f.write(
                        "\t"
                        + "\t".join([str(crd) for crd in da.coords[col].values])
                        + "\n"
                    )
                else:
                    f.write("\t" + "\t".join([str(crd) for crd in tcoords]) + "\n")
                if row != "T":
                    temp = np.hstack([da.coords[row].values.reshape(-1, 1), temp])
                else:
                    temp = np.hstack([tcoords.reshape(-1, 1), temp.astype("object")])
                if row != "T":
                    np.savetxt(f, temp, fmt="%.6f", delimiter="\t")
                else:
                    np.savetxt(f, temp, fmt="%s", delimiter="\t")

        elif len(extra_dims) == 1 and C is not None:
            da = da.transpose(C, row, col)
            for j in range(da.shape[list(da.dims).index(C)]):
                header = f"cpt:field={da.name}, cpt:{C}={da.coords[C].values[j]}{', cpt:clim_prob=0.33333' if C=='C' else ''}, cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}{unitsblurb}{missingblurb}\n"
                if assertmissing:
                    temp = da.isel({C: j}).fillna(float(da.attrs["missing"])).values
                else:
                    temp = da.isel({T: i}).values
                f.write(header)
                if row == "T" or col == "T":
                    tcoords_temp = [
                        (
                            convert_np64_datetime(da.T.coords["Ti"].values[i]).year,
                            convert_np64_datetime(da.T.coords["Ti"].values[i]).month,
                            convert_np64_datetime(da.T.coords["Ti"].values[i]).day,
                            convert_np64_datetime(da.T.coords["Tf"].values[i]).year,
                            convert_np64_datetime(da.T.coords["Tf"].values[i]).month,
                            convert_np64_datetime(da.T.coords["Tf"].values[i]).day,
                        )
                        for i in range(da.shape[list(da.dims).index("T")])
                    ]
                    tcoords = ["{}-{}-{}/{}-{}-{}".format(*i) for i in tcoords_temp]
                    tcoords = np.asarray(tcoords, dtype="object")
                if col != "T":
                    f.write(
                        "\t"
                        + "\t".join([str(crd) for crd in da.coords[col].values])
                        + "\n"
                    )
                else:
                    f.write("\t" + "\t".join([str(crd) for crd in tcoords]) + "\n")
                if row != "T":
                    temp = np.hstack([da.coords[row].values.reshape(-1, 1), temp])
                else:
                    temp = np.hstack([tcoords.reshape(-1, 1), temp.astype("object")])
                if row != "T":
                    np.savetxt(f, temp, fmt="%.6f", delimiter="\t")
                else:
                    np.savetxt(f, temp, fmt="%s", delimiter="\t")
        else:
            da = da.transpose(row, col)
            header = f"cpt:field={da.name}, cpt:nrow={da.shape[list(da.dims).index(row)]}, cpt:ncol={da.shape[list(da.dims).index(col)]}, cpt:row={row}, cpt:col={col}{unitsblurb}{missingblurb}\n"
            if assertmissing:
                temp = da.fillna(float(da.attrs["missing"])).values
            else:
                temp = da.values
            f.write(header)
            if row == "T" or col == "T":
                tcoords_temp = [
                    (
                        convert_np64_datetime(da.T.coords["Ti"].values[i]).year,
                        convert_np64_datetime(da.T.coords["Ti"].values[i]).month,
                        convert_np64_datetime(da.T.coords["Ti"].values[i]).day,
                        convert_np64_datetime(da.T.coords["Tf"].values[i]).year,
                        convert_np64_datetime(da.T.coords["Tf"].values[i]).month,
                        convert_np64_datetime(da.T.coords["Tf"].values[i]).day,
                    )
                    for i in range(da.shape[list(da.dims).index("T")])
                ]
                tcoords = ["{}-{}-{}/{}-{}-{}".format(*i) for i in tcoords_temp]
                tcoords = np.asarray(tcoords, dtype="object")
            if col != "T":
                f.write(
                    "\t" + "\t".join([str(crd) for crd in da.coords[col].values]) + "\n"
                )
            else:
                f.write("\t" + "\t".join([str(crd) for crd in tcoords]) + "\n")
            if row != "T":
                temp = np.hstack([da.coords[row].values.reshape(-1, 1), temp])
            else:
                temp = np.hstack([tcoords.reshape(-1, 1), temp.astype("object")])
            if row != "T":
                np.savetxt(f, temp, fmt="%.6f", delimiter="\t")
            else:
                np.savetxt(f, temp, fmt="%s", delimiter="\t")
    return opfile
