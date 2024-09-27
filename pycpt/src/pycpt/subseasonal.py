import cartopy
import cartopy.crs as ccrs
import cartopy.mpl.gridliner as gridliner
import datetime
import cptcore as cc
import cptdl as dl
import cptextras as ce
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
import numpy as np
from PIL import Image
from pycpt import missing_value_flag, SKILL_METRICS as SKILL_METRICS_ORIG
from pathlib import Path
import scipy.stats
import xarray as xr

hindcasts = {
    'GEFSv12.PRCP': "https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFSv12_CPC/.hindcast/.weekly/.pr/S/(0000%206%20Jan%201999)/(0000%2028%20Jun%202017)/RANGEEDGES/S/(days%20since%201999-01-01)/streamgridunitconvert/Y/{predictor_extent['south']}/{predictor_extent['north']}/RANGE/X/{predictor_extent['west']}/{predictor_extent['east']}/RANGE/L/{day1}/{day2}/RANGEEDGES/%5BM%5Daverage/L/{nday}/runningAverage/SOURCES/.Models/.SubX/.EMC/.GEFSv12_CPC/.hindcast/.dc0018/.pr/X/{predictor_extent['west']}/{predictor_extent['east']}/RANGE/Y/{predictor_extent['south']}/{predictor_extent['north']}/RANGE/L/{day1}/{day2}/RANGEEDGES/L/{nday}/runningAverage/S/to366daysample/%5BYR%5Daverage/S/sampleDOY/sub/S/({training_season})/VALUES/L/removeGRID/S/(T)/renameGRID/c%3A/0.001/(m3%20kg-1)/%3Ac/mul/c%3A/1000/(mm%20m-1)/%3Ac/mul/c%3A/7.0//units//days/def/%3Ac/mul/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1700/ensotime/12./16/Jan/2100/ensotime/%3Agrid/use_as_grid/T//pointwidth/1/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/{'%5BX/Y%5D%5BT%5Dcptv10.tsv' if filetype == 'cptv10.tsv' else 'data.nc'}",
}

obs_template = {
    'GEFSv12.PRCP': "https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFSv12_CPC/.hindcast/.weekly/.pr/S/(0000%206%20Jan%201999)/(0000%2028%20Jun%202017)/RANGEEDGES/L/{day1}/{day2}/RANGEEDGES/L/{nday}/runningAverage/S/({training_season})/VALUES/L/S/add/%5BL/S%5D//T/sampleNDto1D/{obs_source}/Y/{predictand_extent['south']}/{predictand_extent['north']}/RANGE/X/{predictand_extent['west']}/{predictand_extent['east']}/RANGE/{obsclimo_source}/Y/{predictand_extent['south']}/{predictand_extent['north']}/RANGE/X/{predictand_extent['west']}/{predictand_extent['east']}/RANGE/T/to366daysample/%5BYR%5Daverage/T/sampleDOY/sub/T/%28days%20since%201960-01-01%29/streamgridunitconvert/T/{nday}/runningAverage/c%3A/7.0//units//days/def/%3Ac/mul/T/2/index/.T/SAMPLE/nip/dup/T/npts//I/exch/NewIntegerGRID/replaceGRID/I/3/-1/roll/.T/replaceGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/1700/ensotime/12./16/Jan/2100/ensotime/%3Agrid/use_as_grid/-999/setmissing_value/{'%5BX/Y%5D%5BT%5Dcptv10.tsv' if filetype == 'cptv10.tsv' else 'data.nc'}"
}

obs_source = {
    'CHIRPS.PRCP': 'SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p25/.prcp/X/-180./.5/180./GRID/Y/-90/.5/90/GRID',
}
obsclimo_source = {
    'CHIRPS.PRCP': 'SOURCES/.ECMWF/.S2S/.climatologies/.observed/.CHIRPS/.prcpSmooth/X/-180./.5/180./GRID/Y/-90/.5/90/GRID',
}

monthabbrevs = [None, 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

forecasts = {
    'GEFSv12.PRCP': "https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFSv12_CPC/.forecast/.pr/S/(0000%20{fdate.day}%20{monthabbrevs[fdate.month]}%20{fdate.year})/VALUES/Y/{predictor_extent['south']}/{predictor_extent['north']}/RANGE/X/{predictor_extent['west']}/{predictor_extent['east']}/RANGE/L/{day1}/{day2}/RANGEEDGES/%5BM%5Daverage/L/{nday}/runningAverage/c%3A/86400/(s%20day-1)/%3Ac/mul/SOURCES/.Models/.SubX/.EMC/.GEFSv12_CPC/.hindcast/.dc0018/.pr/Y/{predictor_extent['south']}/{predictor_extent['north']}/RANGE/X/{predictor_extent['west']}/{predictor_extent['east']}/RANGE/L/{day1}/{day2}/RANGEEDGES/L/{nday}/runningAverage/S/(T)/renameGRID/pentadAverage/pentadmean/T/(S)/renameGRID/%5BS%5DregridLinear/S/1/setgridtype/pop/S/2/index/.S/SAMPLE/sub/c%3A/0.001/(m3%20kg-1)/%3Ac/mul/c%3A/1000/(mm%20m-1)/%3Ac/mul/c%3A/7.0//units//days/def/%3Ac/mul/S/(T)/renameGRID/grid%3A//name/(T)/def//units/(months%20since%201960-01-01)/def//standard_name/(time)/def//pointwidth/1/def/16/Jan/2261/ensotime/12.0/16/Jan/2261/ensotime/%3Agrid/use_as_grid/T//pointwidth/0/def/pop//name/(tp)/def//units/(mm)/def//long_name/(precipitation_amount)/def/-999/setmissing_value/%5BX/Y%5D%5BT%5Dcptv10.tsv",

}

# Display RPSS over a range of -20 to 20, compared to -50 to 50 for
# the seasonal version. TODO: make this change in notebook.py?
SKILL_METRICS = dict(
    SKILL_METRICS_ORIG,
    rank_probability_skill_score=(ce.cmaps["cpt_correlation"], -20, 20),
)
SKILL_METRICS["rank_probability_skill_score"][0].set_under("lightgray")


def download_data(
        predictor_names,
        predictand_name,
        download_args,
        files_root,
        force_download,
):
    print('Downloading predictor model hindcast datasets')
    hindcasts = download_hindcasts(
        predictor_names, download_args, files_root, force_download
    )

    print('Downloading observed predictand dataset')
    obs = download_observations(
        predictor_names, predictand_name, download_args, files_root, force_download
    )

    print('Downloading predictor model forecast datasets')
    forecasts = download_forecasts(
        predictor_names, download_args, files_root, force_download
    )
    return hindcasts, obs, forecasts

def download_hindcasts(predictor_names, download_args, files_root, force_download):
    dataDir = files_root / "data"
    model_slices = []
    for model in predictor_names:
        lead_slices = []
        for lead_name, lead_low, lead_high in download_args['leads']:
            basename = f"{model}-{lead_low}-{lead_high}"
            nc_file = dataDir / f"{basename}.nc"
            tsv_file = dataDir / f"{basename}].tsv"
            hindcast_download_args = dict(
                download_args,
                day1=lead_low + .5, # TODO .5 is model-specific?
                day2=lead_high + .5,
                nday=lead_high - lead_low + 1,
            )
            if not nc_file.is_file() or force_download:
                X = dl.download(
                    hindcasts[model],
                    tsv_file,
                    **hindcast_download_args,
                    verbose=True,
                    use_dlauth=False,
                )
                X.to_netcdf(nc_file)
            else:
                X = xr.open_dataset(nc_file)
            X = getattr(X, [i for i in X.data_vars][0])
            X = X.assign_coords(lead_name=lead_name)
            lead_slices.append(X)
        model_slice = xr.concat(lead_slices, 'lead_name')
        model_slice = model_slice.assign_coords(model=model)
        model_slices.append(model_slice)
    X = xr.concat(model_slices, 'model')
    return X

def download_observations(predictor_names, predictand_name,download_args, files_root, force_download):
    dataDir = files_root / "data"
    lead_slices = []
    for lead_name, lead_low, lead_high in download_args['leads']:
        basename = f"{predictand_name}-{lead_low}-{lead_high}"
        nc_file = dataDir / f"{basename}.nc"
        tsv_file = dataDir / f"{basename}].tsv"
        obs_download_args = dict(
            download_args,
            day1=lead_low + .5, # TODO .5 is model-specific?
            day2=lead_high + .5,
            nday=lead_high - lead_low + 1,
            obs_source=obs_source[predictand_name],
            obsclimo_source=obsclimo_source[predictand_name],
        )
        if not nc_file.is_file() or force_download:
            Y1 = dl.download(
                obs_template[f"{predictor_names[0]}"],
                tsv_file,
                **obs_download_args,
                verbose=True,
                use_dlauth=False,
            )
            Y1.to_netcdf(nc_file)
        else:
            Y1 = xr.open_dataset(nc_file)
        Y1 = getattr(Y1, [i for i in Y1.data_vars][0])
        Y1 = Y1.assign_coords(lead_name=lead_name)
        lead_slices.append(Y1)
    Y = xr.concat(lead_slices, 'lead_name').rename(predictand_name)
    return Y


def download_forecasts(predictor_names, download_args, files_root, force_download):
    dataDir = files_root / "data"
    model_slices = []
    fdate = download_args['fdate']
    for model in predictor_names:
        lead_slices = []
        for lead_name, lead_low, lead_high in download_args['leads']:
            basename = f"{model}-{fdate.year}-{fdate.month}-{fdate.day}-L{lead_low}-{lead_high}"
            nc_file = dataDir / f"{basename}.nc"
            tsv_file = dataDir / f"{basename}].tsv"
            forecast_download_args = dict(
                download_args,
                day1=lead_low + .5, # TODO .5 is model-specific?
                day2=lead_high + .5,
                nday=lead_high - lead_low + 1,
                monthabbrevs=monthabbrevs,
            )
            if not nc_file.is_file() or force_download:
                F = dl.download(
                    forecasts[model],
                    tsv_file,
                    **forecast_download_args,
                    verbose=True,
                    use_dlauth=False,
                )
                F.to_netcdf(nc_file)
            else:
                F = xr.open_dataset(nc_file)
            F = getattr(F, [i for i in F.data_vars][0])
            F = F.assign_coords(lead_name=lead_name)
            lead_slices.append(F)
        model_slice = xr.concat(lead_slices, 'lead_name')
        model_slice = model_slice.assign_coords(model=model)
        model_slices.append(model_slice)
    F = xr.concat(model_slices, 'model')
    return F

def evaluate_models(hindcast_data, forecast_data, Y, MOS, cpt_args, domain_dir, interactive=False):
    outputDir = domain_dir / "output"
    hcst_model_slices, fcst_model_slices, skill_model_slices, px_model_slices, py_model_slices = [], [], [], [], []

    for model in hindcast_data['model'].values:
        hcst_lead_slices, fcst_lead_slices, skill_lead_slices, px_lead_slices, py_lead_slices = [], [], [], [], []
        for lead_name in hindcast_data['lead_name'].values:
            if str(MOS).upper() == 'CCA':
                hindcast1 = hindcast_data.sel(model=model, lead_name=lead_name, drop=True)
                forecast1 = forecast_data.sel(model=model, lead_name=lead_name, drop=True)
                Y1 = Y.sel(lead_name=lead_name, drop=True)
                
                # fit CCA model between X & Y and produce real-time forecasts for F
                cca_h, cca_rtf, cca_s, cca_px, cca_py = cc.canonical_correlation_analysis(
                    hindcast1, Y1,  F=forecast1, **cpt_args,
                )

                hcst_lead_slices.append(cca_h.assign_coords(lead_name=lead_name))
                fcst_lead_slices.append(cca_rtf.assign_coords(lead_name=lead_name))
                skill_lead_slices.append(cca_s.where(cca_s > -999, other=np.nan).assign_coords(lead_name=lead_name))
                px_lead_slices.append(cca_px.assign_coords(lead_name=lead_name))
                py_lead_slices.append(cca_py.assign_coords(lead_name=lead_name))

            elif str(MOS).upper() == 'PCR':
                raise Exception('PCR not yet implemented for subseasonal')
            else:
                # simply compute deterministic skill scores of non-corrected ensemble means
                nomos_skill = cc.deterministic_skill(hindcast1, Y1, **cpt_args)
                skill_lead_slices.append(nomos_skill.where(nomos_skill > -999, other=np.nan).assign_coords(lead_name=lead_name))

            # choose what data to export here (any of the above results data arrays can be saved to netcdf)
            # TODO include forecast date in the filenames?
            basename = f"{model}-{lead_name}"
            if str(MOS).upper() == 'CCA':
                cca_h.to_netcdf(outputDir /  (basename + '_crossvalidated_cca_hindcasts.nc'))
                cca_rtf.to_netcdf(outputDir / (basename + '_realtime_cca_forecasts.nc'))
                cca_s.to_netcdf(outputDir / (basename + '_skillscores_cca.nc'))
                cca_px.to_netcdf(outputDir / (basename + '_cca_x_spatial_loadings.nc'))
                cca_py.to_netcdf(outputDir / (basename + '_cca_y_spatial_loadings.nc'))
            elif str(MOS).upper() == 'PCR':
                raise Exception('PCR not yet implemented for subseasonal')
            else:
                nomos_skill.to_netcdf(outputDir / (basename + '_nomos_skillscores.nc'))
            
        hcst_model_slices.append(
            xr.concat(hcst_lead_slices, 'lead_name')
            .assign_coords(model=model)
        )
        fcst_model_slices.append(
            xr.concat(fcst_lead_slices, 'lead_name')
            .assign_coords(model=model)
        )
        skill_model_slices.append(
            xr.concat(skill_lead_slices, 'lead_name')
            .assign_coords(model=model)
        )
        px_model_slices.append(
            xr.concat(px_lead_slices, 'lead_name')
            .assign_coords(model=model)
        )
        py_model_slices.append(
            xr.concat(py_lead_slices, 'lead_name')
            .assign_coords(model=model)
        )

    hcsts = xr.concat(hcst_model_slices, 'model')
    fcsts = xr.concat(fcst_model_slices, 'model')
    skill = xr.concat(skill_model_slices, 'model')
    pxs = xr.concat(px_model_slices, 'model')
    pys = xr.concat(py_model_slices, 'model')
    return hcsts, fcsts, skill, pxs, pys


def plot_skill(skill, MOS, files_root, skill_metrics):
    nblocks = len(skill_metrics)
    nrows_per_block = len(skill['model'])
    nrows = nblocks * nrows_per_block
    ncols = len(skill['lead_name'])
    fig, ax = plt.subplots(
        nrows=nrows,
        ncols=ncols,
        subplot_kw={"projection": cartopy.crs.PlateCarree()},
        figsize=(8, 2 * nrows),
        squeeze=False,
    )
    for b, metric_name in enumerate(skill_metrics):
        metric = SKILL_METRICS[metric_name]
        for row_of_block, model in enumerate(skill['model'].values):
            for c, lead_name in enumerate(skill['lead_name'].values):
                r = b * nrows_per_block + row_of_block
                (
                    skill[metric_name].sel(model=model, lead_name=lead_name)
                    .where(lambda x: x > missing_value_flag)
                    .plot(
                        ax=ax[r][c],
                        cmap=metric[0],
                        vmin=metric[1],
                        vmax=metric[2],
                        # colorbar only on last (rightmost) plot
                        add_colorbar=c == len(skill['lead_name']) - 1)
                )
                ax[r][c].coastlines()
                ax[r][c].add_feature(cartopy.feature.BORDERS)
                ax[r][c].set_title(lead_name)

            ax[r][0].text(
                -0.07,
                0.55,
                model.upper(),
                va="bottom",
                ha="center",
                rotation="vertical",
                rotation_mode="anchor",
                transform=ax[r][0].transAxes,
            )

    # save plots
    figName = MOS + "_models_skillMatrices.png"
    fig.savefig(
        files_root / "figures" / figName,
        bbox_inches="tight",
    )


def plot_eof_modes(
        pxs, pys, MOS, files_root
):
    nmodes = 5
    cmap = plt.get_cmap("cpt.loadings", 11)
    vmin = -10
    vmax = 10

    graph_orientation = ce.graphorientation(
        len(pys["X"]),
        len(pys["Y"])
    )

    if MOS == "CCA":
        for i, model in enumerate(pys['model'].values):
            for j, lead_name in enumerate(pys['lead_name'].values):
                for k, mode in enumerate(pys['Mode'][0:nmodes + 1].values):
                    px = pxs.sel(model=model, lead_name=lead_name, Mode=mode)
                    py = pys.sel(model=model, lead_name=lead_name, Mode=mode)
                    if i == 0 and j == 0 and k == 0:
                        loadings = (
                            px['x_eof_loadings']
                            .where(lambda x: x > missing_value_flag)
                        )
                        Vmin, Vmax = ce.standardized_range(
                            loadings.min().item(),
                            loadings.max().item(),
                        )

                    print(f"{model.upper()} {lead_name}: EOF {int(mode)}")

                    fig = plt.figure(figsize=(20, 5))

                    gs0 = gridspec.GridSpec(1, 3, figure=fig)
                    gs00 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[0])
                    gs01 = gridspec.GridSpecFromSubplotSpec(4, 5, subplot_spec=gs0[1])
                    gs02 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[2])
                    ts = xr.concat(
                        [px['x_eof_scores'], py['y_eof_scores']],
                        "M",
                    ).assign_coords({"M": ["x", "y"]})

                    map1_ax = fig.add_subplot(gs00[:, :], projection=ccrs.PlateCarree())
                    ts_ax = fig.add_subplot(gs01[1:3, 1:])
                    map2_ax = fig.add_subplot(gs02[:, :], projection=ccrs.PlateCarree())

                    art = (
                        px.x_eof_loadings
                        .where(lambda x: x > missing_value_flag)
                        .plot(
                            ax=map1_ax, add_colorbar=False, vmin=Vmin, vmax=Vmax, cmap=cmap
                        )
                    )

                    canvarX = round(
                        float(
                            px
                            .x_explained_variance
                            .where(lambda x: x > missing_value_flag)
                            .max()
                        ),
                        1,
                    )

                    cb = plt.colorbar(art, orientation=graph_orientation)
                    cb.set_label(label="x_eof_loadings", size=14)
                    cb.ax.tick_params(labelsize=12)
                    if graph_orientation == "horizontal":
                        cb.ax.tick_params(axis="x", which="major", rotation=-45)
                    # cb.ax.set_xticks(len(6))
                    art = (
                        py.y_eof_loadings
                        .where(lambda x: x > missing_value_flag)
                        .plot(
                            ax=map2_ax, add_colorbar=False, vmin=Vmin, vmax=Vmax, cmap=cmap
                        )
                    )

                    canvarY = round(
                        float(
                            py
                            .y_explained_variance
                            .where(lambda x: x > missing_value_flag)
                            .max()
                        ),
                        1,
                    )
                    cb = plt.colorbar(art, orientation=graph_orientation)
                    ticks_loc = cb.ax.get_xticklabels()
                    cb.set_label(label="y_eof_loadings", size=14)
                    cb.ax.tick_params(labelsize=12)

                    if graph_orientation == "horizontal":
                        cb.ax.tick_params(axis="x", which="major", rotation=-45)

                    primitive = ts.plot.line(
                        marker="x", ax=ts_ax, markersize=12, hue="M", add_legend=False
                    )
                    ts_ax.grid(axis="x", linestyle="-.")
                    ts_ax.legend(
                        handles=primitive, labels=list(ts.coords["M"].values), loc="best"
                    )
                    ts_ax.spines["top"].set_visible(False)
                    ts_ax.spines["right"].set_visible(False)
                    ts_ax.spines["bottom"].set_visible(False)
                    ts_ax.set_title("EOF Scores (Mode {})".format(mode))
                    ts_ax.set_ylabel(None)
                    ts_ax.set_xlabel(None)

                    map1_ax.set_title(
                        "X EOF MODE {} = {}%".format(mode, ce.truncate(canvarX, 2))
                    )
                    map2_ax.set_title(
                        "Y EOF MODE {} = {}%".format(mode, ce.truncate(canvarY, 2))
                    )

                    map1_ax.coastlines()
                    map2_ax.coastlines()
                    map1_ax.add_feature(cartopy.feature.BORDERS)
                    map2_ax.add_feature(cartopy.feature.BORDERS)
                    plt.show()

                    # save plots
                    figName = MOS + "_" + str(model) + "_EOF_mode_" + str(int(mode)) + ".png"
                    fig.savefig(files_root / "figures" / figName, bbox_inches="tight")
    elif MOS == "PCR":
        raise Exception('PCR not yet implemented for subseasonal')
    else:
        print("You will need to set MOS=CCA in order to see CCA Modes")


def plot_cca_modes(
        pxs, pys, MOS, files_root
):
    nmodes = 3
    cmap = plt.get_cmap("cpt.loadings", 11)
    vmin = -10
    vmax = 10
    missing_value_flag = -999

    if MOS == "CCA":
        for i, model in enumerate(pys['model'].values):
            for j, lead_name in enumerate(pys['lead_name'].values):
                for k, mode in enumerate(pys['Mode'].values[:nmodes+1]):
                    px = pxs.sel(model=model, lead_name=lead_name)
                    py = pys.sel(model=model, lead_name=lead_name)
                    if i == 0 and j == 0 and k == 0:
                        loadings = (
                            px['x_cca_loadings']
                            .sel(Mode=mode)
                            .where(lambda x: x > missing_value_flag)
                        )
                        Vmin, Vmax = ce.standardized_range(
                            loadings.min().item(),
                            loadings.max().item()
                        )

                    cancorr = np.correlate(
                        px['x_cca_scores'].sel(Mode=mode),
                        py['y_cca_scores'].sel(Mode=mode),
                    )
                    print(
                        f"{model.upper()} {lead_name}: CCA MODE {int(mode)}"
                        f" - Canonical Correlation = {ce.truncate(cancorr[0], 2)}"
                    )

                    fig = plt.figure(figsize=(20, 5))

                    gs0 = gridspec.GridSpec(1, 3, figure=fig)
                    gs00 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[0])
                    gs01 = gridspec.GridSpecFromSubplotSpec(5, 10, subplot_spec=gs0[1])
                    gs02 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[2])
                    ts = xr.concat(
                        [
                            px['x_cca_scores'].sel(Mode=mode),
                            py['y_cca_scores'].sel(Mode=mode),
                        ],
                        "M",
                    ).assign_coords({"M": ["x", "y"]})

                    map1_ax = fig.add_subplot(gs00[:, :], projection=ccrs.PlateCarree())
                    ts_ax = fig.add_subplot(gs01[1:3, 1:])
                    map2_ax = fig.add_subplot(gs02[:, :], projection=ccrs.PlateCarree())

                    art = (
                        px
                        .x_cca_loadings.sel(Mode=mode)
                        .where(lambda x: x > missing_value_flag)
                        .plot(
                            ax=map1_ax, add_colorbar=False, vmin=Vmin, vmax=Vmax, cmap=cmap
                        )
                    )

                    graph_orientation = ce.graphorientation(
                        len(pys["X"]),
                        len(pys["Y"])
                    )

                    cb = plt.colorbar(art, orientation=graph_orientation)
                    cb.set_label(label="x_cca_loadings", size=14)
                    cb.ax.tick_params(labelsize=12)

                    art = (
                        py
                        .y_cca_loadings.sel(Mode=mode)
                        .where(lambda x: x > missing_value_flag)
                        .plot(
                            ax=map2_ax, add_colorbar=False, vmin=Vmin, vmax=Vmax, cmap=cmap
                        )
                    )
                    cb = plt.colorbar(art, orientation=graph_orientation)
                    cb.set_label(label="y_cca_loadings", size=14)
                    cb.ax.tick_params(labelsize=12)

                    primitive = ts.plot.line(
                        marker="x", ax=ts_ax, markersize=12, hue="M", add_legend=False
                    )
                    ts_ax.grid(axis="x", linestyle="-.")
                    ts_ax.legend(
                        handles=primitive, labels=list(ts.coords["M"].values), loc="best"
                    )
                    ts_ax.spines["top"].set_visible(False)
                    ts_ax.spines["right"].set_visible(False)
                    ts_ax.spines["bottom"].set_visible(False)
                    ts_ax.set_title("CCA Scores (Mode {})".format(mode + 1))
                    ts_ax.set_ylabel(None)
                    ts_ax.set_xlabel(None)

                    map1_ax.set_title("X CCA MODE {}".format(mode + 1))
                    map2_ax.set_title("Y CCA MODE {}".format(mode + 1))

                    map1_ax.coastlines()
                    map2_ax.coastlines()

                    map1_ax.add_feature(cartopy.feature.BORDERS)
                    map2_ax.add_feature(cartopy.feature.BORDERS)
                    plt.show()

                    # save plots
                    figName = MOS + "_" + str(model) + "_CCA_mode_" + str(int(mode)) + ".png"
                    fig.savefig(files_root / "figures" / figName, bbox_inches="tight")
    else:
        print("You will need to set MOS=CCA in order to see CCA Modes")



def plot_forecasts(
    fcsts,
    predictand_name,
    MOS,
    cpt_args,
    files_root,
    color_bar=None,
    vmin=None,
    vmax=None,
):
    prob_missing_value_flag = -1
    my_dpi = 100

    graph_orientation = ce.graphorientation(
        len(fcsts["X"]),
        len(fcsts["Y"])
    )

    # colormap for deterministic forecast
    det_title, vmin_auto, vmax_auto, barcolor = ce.prepare_canvas(
        cpt_args["tailoring"],
        predictand_name,
        user_color=color_bar,
        user_vmin=vmin,
        user_vmax=vmax
    )

    # colormap for probabilistic forecast
    cmapB, cmapN, cmapA = ce.prepare_canvas(None, predictand_name, "probabilistic")

    for i, model in enumerate(fcsts['model'].values):
        for j, lead_name in enumerate(fcsts['lead_name'].values):
            f = fcsts.sel(model=model, lead_name=lead_name)
            if graph_orientation == "horizontal":
                fig = plt.figure(figsize=(18, 10), facecolor="w", dpi=my_dpi)
            else:
                fig = plt.figure(figsize=(15, 12), facecolor="w", dpi=my_dpi)

            matplotlibInstance, cartopyInstance = ce.view_probabilistic(
                f
                .probabilistic.where(lambda x: x > prob_missing_value_flag)
                .rename({"C": "M"})
                .isel(T=-1)
                / 100,
                cmap_an=cmapA,
                cmap_bn=cmapB,
                cmap_nn=cmapN,
                orientation=graph_orientation,
            )
            cartopyInstance.add_feature(cartopy.feature.BORDERS, edgecolor="black")
            cartopyInstance.set_title("")

            cartopyInstance.spines["left"].set_color("blue")

            matplotlibInstance.savefig(
                files_root / "figures" / "Test.png",
                bbox_inches="tight",
            )  # ,pad_inches = 0)

            matplotlibInstance.clf()
            cartopyInstance.cla()

            ax1 = fig.add_subplot(2, 2, 1)
            ax1.set_axis_off()
            prob_var_desc = 'Dominant Tercile Probability'
            ax1.set_title(f"{model.upper()} - Probabilistic Forecast {lead_name}")
            pil_img = Image.open(
                files_root / "figures" / "Test.png"
            )
            ax1.set_xticks([])
            ax1.set_yticks([])
            ax1.imshow(pil_img)

            datart = (
                f
                .deterministic.where(lambda x: x > missing_value_flag)
                .isel(T=-1)
            )
            if (
                any(x in predictand_name for x in ["TMAX", "TMIN", "TMEAN", "TMED"])
                and i == 0 and j == 0
            ):
                vmin = round(float(datart.min()) - 0.5 * 2) / 2

            art = datart.plot(
                figsize=(12, 10),
                aspect="equal",
                yincrease=True,
                subplot_kws={"projection": ccrs.PlateCarree()},
                # cbar_kwargs={'location': 'bottom',
                # "label": "Temperature (°C)"
                #'xticklabels':{'fontsize':100},
                #            },
                extend="neither",
                add_colorbar=False,
                transform=ccrs.PlateCarree(),
                cmap=barcolor,
                vmin=vmin,
                vmax=vmax,
            )

            plt.title("")
            # plt.axis("off")
            art.axes.coastlines()

            cb = plt.colorbar(art, orientation=graph_orientation)  # location='bottom')
            cb.set_label(
                label=datart.attrs["field"] + " [" + datart.attrs["units"] + "]", size=16
            )
            cb.ax.tick_params(labelsize=15)

            art.axes.add_feature(
                cartopy.feature.BORDERS, edgecolor="black"
            )  # ,linewidth=4.5
            art.axes.coastlines(edgecolor="black")  # ,linewidth=4.5
            plt.savefig(
                files_root / "figures" / "Test.png",
                bbox_inches="tight",
                pad_inches=0,
            )

            ax2 = fig.add_subplot(2, 2, 2)
            ax2.set_axis_off()

            ax2.set_title(f"{model.upper()} - Deterministic Forecast {lead_name}")
            pil_img = Image.open(
                files_root / "figures" / "Test.png"
            )
            ax2.set_xticks([])
            ax2.set_yticks([])
            ax2.imshow(pil_img)  # , aspect=4 1.45 , extent=[0, 1.45, 1.5, 0],

            # save plots
            figName = (
                MOS
                + "var"
                + "["
                + model
                + "]"
                + "[determinstic-probabilistic]-Forecast"
                + ".png"
            )
            fig.savefig(
                files_root / "figures" / figName,
                bbox_inches="tight",
            )
            plt.close()

def construct_mme(hcsts, Y, fcsts, ensemble, cpt_args, domain_dir):
    outputDir = domain_dir / "output"

    for model in ensemble:
        assert model in hcsts['model'], "all members of the nextgen ensemble must be in hindcasts - {} is not".format(model)

    units = hcsts.attrs['units']
    assert Y.attrs['units'] == units
    assert fcsts.attrs['units'] == units

    hcsts = hcsts.sel(model=ensemble).mean('model')
    fcsts = fcsts.sel(model=ensemble).mean('model')

    det_fcst = fcsts['deterministic']
    det_hcst = hcsts['deterministic']
    pr_fcst = fcsts['probabilistic']
    pr_hcst = hcsts['probabilistic']
    pev_fcst = fcsts['prediction_error_variance']
    pev_hcst = hcsts['prediction_error_variance']

    nextgen_skill = []
    for lead_name in hcsts['lead_name'].values:
        h1 = hcsts.sel(lead_name=lead_name, drop=True)
        h1['deterministic'].attrs['units'] = units
        h1['probabilistic'].attrs['units'] = 'percent'
        for v in h1.data_vars.values():
            v.attrs['missing'] = missing_value_flag
        Y1 = Y.sel(lead_name=lead_name, drop=True)
        Y1.attrs['missing'] = missing_value_flag
        Y1.attrs['units'] = units

        nextgen_skill_deterministic = cc.deterministic_skill(h1['deterministic'], Y1, **cpt_args)
        nextgen_skill_probabilistic = cc.probabilistic_forecast_verification(h1['probabilistic'], Y1, **cpt_args)
        nextgen_skill.append(
            xr.merge(
                [nextgen_skill_deterministic, nextgen_skill_probabilistic]
            ).assign_coords(lead_name=lead_name))
    nextgen_skill = xr.concat(nextgen_skill, dim='lead_name')

    # write out files to outputs directory (NB: generic filenaming neeeds improving)
    det_fcst.to_netcdf(outputDir / ('MME_deterministic_forecasts.nc'))
    det_hcst.to_netcdf(outputDir / ('MME_deterministic_hindcasts.nc'))
    pev_fcst.to_netcdf(outputDir / ('MME_forecast_prediction_error_variance.nc'))
    pev_hcst.to_netcdf(outputDir / ('MME_hindcast_prediction_error_variance.nc'))
    pr_fcst.to_netcdf(outputDir / ('MME_probabilistic_forecasts.nc'))
    pr_hcst.to_netcdf(outputDir / ('MME_probabilistic_hindcasts.nc'))
    nextgen_skill.to_netcdf(outputDir / ('MME_skill_scores.nc'))

    return det_fcst, pr_fcst, pev_fcst, nextgen_skill


def plot_mme_skill(
        predictor_names, nextgen_skill, MOS, files_root, skill_metrics
):
    graph_orientation = ce.graphorientation(
        len(nextgen_skill["X"]),
        len(nextgen_skill["Y"])
    )

    nleads = len(nextgen_skill['lead_name'])
    nmetrics = len(skill_metrics)
    fig, ax = plt.subplots(
        nrows=nleads,
        ncols=nmetrics,
        subplot_kw={"projection": ccrs.PlateCarree()},
        figsize=(10 * nleads, 1 * nmetrics),
        squeeze=False,
    )

    for i, lead_name in enumerate(nextgen_skill['lead_name'].values):
        for j, skill_metric in enumerate(skill_metrics):
            metric = SKILL_METRICS[skill_metric]
            ax[i][j].set_title(skill_metric)
            n = (
                nextgen_skill[skill_metric].sel(lead_name=lead_name)
                .where(lambda x: x > missing_value_flag)
                .plot(
                    ax=ax[i][j],
                    cmap=metric[0],
                    vmin=metric[1],
                    vmax=metric[2],
                    add_colorbar=False,
                )
            )

            ax[i][j].coastlines()
            ax[i][j].add_feature(cartopy.feature.BORDERS)
            ax[i][j].set_title(skill_metric.upper())

            cb = plt.colorbar(n, orientation=graph_orientation)  # location='bottom')
            cb.set_label(label=skill_metric, size=15)
            cb.ax.tick_params(labelsize=12)

    # save plots
    figName = MOS + "_ensemble_forecast_skillMatrices.png"
    fig.savefig(
        files_root / "figures" / figName,
        bbox_inches="tight",
    )


def plot_mme_forecasts(
    cpt_args,
    predictand_name,
    pr_fcst,
    MOS,
    files_root,
    det_fcst,
):
    missing_value_flag = -999
    prob_missing_value_flag = -1

    my_dpi = 80

    graph_orientation = ce.graphorientation(
        len(det_fcst["X"]),
        len(det_fcst["Y"])
    )

    if graph_orientation == "horizontal":
        fig = plt.figure(figsize=(18, 10), dpi=my_dpi)
    else:
        fig = plt.figure(figsize=(15, 12), dpi=my_dpi)

    ForTitle, vmin, vmax, barcolor = ce.prepare_canvas(
        cpt_args["tailoring"], predictand_name
    )
    cmapB, cmapN, cmapA = ce.prepare_canvas(None, predictand_name, "probabilistic")

    nleads = len(pr_fcst['lead_name'])
    for i, lead_name in enumerate(pr_fcst['lead_name']):
        matplotlibInstance, cartopyInstance = ce.view_probabilistic(
            pr_fcst.sel(lead_name=lead_name)
            .where(lambda x: x > prob_missing_value_flag)
            .rename({"C": "M"})
            .isel(T=-1)
            / 100,
            cmap_an=cmapA,
            cmap_bn=cmapB,
            cmap_nn=cmapN,
            orientation=graph_orientation,
        )

        cartopyInstance.add_feature(cartopy.feature.BORDERS)
        cartopyInstance.set_title("")
        # cartopyInstance.axis("off")

        figName = MOS + "_ensemble_probabilistic-deterministicForecast-lead-{lead_name}.png"
        plt.savefig(
            files_root / "figures" / "Test.png",
            bbox_inches="tight",
        )

        matplotlibInstance.clf()
        cartopyInstance.cla()

        ax1 = fig.add_subplot(nleads, 2, 2*i + 1)
        ax1.set_axis_off()
        ax1.set_title(MOS + "_ensemble" + " - Probabilistic Forecasts " + ForTitle)
        pil_img = Image.open(
            files_root / "figures" / "Test.png"
        )
        ax1.set_xticks([])
        ax1.set_yticks([])
        ax1.imshow(pil_img)

        datart = det_fcst.sel(lead_name=lead_name).where(lambda x: x > missing_value_flag).isel(T=-1)
        if any(x in predictand_name for x in ["TMAX", "TMIN", "TMEAN", "TMED"]) and i == 0:
            vmin = round(float(datart.min()) - 0.5 * 2) / 2

        art = datart.plot(
            figsize=(12, 10),
            aspect="equal",
            yincrease=True,
            # size=45,
            subplot_kws={"projection": ccrs.PlateCarree()},
            extend="neither",
            add_colorbar=False,
            transform=ccrs.PlateCarree(),
            cmap=barcolor,
            vmin=vmin,
            vmax=vmax,
        )

        plt.title("")
        art.axes.coastlines()

        cb = plt.colorbar(art, orientation=graph_orientation)
        cb.set_label(label=datart.name, size=16)
        cb.ax.tick_params(labelsize=15)

        art.axes.add_feature(cartopy.feature.BORDERS, edgecolor="black")  # ,linewidth=4.5
        art.axes.coastlines(edgecolor="black")

        plt.savefig(
            files_root / "figures" / "Test.png",
            bbox_inches="tight",
        )

        ax2 = fig.add_subplot(nleads, 2, 2*i + 2)
        ax2.set_axis_off()
        ax2.set_title(MOS + "_ensemble" + " - Deterministic Forecasts " + ForTitle)
        pil_img = Image.open(
            files_root / "figures" / "Test.png"
        )
        ax2.set_xticks([])
        ax2.set_yticks([])
        ax2.imshow(pil_img)

        fig.savefig(
            files_root / "figures" / figName,
            bbox_inches="tight",
        )
        plt.close()


def plot_flex_forecasts(
    predictand_name,
    exceedance_prob,
    point_latitude,
    point_longitude,
    predictand_extent,
    threshold,
    fcst_scale,
    climo_scale,
    fcst_mu,
    climo_mu,
    Y2,
    is_transformed,
    ntrain,
    Y,
    MOS,
    files_root,
    color_bar,
):
    if point_latitude is None:
        point_latitude = round(
            (
                predictand_extent['south'] +
                predictand_extent['north']
            )/2,
            2
        )
    elif (
            point_latitude  < float(predictand_extent['south'])
            or
            point_latitude > float(predictand_extent['north'])
    ):
        raise Exception(
            f"point_latitude {point_latitude} is outside predictor domain "
            f"{predictand_extent['south']} to "
            f"{predictand_extent['north']}"
        )

    if point_longitude is None:
        point_longitude = round(
            (
                predictand_extent['west'] +
                predictand_extent['east']
            )/2,
            2
        )
    elif (
            point_longitude < float(predictand_extent['west'])
            or
            point_longitude > float(predictand_extent['east'])
    ):
        raise Exception(
            f"point_longitude {point_longitude} is outside predictor domain "
            f"{predictand_extent['west']} to "
            f"{predictand_extent['east']}"
        )

    graph_orientation = ce.graphorientation(
        len(Y["X"]),
        len(Y["Y"])
    )

    if point_latitude < float(
        predictand_extent["south"]
    ) or point_latitude > float(predictand_extent["north"]):
        point_latitude = round(
            (
                predictand_extent["south"]
                + predictand_extent["north"]
            )
            / 2,
            2,
        )

    if point_longitude < float(
        predictand_extent["west"]
    ) or point_longitude > float(predictand_extent["east"]):
        point_longitude = round(
            (
                predictand_extent["west"]
                + predictand_extent["east"]
            )
            / 2,
            2,
        )

    # setting up canvas on which to draw

    ForTitle, vmin, vmax, mark, barcolor = ce.prepare_canvas('POE',predictand_name,user_color=color_bar)

    nleads = len(fcst_mu['lead_name'])

    if graph_orientation == "horizontal":
        fig = plt.figure(figsize=(15, 20 * nleads))
    else:
        fig = plt.figure(figsize=(10, 20 * nleads))

    gs0 = gridspec.GridSpec(4*nleads, 1, figure=fig)

    for i, lead_name in enumerate(fcst_mu['lead_name'].values):
        # plot exceedance probability map
        gs00 = gridspec.GridSpecFromSubplotSpec(1, 1, subplot_spec=gs0[i*4:i*4+3])
        gs11 = gridspec.GridSpecFromSubplotSpec(1, 2, subplot_spec=gs0[i*4+3])
        gs01 = gridspec.GridSpecFromSubplotSpec(1, 1, subplot_spec=gs11[0])
        gs02 = gridspec.GridSpecFromSubplotSpec(1, 1, subplot_spec=gs11[1])

        map_ax = fig.add_subplot(gs00[:, :], projection=ccrs.PlateCarree(), aspect="auto")
        cdf_ax = fig.add_subplot(gs01[:, :], aspect="auto")
        pdf_ax = fig.add_subplot(gs02[:, :], aspect="auto")

        # plot the map
        art = exceedance_prob.sel(lead_name=lead_name).transpose("Y", "X", ...).plot(
            cmap=barcolor, ax=map_ax, vmin=vmin, vmax=vmax
        )
        map_ax.scatter(
            [point_longitude],
            [point_latitude],
            marker="x",
            s=100,
            color=mark,
            transform=ccrs.PlateCarree(),
        )
        coasts = art.axes.coastlines()
        art.axes.add_feature(cartopy.feature.BORDERS)
        gl = map_ax.gridlines(
                crs=ccrs.PlateCarree(),
                draw_labels=True,
                linewidth=1,
                color='gray',
                alpha=0.5,
                linestyle='--'
            )
        gl.top_labels = False
        gl.right_labels = False
        gl.xformatter = gridliner.LongitudeFormatter()
        gl.yformatter = gridliner.LatitudeFormatter()

        title = map_ax.set_title(f"Probability of Exceedance {lead_name}")

        # point calculations - select the nearest point to the lat/lon the user wanted to plot curves
        def point_value(arr):
            return float(
                arr
                .sel(lead_name=lead_name)
                .sel(X=point_longitude, Y=point_latitude, method="nearest")
                .item()
            )

        point_threshold = point_value(threshold)
        point_fcst_scale = point_value(fcst_scale)
        point_climo_scale = point_value(climo_scale)
        point_fcst_mu = point_value(fcst_mu)
        point_climo_mu = point_value(climo_mu)
        point_climo = np.squeeze(
            Y2.sel(lead_name=lead_name)
            .sel(X=point_longitude, Y=point_latitude, method="nearest")
            .values
        )
        point_climo.sort()

        if is_transformed:
            point_climo_mu_nontransformed = float(
                Y.mean("T")
                .sel(**{"X": point_longitude, "Y": point_latitude}, method="nearest")
                .values
            )
            point_climo_std_nontransformed = float(
                Y.std("T")
                .sel(**{"X": point_longitude, "Y": point_latitude}, method="nearest")
                .values
            )

        x = point_climo
        x1 = np.linspace(x.min(), x.max(), 1000)
        cprobth = (
            sum(x >= point_threshold) / x.shape[0]
        )  # round(t.sf(point_threshold, ntrain, loc=point_climo_mu, scale=point_climo_scale),2)
        fprobth = round(
            scipy.stats.t.sf(point_threshold, ntrain, loc=point_fcst_mu, scale=point_fcst_scale), 2
        )

        # POE plot
        cdf_ax.plot(
            x,
            [sum(x >= x[i]) / x.shape[0] for i in range(x.shape[0])],
            "g-",
            lw=2,
            marker="x",
            alpha=0.8,
            label="clim (empirical)",
        )
        cdf_ax.plot(
            x1,
            scipy.stats.t.sf(x1, ntrain, loc=point_fcst_mu, scale=point_fcst_scale),
            "r-",
            lw=1,
            alpha=0.8,
            label="fcst",
        )
        cdf_ax.plot(
            x1,
            scipy.stats.norm.sf(x1, loc=point_climo_mu, scale=point_fcst_scale),
            "b-",
            lw=1,
            alpha=0.8,
            label="clim (fitted)",
        )

        cdf_ax.plot(point_threshold, fprobth, "ok")
        cdf_ax.plot(point_threshold, cprobth, "ok")
        cdf_ax.axvline(x=point_threshold, color="k", linestyle="--")
        cdf_ax.set_title(f"Point Probabilities of Exceedance {lead_name}")
        cdf_ax.set_xlabel(Y.name.upper())
        cdf_ax.set_ylabel("Probability (%)")
        cdf_ax.legend(loc="best", frameon=False)

        # PDF plot
        fpdf = scipy.stats.t.pdf(x1, ntrain, loc=point_fcst_mu, scale=point_fcst_scale)

        pdf_ax.plot(
            x1,
            scipy.stats.norm.pdf(x1, loc=point_climo_mu, scale=point_climo_scale),
            "b-",
            alpha=0.8,
            label="clim (fitted)",
        )  # clim pdf in blue
        pdf_ax.plot(x1, fpdf, "r-", alpha=0.8, label="fcst")  # fcst PDF in red
        pdf_ax.hist(
            point_climo, density=True, histtype="step", label="clim (empirical)"
        )  # want this in GREEN

        pdf_ax.axvline(x=point_threshold, color="k", linestyle="--")
        pdf_ax.legend(loc="best", frameon=False)
        pdf_ax.set_title(f"Point Probability Density Functions {lead_name}")
        pdf_ax.set_xlabel(Y.name.upper())
        pdf_ax.set_ylabel("")

        if is_transformed:
            newticks = [-2, -1, 0, 1, 2]
            pdf_ax.set_xticks(
                newticks,
                [
                    round(
                        i * point_climo_std_nontransformed + point_climo_mu_nontransformed,
                        2,
                    )
                    for i in newticks
                ],
                rotation=0,
            )
            cdf_ax.set_xticks(
                newticks,
                [
                    round(
                        i * point_climo_std_nontransformed + point_climo_mu_nontransformed,
                        2,
                    )
                    for i in newticks
                ],
                rotation=0,
            )

        # save plot
        figName = MOS + "_flexForecast_probExceedence.png"
        plt.savefig(
            files_root / "figures" / figName,
            bbox_inches="tight",
        )
