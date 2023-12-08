import cartopy
import cartopy.crs as ccrs
import cartopy.mpl.gridliner as gridliner
import cptcore as cc
import cptdl as dl
import cptextras as ce
import cptio as cio
import matplotlib.gridspec as gridspec
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from pathlib import Path
from PIL import Image
from scipy.stats import norm, t
import xarray as xr


missing_value_flag = -999


def setup(case_dir, domain):
    # extracting domain boundaries and create house keeping
    e = domain['east']
    w = domain['west']
    n = domain['north']
    s = domain['south']

    domainFolder = str(w) + "W-" + str(e) + "E" + "_to_" + str(s) + "S-" + str(n) + "N"

    files_root = case_dir / domainFolder
    setup_domain_dir(files_root)

    return files_root


def setup_domain_dir(files_root):
    files_root.mkdir(exist_ok=True, parents=True)

    dataDir = files_root / "data"
    dataDir.mkdir(exist_ok=True, parents=True)

    figDir = files_root / "figures"
    figDir.mkdir(exist_ok=True, parents=True)

    outputDir = files_root / "output"
    outputDir.mkdir(exist_ok=True, parents=True)

    print(f"Input data will be saved in {dataDir}")
    print(f"Figures will be saved in {figDir}")
    print(f"Output will be saved in {outputDir}")


def download_data(
        predictand_name, local_predictand_file, predictor_names, download_args, files_root, force_download
):
    forecast_data = download_forecasts(
        predictor_names, files_root, force_download, download_args
    )
    if local_predictand_file is None:
        Y = download_observations(
            download_args, files_root, predictand_name, force_download
        )
    else:
        print('Using local observed predictand dataset')
        if local_predictand_file.endswith('.tsv'):
            Y = cio.open_cptdataset(local_predictand_file)
        elif local_predictand_file.endswith('.nc'):
            Y = xr.open_dataset(local_predictand_file)
        else:
            assert False
        Y = next(iter(Y.data_vars.values()))

    hindcast_data = download_hindcasts(
        predictor_names, files_root, force_download, download_args
    )
    return Y, hindcast_data, forecast_data


def _preprocess_download_args(download_args):
    lead_low, lead_high = dl.leads_from_target(download_args['fdate'], download_args['target'])
    user_lead_low = download_args.get('lead_low')
    user_lead_high = download_args.get('lead_high')
    assert (
            (user_lead_low is None or user_lead_low == lead_low) and
            (user_lead_high is None or user_lead_high == lead_high)
    ), "lead_low and lead_high are not consistent with fdate and target."
    result = dict(
        download_args,
        lead_low=lead_low,
        lead_high=lead_high,
    )
    return result

def download_observations(download_args, files_root, predictand_name, force_download):
    download_args_obs = _preprocess_download_args(download_args)

    dataDir = files_root / "data"
    # Deal with "Cross-year issues" where either the target season
    # crosses Jan 1 (eg DJF), or where the forecast initialization is
    # in the calendar year before the start of the target season (eg
    # JFM from Dec 1 sart)

    fmon = download_args_obs["fdate"].month
    tmon1 = fmon + download_args_obs["lead_low"]  # first month of the target season
    tmon2 = fmon + download_args_obs["lead_high"]  # last month of the target season

    # For when the target season crossing Jan 1 (eg DJF)
    # (i.e., when target season starts in the same calendar year as the forecast init
    # and ends in the following calendar year)
    # Here the final year of the obs dataset needs to be incremented by 1.
    if tmon1 <= 12.5 and tmon2 > 12.5:
        download_args_obs["final_year"] += 1

    # For JFM, FMA .. with forecast initialization in the previous year.
    # (i.e., when target season starts in the calendar year after the forecast init.)
    # Here both the first and final year of the obs dataset need to be incremented by 1.
    if tmon1 > 12.5:
        download_args_obs["first_year"] += 1
        download_args_obs["final_year"] += 1

    url_pattern = dl.observations[predictand_name]

    return cached_download(
        dl.observations[predictand_name], files_root, predictand_name,
        force_download, download_args_obs
    )


def download_hindcasts(predictor_names, files_root, force_download, download_args):
    return [
        cached_download(
            dl.hindcasts[model], files_root, f'{model}',
            force_download, download_args
        )
        for model in predictor_names
    ]


def download_forecasts(predictor_names, files_root, force_download, download_args):
    return [
        cached_download(
            dl.forecasts[model], files_root,
            f'{model}_f{download_args["fdate"].year}',
            force_download, download_args
        )
        for model in predictor_names
    ]


def cached_download(url_pattern, files_root, basename, force_download, download_args):
    dataDir = files_root / "data"
    full_download_args = _preprocess_download_args(download_args)
    tsvfile = dataDir / f'{basename}.tsv'
    ncfile = dataDir / f'{basename}.nc'
    if not ncfile.is_file() or force_download:
        print(f'Downloading {basename}')
        ds = dl.download(
            url_pattern,
            tsvfile,
            **full_download_args,
            verbose=True,
            use_dlauth=False,
        )
        ds.to_netcdf(ncfile)
    else:
        print(f'Reusing already-downloaded {basename}')
        ds = xr.open_dataset(ncfile)

    return getattr(ds, [i for i in ds.data_vars][0])


def evaluate_models(hindcast_data, MOS, Y, forecast_data, cpt_args, domain_dir, predictor_names, interactive=False):
    outputDir = domain_dir / "output"
    hcsts, fcsts, skill, pxs, pys = [], [], [], [], []

    for i, model_hcst in enumerate(hindcast_data):


        if str(MOS).upper() == 'CCA':

            # fit CCA model between X & Y and produce real-time forecasts for F
            cca_h, cca_rtf, cca_s, cca_px, cca_py = cc.canonical_correlation_analysis(model_hcst, Y, \
            F=forecast_data[i] ,**cpt_args, cpt_kwargs={"interactive": interactive} )

    #         fit CCA model again between X & Y, and produce in-sample probabilistic hindcasts
    #         this is using X in place of F, with the year coordinates changed to n+100 years
    #         because CPT does not allow you to make forecasts for in-sample data
            cca_h, cca_f, cca_s, cca_px, cca_py = cc.canonical_correlation_analysis(model_hcst, Y, \
            F=ce.redate(model_hcst, yeardelta=48), **cpt_args)
            cca_h = xr.merge([cca_h, ce.redate(cca_f.probabilistic, yeardelta=-48), ce.redate(cca_f.prediction_error_variance, yeardelta=-48)])

    #         # use the in-sample probabilistic hindcasts to perform probabilistic forecast verification
    #         # warning - this produces unrealistically optimistic values
            cca_pfv = cc.probabilistic_forecast_verification(cca_h.probabilistic, Y, **cpt_args)
            cca_s = xr.merge([cca_s, cca_pfv])

            hcsts.append(cca_h)
            fcsts.append(cca_rtf)
            skill.append(cca_s.where(cca_s > -999, other=np.nan))
            pxs.append(cca_px)
            pys.append(cca_py)

        elif str(MOS).upper() == 'PCR':

            # fit PCR model between X & Y and produce real-time forecasts for F
            pcr_h, pcr_rtf, pcr_s, pcr_px = cc.principal_components_regression(model_hcst, Y, F=forecast_data[i], **cpt_args)

            # fit PCR model again between X & Y, and produce in-sample probabilistic hindcasts
            # this is using X in place of F, with the year coordinates changed to n+100 years
            # because CPT does not allow you to make forecasts for in-sample data
            pcr_h, pcr_f, pcr_s, pcr_px = cc.principal_components_regression(model_hcst, Y, F=ce.redate(model_hcst, yeardelta=48), **cpt_args)
            pcr_h = xr.merge([pcr_h, ce.redate(pcr_f.probabilistic, yeardelta=-48), ce.redate(pcr_f.prediction_error_variance, yeardelta=-48)])

            # use the in-sample probabilistic hindcasts to perform probabilistic forecast verification
            # warning - this produces unrealistically optimistic values
            pcr_pfv = cc.probabilistic_forecast_verification(pcr_h.probabilistic, Y, **cpt_args)
            pcr_s = xr.merge([pcr_s, pcr_pfv])
            hcsts.append(pcr_h)
            fcsts.append(pcr_rtf)
            skill.append(pcr_s.where(pcr_s > -999, other=np.nan))
            pxs.append(pcr_px)
        else:
            # simply compute deterministic skill scores of non-corrected ensemble means
            nomos_skill = cc.deterministic_skill(model_hcst, Y, **cpt_args)
            skill.append(nomos_skill.where(nomos_skill > -999, other=np.nan))

        # choose what data to export here (any of the above results data arrays can be saved to netcdf)
        if str(MOS).upper() == 'CCA':
            cca_h.to_netcdf(outputDir /  (predictor_names[i] + '_crossvalidated_cca_hindcasts.nc'))
            cca_rtf.to_netcdf(outputDir / (predictor_names[i] + '_realtime_cca_forecasts.nc'))
            cca_s.to_netcdf(outputDir / (predictor_names[i] + '_skillscores_cca.nc'))
            cca_px.to_netcdf(outputDir / (predictor_names[i] + '_cca_x_spatial_loadings.nc'))
            cca_py.to_netcdf(outputDir / (predictor_names[i] + '_cca_y_spatial_loadings.nc'))
        elif str(MOS).upper() == 'PCR':
            pcr_h.to_netcdf(outputDir /  (predictor_names[i] + '_crossvalidated_pcr_hindcasts.nc'))
            pcr_rtf.to_netcdf(outputDir / (predictor_names[i] + '_realtime_pcr_forecasts.nc'))
            pcr_s.to_netcdf(outputDir / (predictor_names[i] + '_skillscores_pcr.nc'))
            pcr_px.to_netcdf(outputDir / (predictor_names[i] + '_pcr_x_spatial_loadings.nc'))
        else:
            nomos_skill.to_netcdf(outputDir / (predictor_names[i] + '_nomos_skillscores.nc'))
    return hcsts, fcsts, skill, pxs, pys


SKILL_METRICS = {
    # each entry has the form (colormap, min, max)

    # deterministic
    "pearson": (ce.cmaps["cpt_correlation"], -1, 1),
    "spearman": (ce.cmaps["cpt_correlation"], -1, 1),
    "two_alternative_forced_choice": (ce.cmaps["pycpt_roc"], 0, 100),
    "2afc": (ce.cmaps["pycpt_roc"], 0, 100),
    "roc_area_below_normal": (ce.cmaps["pycpt_roc"], 0, 1),
    "roc_below": (ce.cmaps["pycpt_roc"], 0, 1),
    "roc_area_above_normal": (ce.cmaps["pycpt_roc"], 0, 1),
    "roc_above": (ce.cmaps["pycpt_roc"], 0, 1),

    # probabilistic (in sample):
    "generalized_roc": (ce.cmaps["pycpt_roc"], 0, 100),
    "ignorance": (), # TODO
    "rank_probability_skill_score": (ce.cmaps["cpt_correlation"], -50, 50),
}
SKILL_METRICS["generalized_roc"][0].set_under("lightgray")
SKILL_METRICS["rank_probability_skill_score"][0].set_under("lightgray")

def plot_skill(predictor_names, skill, MOS, files_root, skill_metrics):
    fig, ax = plt.subplots(
        nrows=len(predictor_names),
        ncols=len(skill_metrics),
        subplot_kw={"projection": cartopy.crs.PlateCarree()},
        figsize=(5 * len(skill_metrics), 2.5 * len(predictor_names)),
        squeeze=False,
    )
    for i, model in enumerate(predictor_names):
        for j, skill_metric in enumerate(skill_metrics):
            metric = SKILL_METRICS[skill_metric]
            n = (
                getattr(skill[i], skill_metric)
                .where(getattr(skill[i], skill_metric) > missing_value_flag)
                .plot(ax=ax[i][j], cmap=metric[0], vmin=metric[1], vmax=metric[2])
            )
            ax[i][j].coastlines()
            ax[i][j].add_feature(cartopy.feature.BORDERS)
            ax[0][j].set_title(skill_metric.upper())

        ax[i][0].text(
            -0.07,
            0.55,
            model.upper(),
            va="bottom",
            ha="center",
            rotation="vertical",
            rotation_mode="anchor",
            transform=ax[i][0].transAxes,
        )

    # save plots
    figName = MOS + "_models_skillMatrices.png"
    fig.savefig(
        files_root / "figures" / figName,
        bbox_inches="tight",
    )


def plot_cca_modes(
    MOS, predictor_names, pxs, pys, files_root
):
    nmodes = 3
    cmap = plt.get_cmap("cpt.loadings", 11)
    vmin = -10
    vmax = 10
    missing_value_flag = -999

    if MOS == "CCA":
        for i, model in enumerate(predictor_names):
            for mode in range(nmodes):
                if mode == 0 and model == predictor_names[0]:
                    Vmin, Vmax = ce.standardized_range(
                        float(
                            pxs[i]
                            .x_cca_loadings.isel(Mode=mode)
                            .where(
                                pxs[i].x_cca_loadings.isel(Mode=mode)
                                > missing_value_flag
                            )
                            .min()
                        ),
                        float(
                            pxs[i]
                            .x_cca_loadings.isel(Mode=mode)
                            .where(
                                pxs[i].x_cca_loadings.isel(Mode=mode)
                                > missing_value_flag
                            )
                            .max()
                        ),
                    )

                cancorr = np.correlate(
                    pxs[i].x_cca_scores[:, mode], pys[i].y_cca_scores[:, mode]
                )
                print(
                    model.upper()
                    + ": CCA MODE {}".format(mode + 1)
                    + " - Canonical Correlation = "
                    + str(ce.truncate(cancorr[0], 2))
                )

                fig = plt.figure(figsize=(20, 5))

                gs0 = gridspec.GridSpec(1, 3, figure=fig)
                gs00 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[0])
                gs01 = gridspec.GridSpecFromSubplotSpec(5, 10, subplot_spec=gs0[1])
                gs02 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[2])
                ts = xr.concat(
                    [
                        pxs[i].x_cca_scores.isel(Mode=mode),
                        pys[i].y_cca_scores.isel(Mode=mode),
                    ],
                    "M",
                ).assign_coords({"M": ["x", "y"]})

                map1_ax = fig.add_subplot(gs00[:, :], projection=ccrs.PlateCarree())
                ts_ax = fig.add_subplot(gs01[1:3, 1:])
                map2_ax = fig.add_subplot(gs02[:, :], projection=ccrs.PlateCarree())

                art = (
                    pxs[i]
                    .x_cca_loadings.isel(Mode=mode)
                    .where(pxs[i].x_cca_loadings.isel(Mode=mode) > missing_value_flag)
                    .plot(
                        ax=map1_ax, add_colorbar=False, vmin=Vmin, vmax=Vmax, cmap=cmap
                    )
                )

                graph_orientation = ce.graphorientation(
                    len(pys[0]["X"]),
                    len(pys[0]["Y"])
                )

                cb = plt.colorbar(art, orientation=graph_orientation)
                cb.set_label(label="x_cca_loadings", size=14)
                cb.ax.tick_params(labelsize=12)

                art = (
                    pys[i]
                    .y_cca_loadings.isel(Mode=mode)
                    .where(pys[i].y_cca_loadings.isel(Mode=mode) > missing_value_flag)
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
                figName = MOS + "_" + str(model) + "_CCA_mode_" + str(mode + 1) + ".png"
                fig.savefig(files_root / "figures" / figName, bbox_inches="tight")
    else:
        print("You will need to set MOS=CCA in order to see CCA Modes")


def plot_eof_modes(
    MOS, predictor_names, pxs, pys, files_root
):
    nmodes = 5
    cmap = plt.get_cmap("cpt.loadings", 11)
    vmin = -10
    vmax = 10

    graph_orientation = ce.graphorientation(
        len(pys[0]["X"]),
        len(pys[0]["Y"])
    )

    import matplotlib.ticker as mticker
    import matplotlib.gridspec as gridspec

    if MOS == "CCA":
        for i, model in enumerate(predictor_names):
            for mode in range(nmodes):
                if mode == 0 and model == predictor_names[0]:
                    Vmin, Vmax = ce.standardized_range(
                        float(
                            pxs[i]
                            .x_eof_loadings.isel(Mode=mode)
                            .where(
                                pxs[i].x_eof_loadings.isel(Mode=mode)
                                > missing_value_flag
                            )
                            .min()
                        ),
                        float(
                            pxs[i]
                            .x_eof_loadings.isel(Mode=mode)
                            .where(
                                pxs[i].x_eof_loadings.isel(Mode=mode)
                                > missing_value_flag
                            )
                            .max()
                        ),
                    )

                print(
                    model.upper() + ": EOF {}".format(mode + 1)
                )  # str(truncate(canvar[0], 2)))
                fig = plt.figure(figsize=(20, 5))

                gs0 = gridspec.GridSpec(1, 3, figure=fig)
                gs00 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[0])
                gs01 = gridspec.GridSpecFromSubplotSpec(4, 5, subplot_spec=gs0[1])
                gs02 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[2])
                ts = xr.concat(
                    [
                        pxs[i].x_eof_scores.isel(Mode=mode),
                        pys[i].y_eof_scores.isel(Mode=mode),
                    ],
                    "M",
                ).assign_coords({"M": ["x", "y"]})

                map1_ax = fig.add_subplot(gs00[:, :], projection=ccrs.PlateCarree())
                ts_ax = fig.add_subplot(gs01[1:3, 1:])
                map2_ax = fig.add_subplot(gs02[:, :], projection=ccrs.PlateCarree())

                art = (
                    pxs[i]
                    .x_eof_loadings.isel(Mode=mode)
                    .where(pxs[i].x_eof_loadings.isel(Mode=mode) > missing_value_flag)
                    .plot(
                        ax=map1_ax, add_colorbar=False, vmin=Vmin, vmax=Vmax, cmap=cmap
                    )
                )

                canvarX = round(
                    float(
                        pxs[i]
                        .x_explained_variance.isel(Mode=mode)
                        .where(
                            pxs[i].x_explained_variance.isel(Mode=mode)
                            > missing_value_flag
                        )
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
                pxs[i].x_explained_variance
                art = (
                    pys[i]
                    .y_eof_loadings.isel(Mode=mode)
                    .where(pys[i].y_eof_loadings.isel(Mode=mode) > missing_value_flag)
                    .plot(
                        ax=map2_ax, add_colorbar=False, vmin=Vmin, vmax=Vmax, cmap=cmap
                    )
                )

                canvarY = round(
                    float(
                        pys[i]
                        .y_explained_variance.isel(Mode=mode)
                        .where(
                            pys[i].y_explained_variance.isel(Mode=mode)
                            > missing_value_flag
                        )
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
                ts_ax.set_title("EOF Scores (Mode {})".format(mode + 1))
                ts_ax.set_ylabel(None)
                ts_ax.set_xlabel(None)

                map1_ax.set_title(
                    "X EOF MODE {} = {}%".format(mode + 1, ce.truncate(canvarX, 2))
                )
                map2_ax.set_title(
                    "Y EOF MODE {} = {}%".format(mode + 1, ce.truncate(canvarY, 2))
                )

                map1_ax.coastlines()
                map2_ax.coastlines()
                map1_ax.add_feature(cartopy.feature.BORDERS)
                map2_ax.add_feature(cartopy.feature.BORDERS)
                plt.show()

                # save plots
                figName = MOS + "_" + str(model) + "_EOF_mode_" + str(mode + 1) + ".png"
                fig.savefig(files_root / "figures" / figName, bbox_inches="tight")
    elif MOS == "PCR":
        for i, model in enumerate(predictor_names):
            for mode in range(nmodes):
                print(model.upper() + " - MODE {}".format(mode + 1))
                # print(model.upper() + ': EOF {}'.format(mode+1)  +' = '+ str(truncate(cancorr[0], 2)))
                fig = plt.figure(figsize=(20, 5))
                gs0 = gridspec.GridSpec(1, 3, figure=fig)
                gs00 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[0])
                gs01 = gridspec.GridSpecFromSubplotSpec(5, 10, subplot_spec=gs0[1])
                gs02 = gridspec.GridSpecFromSubplotSpec(3, 3, subplot_spec=gs0[2])
                ts = xr.concat(
                    [pxs[i].x_eof_scores.isel(Mode=mode)], "M"
                ).assign_coords({"M": ["x"]})

                map1_ax = fig.add_subplot(gs00[:, :], projection=ccrs.PlateCarree())
                ts_ax = fig.add_subplot(gs01[1:3, 1:])
                map2_ax = fig.add_subplot(gs02[:, :], projection=ccrs.PlateCarree())

                (
                    pxs[i]
                    .x_eof_loadings.isel(Mode=mode)
                    .where(pxs[i].x_eof_loadings.isel(Mode=mode) > missing_value_flag)
                    .plot(ax=map1_ax, cmap=cmap)
                )

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
                ts_ax.set_title("EOF Scores (Mode {})".format(mode + 1))
                ts_ax.set_ylabel(None)
                ts_ax.set_xlabel(None)

                map1_ax.set_title("X EOF MODE {}".format(mode + 1))
                # map2_ax.set_title('Y EOF MODE {}'.format(mode+1))

                map1_ax.coastlines()
                map1_ax.add_feature(cartopy.feature.BORDERS)
                # map2_ax.coastlines()
                plt.show()

                # save plots
                figName = MOS + "_" + str(model) + "_EOF_mode_" + str(mode + 1) + ".png"
                fig.savefig(files_root / "figures" / figName, bbox_inches="tight")
    else:
        print("You will need to set MOS=CCA in order to see CCA Modes")


def plot_forecasts(
    cpt_args,
    predictand_name,
    fcsts,
    files_root,
    predictor_names,
    MOS,
):
    prob_missing_value_flag = -1
    my_dpi = 100

    graph_orientation = ce.graphorientation(
        len(fcsts[0]["X"]),
        len(fcsts[0]["Y"])
    )

    ForTitle, vmin, vmax, barcolor = ce.prepare_canvas(
        cpt_args["tailoring"], predictand_name
    )
    cmapB, cmapN, cmapA = ce.prepare_canvas(None, predictand_name, "probabilistic")

    from mpl_toolkits.axes_grid1 import make_axes_locatable

    iidx = 1
    for i in range(len(fcsts)):
        if graph_orientation == "horizontal":
            fig = plt.figure(figsize=(18, 10), facecolor="w", dpi=my_dpi)
        else:
            fig = plt.figure(figsize=(15, 12), facecolor="w", dpi=my_dpi)

        matplotlibInstance, cartopyInstance = ce.view_probabilistic(
            fcsts[i]
            .probabilistic.where(fcsts[i].probabilistic > prob_missing_value_flag)
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
        # cartopyInstance.axis("off")
        allaxes = matplotlibInstance.get_axes()

        cartopyInstance.spines["left"].set_color("blue")

        matplotlibInstance.savefig(
            files_root / "figures" / "Test.png",
            bbox_inches="tight",
        )  # ,pad_inches = 0)

        matplotlibInstance.clf()
        cartopyInstance.cla()

        ax1 = fig.add_subplot(2, 2, 1)
        ax1.set_axis_off()
        ax1.set_title(
            predictor_names[i].upper() + " - Probabilistic Forecast "
        )
        pil_img = Image.open(
            files_root / "figures" / "Test.png"
        )
        ax1.set_xticks([])
        ax1.set_yticks([])
        ax1.imshow(pil_img)

        iidx = iidx + 1

        datart = (
            fcsts[i]
            .deterministic.where(fcsts[i].deterministic > missing_value_flag)
            .isel(T=-1)
        )
        if (
            any(x in predictand_name for x in ["TMAX", "TMIN", "TMEAN", "TMED"])
            and i == 0
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

        ax2.set_title(
            predictor_names[i].upper() + " - Deterministic Forecast " + ForTitle
        )
        pil_img = Image.open(
            files_root / "figures" / "Test.png"
        )
        ax2.set_xticks([])
        ax2.set_yticks([])
        ax2.imshow(pil_img)  # , aspect=4 1.45 , extent=[0, 1.45, 1.5, 0],

        iidx = iidx + 1

        # save plots
        figName = (
            MOS
            + ForTitle.replace(" ", "_")
            + "["
            + ",".join(predictor_names)
            + "]"
            + "[determinstic-probabilistic]-Forecast"
            + ".png"
        )
        fig.savefig(
            files_root / "figures" / figName,
            bbox_inches="tight",
        )
        plt.close()


def plot_mme_skill(
        predictor_names, nextgen_skill, MOS, files_root, skill_metrics
):
    graph_orientation = ce.graphorientation(
        len(nextgen_skill["X"]),
        len(nextgen_skill["Y"])
    )

    fig, ax = plt.subplots(
        nrows=1,
        ncols=len(skill_metrics),
        subplot_kw={"projection": ccrs.PlateCarree()},
        figsize=(5 * len(skill_metrics), 1 * len(predictor_names)),
        squeeze=False,
    )

    for i in [0]:
        for j, skill_metric in enumerate(skill_metrics):
            metric = SKILL_METRICS[skill_metric]
            ax[i][j].set_title(skill_metric)
            n = (
                getattr(nextgen_skill, skill_metric)
                .where(getattr(nextgen_skill, skill_metric) > missing_value_flag)
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

    # fig = plt.figure( figsize=(9*len(fcsts), 5*len(fcsts)), dpi=my_dpi)
    # fig = plt.figure( figsize=(18, 10), dpi=my_dpi)
    if graph_orientation == "horizontal":
        fig = plt.figure(figsize=(18, 10), dpi=my_dpi)
    else:
        fig = plt.figure(figsize=(15, 12), dpi=my_dpi)

    ForTitle, vmin, vmax, barcolor = ce.prepare_canvas(
        cpt_args["tailoring"], predictand_name
    )
    cmapB, cmapN, cmapA = ce.prepare_canvas(None, predictand_name, "probabilistic")

    matplotlibInstance, cartopyInstance = ce.view_probabilistic(
        pr_fcst.where(pr_fcst > prob_missing_value_flag).rename({"C": "M"}).isel(T=-1)
        / 100,
        cmap_an=cmapA,
        cmap_bn=cmapB,
        cmap_nn=cmapN,
        orientation=graph_orientation,
    )
    cartopyInstance.add_feature(cartopy.feature.BORDERS)
    cartopyInstance.set_title("")
    # cartopyInstance.axis("off")

    figName = MOS + "_ensemble_probabilistic-deterministicForecast.png"
    plt.savefig(
        files_root / "figures" / "Test.png",
        bbox_inches="tight",
    )

    matplotlibInstance.clf()
    cartopyInstance.cla()

    ax1 = fig.add_subplot(2, 2, 1)
    ax1.set_axis_off()
    ax1.set_title(MOS + "_ensemble" + " - Probabilistic Forecast")
    pil_img = Image.open(
        files_root / "figures" / "Test.png"
    )
    ax1.set_xticks([])
    ax1.set_yticks([])
    ax1.imshow(pil_img)

    datart = det_fcst.where(det_fcst > missing_value_flag).isel(T=-1)
    if any(x in predictand_name for x in ["TMAX", "TMIN", "TMEAN", "TMED"]):
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

    ax2 = fig.add_subplot(2, 2, 2)
    ax2.set_axis_off()
    ax2.set_title(MOS + "_ensemble" + " - Deterministic Forecast " + ForTitle)
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


def plot_mme_flex_forecasts(
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

    # plot exceedance probability map

    ForTitle, vmin, vmax, mark, barcolor = ce.prepare_canvas('POE',predictand_name,user_color=color_bar)

    # setting up canvas on which to draw

    if graph_orientation == "horizontal":
        fig = plt.figure(figsize=(15, 10))
    else:
        fig = plt.figure(figsize=(10, 20))

    # fig = plt.figure()
    gs0 = gridspec.GridSpec(4, 1, figure=fig)
    gs00 = gridspec.GridSpecFromSubplotSpec(5, 5, subplot_spec=gs0[:3])
    gs11 = gridspec.GridSpecFromSubplotSpec(1, 2, subplot_spec=gs0[3])
    gs01 = gridspec.GridSpecFromSubplotSpec(5, 5, subplot_spec=gs11[0])
    gs02 = gridspec.GridSpecFromSubplotSpec(5, 5, subplot_spec=gs11[1])

    map_ax = fig.add_subplot(gs00[:, :], projection=ccrs.PlateCarree(), aspect="auto")
    cdf_ax = fig.add_subplot(gs01[:, :], aspect="auto")
    pdf_ax = fig.add_subplot(gs02[:, :], aspect="auto")

    # plot the map
    art = exceedance_prob.transpose("Y", "X", ...).plot(
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

    title = map_ax.set_title("(a) Probabilities of Exceedance")

    # point calculations - select the nearest point to the lat/lon the user wanted to plot curves
    point_threshold = float(
        threshold.sel(
            **{"X": point_longitude, "Y": point_latitude}, method="nearest"
        ).values
    )
    point_fcst_scale = float(
        fcst_scale.sel(
            **{"X": point_longitude, "Y": point_latitude}, method="nearest"
        ).values
    )
    point_climo_scale = float(
        climo_scale.sel(
            **{"X": point_longitude, "Y": point_latitude}, method="nearest"
        ).values
    )
    point_fcst_mu = float(
        fcst_mu.sel(
            **{"X": point_longitude, "Y": point_latitude}, method="nearest"
        ).values
    )
    point_climo_mu = float(
        climo_mu.sel(
            **{"X": point_longitude, "Y": point_latitude}, method="nearest"
        ).values
    )
    point_climo = np.squeeze(
        Y2.sel(**{"X": point_longitude, "Y": point_latitude}, method="nearest").values
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
        t.sf(point_threshold, ntrain, loc=point_fcst_mu, scale=point_fcst_scale), 2
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
        t.sf(x1, ntrain, loc=point_fcst_mu, scale=point_fcst_scale),
        "r-",
        lw=1,
        alpha=0.8,
        label="fcst",
    )
    cdf_ax.plot(
        x1,
        norm.sf(x1, loc=point_climo_mu, scale=point_fcst_scale),
        "b-",
        lw=1,
        alpha=0.8,
        label="clim (fitted)",
    )

    cdf_ax.plot(point_threshold, fprobth, "ok")
    cdf_ax.plot(point_threshold, cprobth, "ok")
    cdf_ax.axvline(x=point_threshold, color="k", linestyle="--")
    cdf_ax.set_title(" (b) Point Probabilities of Exceedance")
    cdf_ax.set_xlabel(Y.name.upper())
    cdf_ax.set_ylabel("Probability (%)")
    cdf_ax.legend(loc="best", frameon=False)

    # PDF plot
    # fpdf=t.pdf(x1, ntrain, loc=point_fcst_mu, scale=np.sqrt(point_fcst_scale))
    fpdf = t.pdf(x1, ntrain, loc=point_fcst_mu, scale=point_fcst_scale)

    pdf_ax.plot(
        x1,
        norm.pdf(x1, loc=point_climo_mu, scale=point_climo_scale),
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
    pdf_ax.set_title("(c) Point Probability Density Functions")
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


# Like construct_mme but also returns hindcasts. This will be renamed
# construct_mme in 3.0.
def construct_mme_new(fcsts, hcsts, Y, ensemble, predictor_names, cpt_args, domain_dir):
    outputDir = domain_dir / "output"
    det_fcst = []
    det_hcst = []
    pr_fcst = []
    pr_hcst = []
    pev_fcst = []
    pev_hcst = []
    for model in ensemble:
        assert model in predictor_names, "all members of the nextgen ensemble must be in predictor_names - {} is not".format(model)
        ndx = predictor_names.index(model)

        det_fcst.append(fcsts[ndx].deterministic)
        det_hcst.append(hcsts[ndx].deterministic)
        pr_fcst.append(fcsts[ndx].probabilistic)
        pr_hcst.append(hcsts[ndx].probabilistic)
        pev_fcst.append(fcsts[ndx].prediction_error_variance)
        pev_hcst.append(hcsts[ndx].prediction_error_variance)

    det_fcst = xr.concat(det_fcst, 'model').mean('model')
    det_hcst = xr.concat(det_hcst, 'model').mean('model')
    pr_fcst = xr.concat(pr_fcst, 'model').mean('model')
    pr_hcst = xr.concat(pr_hcst, 'model').mean('model')
    pev_fcst = xr.concat(pev_fcst, 'model').mean('model')
    pev_hcst = xr.concat(pev_hcst, 'model').mean('model')

    det_hcst.attrs['missing'] = hcsts[0].attrs['missing']
    det_hcst.attrs['units'] = hcsts[0].attrs['units']

    pr_hcst.attrs['missing'] = hcsts[0].attrs['missing']
    pr_hcst.attrs['units'] = hcsts[0].attrs['units']

    nextgen_skill_deterministic = cc.deterministic_skill(det_hcst, Y, **cpt_args)
    nextgen_skill_probabilistic = cc.probabilistic_forecast_verification(pr_hcst, Y, **cpt_args)
    nextgen_skill = xr.merge([nextgen_skill_deterministic, nextgen_skill_probabilistic])

    # write out files to outputs directory (NB: generic filenaming neeeds improving)
    assert len(det_fcst['S']) == 1
    year = pd.Timestamp(det_fcst['S'].values[0]).year
    det_fcst.to_netcdf(outputDir / (f'MME_deterministic_forecast_{year}.nc'))
    det_hcst.to_netcdf(outputDir / ('MME_deterministic_hindcasts.nc'))
    pev_fcst.to_netcdf(outputDir / (f'MME_forecast_prediction_error_variance_{year}.nc'))
    pev_hcst.to_netcdf(outputDir / ('MME_hindcast_prediction_error_variance.nc'))
    pr_fcst.to_netcdf(outputDir / (f'MME_probabilistic_forecast_{year}.nc'))
    pr_hcst.to_netcdf(outputDir / ('MME_probabilistic_hindcasts.nc'))
    nextgen_skill.to_netcdf(outputDir / ('MME_skill_scores.nc'))

    return det_hcst, pev_hcst, det_fcst, pr_fcst, pev_fcst, nextgen_skill


# Backwards-compatible version of construct_mme_new, to avoid breaking existing
# notebooks.
def construct_mme(*args):
    det_hcst, pev_hcst, det_fcst, pr_fcst, pev_fcst, nextgen_skill = construct_mme_new(*args)
    return det_fcst, pr_fcst, pev_fcst, nextgen_skill


def construct_flex_fcst(MOS, cpt_args, det_fcst, threshold, isPercentile, Y, pev_fcst):
    # Define transformer based on transform_predictand setting
    if MOS =='CCA':
        if str(cpt_args['transform_predictand']).upper() == 'GAMMA':
            transformer = ce.GammaTransformer()
        elif str(cpt_args['transform_predictand']).upper() == 'EMPIRICAL':
            transformer = ce.EmpiricalTransformer()
        else:
            transformer = None
    elif MOS == 'PCR':
        if str(cpt_args['transform_predictand']).upper() == 'GAMMA':
            transformer = ce.GammaTransformer()
        elif str(cpt_args['transform_predictand']).upper() == 'EMPIRICAL':
            transformer = ce.EmpiricalTransformer()
        else:
            transformer = None
    else:
        print('FLEX FORECASTS NOT POSSIBLE WITHOUT MOS')
        return

    # if the transformer is not none, then we used a y-transform in cpt
    # therefore we have received a prediction error variance file in "units" of (standard normal deviates)^2
    # and need to transform the forecast mean, in order to calculate probability of exceedance

    if transformer is not None:
        # we need to normalize the forecast mean here, using the same method as CPT
        transformer.fit(Y.expand_dims({'M':[0]}))
        fcst_mu = transformer.transform(det_fcst.expand_dims({'M':[0]}))
    else:
        fcst_mu = det_fcst

    if isPercentile:
        if transformer is None:
            # if the user provided a percentile theshold, rather than an actual value
            # and also used no transformation / normalization,
            # then we also need to compute the theshold as an actual value
            threshold = Y.quantile(threshold, dim='T').drop('quantile')
        else:
            # if the user used a transformation and gave a percentile threshold,
            # we we can set the threshold using the cumulative distribution function
            # for the normal distribution N(0, 1)- since thats what the Y data has
            # been transformed to
            threshold = xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) * norm.cdf(threshold)
    else:
        if transformer is None:
            # if the user did not use a transform, and also did not use a percentile for a threshold,
            # we can just use the value directly. but it must be expanded to a 2D datatype
            threshold = xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) * threshold
        else:
            # if the user used a transformation, but gave a full value and NOT a percentile,
            # we must use the transformation that CPT used to transform the threshold onto
            # the normal distribution at N(0, 1)
            threshold = xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) * threshold
            threshold = transformer.transform(threshold)

    def _xr_tsf(thrs, loc1, scale1, dof1=1):
        return t.sf(thrs, dof1, loc=loc1, scale=scale1)

    ntrain = Y.shape[list(Y.dims).index('T')]
    fcst_scale = np.sqrt( (ntrain -2)/ntrain * pev_fcst )

    # if we transformed the forecast data, we should transform the actual Y data to match
    if transformer is not None:
        Y2 = transformer.transform(Y.expand_dims({'M':[0]})).fillna(Y.min('T')) * xr.ones_like(Y.mean('T')).where(~np.isnan(Y.mean('T')), other=np.nan)
        Y2_fill = xr.where(~np.isfinite(Y2), 0, Y2)
        Y2 = xr.where(np.isfinite(Y2), Y2, Y2_fill)
    else:
        Y2 = Y
    # here we calculate the climatological mean and variance
    climo_var =  Y2.var('T') # xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) if transformer is not None else
    climo_mu =  Y2.mean('T') # xr.ones_like(fcst_mu).where(~np.isnan(fcst_mu), other=np.nan) if transformer is not None else
    climo_scale = np.sqrt( (ntrain -2)/ntrain * climo_var )

    tailoring = cpt_args['tailoring']
    if tailoring == None:
        adjusted_fcst_mu = fcst_mu
    elif tailoring == 'Anomaly':
        adjusted_fcst_mu = fcst_mu + climo_mu
    else:
        raise Exception(f'tailoring {tailoring} not yet supported')
    # we calculate here, the probability of exceedance by taking 1 - t.cdf()
    # after having transformed the forecast mean to match the units of the
    # prediction error variance, if necessary.
    exceedance_prob = xr.apply_ufunc( _xr_tsf, threshold, adjusted_fcst_mu, fcst_scale, input_core_dims=[['X', 'Y'], ['X', 'Y'], ['X', 'Y']], output_core_dims=[['X', 'Y']],keep_attrs=True, kwargs={'dof1':ntrain})

    return exceedance_prob, fcst_scale, climo_scale, adjusted_fcst_mu, climo_mu, Y2, ntrain, threshold

def plot_domains(predictor_extent, predictand_extent):
        #Create a feature for States/Admin 1 regions at 1:10m from Natural Earth
        states_provinces = cartopy.feature.NaturalEarthFeature(
                category='cultural',
                name='admin_1_states_provinces',
                scale='10m',
                facecolor='none')

        fig = plt.subplots(figsize=(5,5), subplot_kw=dict(projection=ccrs.PlateCarree()))
        titles = ['Predictor', 'Predictand']
        extents = [predictor_extent, predictand_extent]
        for i in range(2):
            title = titles[i]
            e = extents[i]
            ax = plt.subplot(1, 2, i+1, projection=ccrs.PlateCarree())
            ax.set_extent([e['west'], e['east'], e['north'], e['south']], ccrs.PlateCarree())

            # Put a background image on for nice sea rendering.
            ax.stock_img()

            ax.add_feature(cartopy.feature.LAND)
            ax.add_feature(cartopy.feature.COASTLINE)
            ax.set_title(f"{title} domain")
            pl=ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                              linewidth=2, color='gray', alpha=0.5, linestyle='--')
            pl.xlabels_top = False
            pl.ylabels_left = False
            pl.xformatter = cartopy.mpl.gridliner.LONGITUDE_FORMATTER
            pl.yformatter = cartopy.mpl.gridliner.LATITUDE_FORMATTER
            ax.add_feature(states_provinces, edgecolor='gray')
        plt.show()
