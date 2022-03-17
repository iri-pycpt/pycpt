
from ..utilities import CPT_GOODNESS_INDICES_R, CPT_DEFAULT_VERSION, CPT_REGRESSIONS_R, CPT_LINKS_R, CPT_TAILORING_R, CPT_OUTPUT_NEW,  CPT_SKILL_R, CPT_TRANSFORMATIONS_R
from ..base import CPT
from pathlib import Path 
import pandas as pd 
import platform, shutil, time, os
from cpttools import open_cptdataset, to_cptv10
from cptlite.checks import check_all, guess_coords 
import xarray as xr 



def multiple_regression(
        X,  # Predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        Y,  # Predictand Dataset in an Xarray DataArray with three dimensions, XYT 
        F=None, # New Out of sample (forecast) predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        crossvalidation_window=5,  # number of samples to leave out in each cross-validation step 
        transform_predictand=None,  # transformation to apply to the predictand dataset - None, 'Empirical', 'Gamma'
        tailoring=None,  # tailoring None, Anomaly, StdAnomaly, or SPI (SPI only available on Gamma)
        cpt_kwargs={}, # a dict of kwargs that will be passed to CPT 
        retroactive_initial_training_period=0.45, # percent of samples to be used as initial training period for retroactive validation
        retroactive_step=0.1, # percent of samples to increment retroactive training period by each time. 
        validation='crossvalidation', #type of leave-n-out crossvalidation to use
        regression='OLS',
        link='identity',
        x_lat_dim=None, 
        x_lon_dim=None, 
        x_sample_dim=None, 
        x_feature_dim=None, 
        y_lat_dim=None, 
        y_lon_dim=None, 
        y_sample_dim=None, 
        y_feature_dim=None, 
        f_lat_dim=None, 
        f_lon_dim=None, 
        f_sample_dim=None, 
        f_feature_dim=None, 
    ):
    assert validation.upper() in ['CROSSVALIDATION', 'RETROACTIVE'], "validation must be one of ['CROSSVALIDATION', 'RETROACTIVE']"
    assert isinstance(crossvalidation_window, int) and crossvalidation_window %2 == 1, "crossvalidation window must be odd integer"
    assert isinstance(retroactive_initial_training_period, float) and  0 < retroactive_initial_training_period < 1, 'retroactive_initial_training_period must be a float between 0 and 1'
    assert isinstance(retroactive_step, float) and  0 < retroactive_initial_training_period < 1, 'retroactive_step must be a float between 0 and 1'
    
    x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim )
    check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
    X = X.squeeze()  # drop all size-one dimensions 

    y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim = guess_coords(Y, y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim )
    check_all(Y, y_lat_dim, y_lon_dim, y_sample_dim, y_feature_dim)
    Y = Y.squeeze() # drop all size-one dimensions 
    retroactive_initial_training_period = int(retroactive_initial_training_period * X.shape[list(X.dims).index(x_sample_dim)])
    retroactive_step = int(retroactive_step * X.shape[list(X.dims).index(x_sample_dim)])

    if F is not None: 
        f_lat_dim, f_lon_dim, f_sample_dim,  f_feature_dim = guess_coords(F, f_lat_dim, f_lon_dim, f_sample_dim,  f_feature_dim )
        check_all(F, f_lat_dim, f_lon_dim, f_sample_dim, f_feature_dim)
        #F = F.squeeze() #drop all size-one dimensions 


    cpt = CPT(**cpt_kwargs)
    cpt.write(614) # activate GCM MOS 
    
    cpt.write(527) # GCM settings
    cpt.write(1) # interpolate? 
    cpt.write(3) # bias and variance corrected 
    cpt.write(3) # bias and variance corrected 

    cpt.write(535) # regression options 
    cpt.write(CPT_REGRESSIONS_R[regression.upper()])
    
    if regression.lower() in ['poisson', 'gamma']:
        cpt.write(CPT_LINKS_R[link.upper()])

    # apply tailoring 
    tailoring = str(tailoring)
    assert  (tailoring.upper() == 'SPI' and transform_predictand.upper()  == 'GAMMA')  or  tailoring != 'SPI', 'Standard Precipitation Index (SPI) tailoring is only available if transform_predictand=Gamma'
    cpt.write(533)
    cpt.write(CPT_TAILORING_R[tailoring.upper()])
    cpt.write(1) # climatologicial probability thresholds 
    cpt.write(0.33) # size of AN category 
    cpt.write(0.33) # size of BN category  

    # set cross validation window
    assert type(crossvalidation_window) == int and crossvalidation_window % 2 == 1 # xval window must be an odd integer 
    cpt.write(534)
    cpt.write(crossvalidation_window)

    # apply transform_predictand 
    if transform_predictand is not None: 
        cpt.write(534) #set transform 
        cpt.write(CPT_TRANSFORMATIONS_R[transform_predictand.upper()])
        cpt.write(541) #activate transform 
    
    # Load X dataset 
    to_cptv10(X.fillna(-999), cpt.outputs['original_predictor'], row=x_lat_dim, col=x_lon_dim, T=x_sample_dim)
    cpt.write(1)
    cpt.write(cpt.outputs['original_predictor'].absolute())
    if len(X.coords) >= 3: # then this is gridded data
        cpt.write( max(X.coords[x_lat_dim].values)) # North
        cpt.write( min(X.coords[x_lat_dim].values)) # South
        cpt.write( min(X.coords[x_lon_dim].values)) # West
        cpt.write( max(X.coords[x_lon_dim].values)) # East 

    # load F dataset if present 
    if F is not None: 
        to_cptv10(F.fillna(-999), cpt.outputs['out_of_sample_predictor'], row=f_lat_dim, col=f_lon_dim, T=f_sample_dim)
        cpt.write(3)
        cpt.write(cpt.outputs['out_of_sample_predictor'].absolute())

    # load Y Dataset 
    to_cptv10(Y.fillna(-999), cpt.outputs['original_predictand'], row=y_lat_dim, col=y_lon_dim, T=y_sample_dim)
    cpt.write(2)
    cpt.write(cpt.outputs['original_predictand'].absolute())
    if len(Y.coords) >= 3: # then this is gridded data
        cpt.write( max(Y.coords[y_lat_dim].values)) # North
        cpt.write( min(Y.coords[y_lat_dim].values)) # South
        cpt.write( min(Y.coords[y_lon_dim].values)) # West
        cpt.write( max(Y.coords[y_lon_dim].values)) # East 

    # set up cpt missing values and goodness index 
    cpt.write(131) # set output fmt to text for goodness index because grads doesnot makes sense
    cpt.write(2)
    # set sigfigs to 6
    cpt.write(132)
    cpt.write(6) 
    cpt.write(531) # Kendalls Tau goodness index 
    cpt.write(3)
    cpt.write(544) # missing value settings 
    cpt.write(-999)
    cpt.write(10)
    cpt.write(10)
    cpt.write(1)
    cpt.write(4 )
    cpt.write(-999)
    cpt.write(10)
    cpt.write(10)
    cpt.write(1)
    cpt.write(4 )

    cpt.write(112) 
    cpt.write(cpt.outputs['goodness_index'].absolute())

    #initiate analysis 
    cpt.write(311)

    # save all deterministic skill scores 
    for skill in ['pearson', 'spearman', '2afc', 'roc_below', 'roc_above']: 
        cpt.write(413)
        cpt.write(CPT_SKILL_R[skill.upper()])
        cpt.write(cpt.outputs[skill].absolute())

    
    cpt.write('111')
    cpt.write('201')
    cpt.write( cpt.outputs['crossvalidated_hindcasts'].absolute())
    cpt.write('0') 

    if F is not None: 
        cpt.write(454) # deterministic forecasts 

        cpt.write(111)
        cpt.write(511)
        cpt.write(cpt.outputs['forecast_values'].absolute())
        cpt.write(0)

        cpt.write(111)
        cpt.write(514)
        cpt.write(cpt.outputs['prediction_error_variance'].absolute())
        cpt.write(0)

        cpt.write(455) # probabilistic forecasts 
        cpt.write(111)
        cpt.write(501)
        cpt.write(cpt.outputs['forecast_probabilities'].absolute())
        cpt.write(0)
    cpt.wait_for_files()

    fcsts = None
    if F is not None: 
        prob_fcst = open_cptdataset(str(cpt.outputs['forecast_probabilities'].absolute()) + '.txt')
        prob_fcst = getattr(prob_fcst, [i for i in prob_fcst.data_vars][0])
        prob_fcst.name = 'probabilistic_forecasts'
        det_fcst = open_cptdataset(str(cpt.outputs['forecast_values'].absolute()) + '.txt')
        det_fcst = getattr(det_fcst, [i for i in det_fcst.data_vars][0])
        det_fcst.name = 'deterministic_forecasts'
        pev = open_cptdataset(str(cpt.outputs['prediction_error_variance'].absolute()) + '.txt')
        pev = getattr(pev, [i for i in pev.data_vars][0])
        pev.name = 'prediction_error_variance'
        fcsts = xr.merge([det_fcst, prob_fcst, pev])

    hcsts = open_cptdataset(str(cpt.outputs['crossvalidated_hindcasts'].absolute()) + '.txt' )
    hcsts = getattr(hcsts, [i for i in hcsts.data_vars][0])
    hcsts.name = 'hindcasts' 
    hcsts = xr.merge([hcsts])
    pearson = open_cptdataset(str(cpt.outputs['pearson'].absolute()) + '.txt')
    pearson = getattr(pearson, [i for i in pearson.data_vars][0])
    pearson.name = 'pearson'
    spearman = open_cptdataset(str(cpt.outputs['spearman'].absolute()) + '.txt')
    spearman = getattr(spearman, [i for i in spearman.data_vars][0])
    spearman.name = 'spearman'
    two_afc = open_cptdataset(str(cpt.outputs['2afc'].absolute()) + '.txt')
    two_afc = getattr(two_afc, [i for i in two_afc.data_vars][0])
    two_afc.name = 'two_alternative_forced_choice'
    roc_below = open_cptdataset(str(cpt.outputs['roc_below'].absolute()) + '.txt') 
    roc_below = getattr(roc_below, [i for i in roc_below.data_vars][0])
    roc_below.name = 'roc_area_under_curve'
    roc_above = open_cptdataset(str(cpt.outputs['roc_above'].absolute()) + '.txt')
    roc_above = getattr(roc_above, [i for i in roc_above.data_vars][0])
    roc_above.name = 'roc_area_above_curve'
    skill_values = [pearson, spearman, two_afc, roc_below, roc_above]
    skill_values = xr.merge(skill_values).mean('Mode')
    return hcsts, fcsts, skill_values

