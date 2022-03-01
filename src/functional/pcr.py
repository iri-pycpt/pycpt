
from ..utilities import CPT_GOODNESS_INDICES_R, CPT_DEFAULT_VERSION, CPT_TAILORING_R, CPT_OUTPUT_NEW,  CPT_SKILL_R, CPT_TRANSFORMATIONS_R
from ..base import CPT
from pathlib import Path 
import pandas as pd 
import platform, shutil, time, os
from cpttools import open_cptdataset, to_cptv10
from cptlite.checks import check_all, guess_coords 
import xarray as xr 

default_output_files = {
    'original_predictor': Path.home() / '.pycpt_workspace' / 'original_predictor',
    'out_of_sample_predictor': Path.home() / '.pycpt_workspace' / 'original_forecast_predictor',
    'original_predictand': Path.home() / '.pycpt_workspace' / 'original_predictand',
    'goodness_index': Path.home() / '.pycpt_workspace' / 'goodness_index',
    'cca_x_timeseries': Path.home() / '.pycpt_workspace' / 'predictor_cca_timeseries',
    'cca_y_timeseries': Path.home() / '.pycpt_workspace' / 'predictand_cca_timeseries',
    'cca_canonical_correlation':  Path.home() / '.pycpt_workspace' / 'cca_canonical_correlation',
    'eof_x_timeseries': Path.home() / '.pycpt_workspace' / 'predictor_eof_timeseries',
    'eof_y_timeseries':  Path.home() / '.pycpt_workspace' / 'predictand_eof_timeseries',
    'eof_x_loadings': Path.home() / '.pycpt_workspace' / 'predictor_eof_spatial_loadings',
    'eof_y_loadings': Path.home() / '.pycpt_workspace' / 'predictand_eof_spatial_loadings',
    'cca_x_loadings': Path.home() / '.pycpt_workspace' / 'predictor_cca_spatial_loadings',
    'cca_y_loadings': Path.home() / '.pycpt_workspace' / 'predictand_cca_spatial_loadings',
    'forecast_probabilities': Path.home() / '.pycpt_workspace' / 'probabilistic_forecasts',
    'forecast_values': Path.home() / '.pycpt_workspace' / 'deterministic_forecasts',
    'crossvalidated_hindcasts': Path.home() / '.pycpt_workspace' / 'crossvalidated_hindcasts',
    'prediction_error_variance': Path.home() / '.pycpt_workspace' / 'prediction_error_variance',
    'probabilistic_reforecasts': Path.home() / '.pycpt_workspace' / 'probabilistic_reforecasts',
    'pearson': Path.home() / '.pycpt_workspace' / 'pearson', 
    'spearman': Path.home() / '.pycpt_workspace' / 'spearman', 
    '2afc': Path.home() / '.pycpt_workspace' / '2afc', 
    'roc_below': Path.home() / '.pycpt_workspace' / 'roc_below', 
    'roc_above': Path.home() / '.pycpt_workspace' / 'roc_above', 
    'generalized_roc': Path.home() / '.pycpt_workspace' / 'generalized_roc', 
    'rank_probability_skill_score': Path.home() / '.pycpt_workspace' / 'rank_probability_skill_score', 
    'ignorance': Path.home() / '.pycpt_workspace' / 'ignorance', 
    'pearson': Path.home() / '.pycpt_workspace' / 'pearson', 
}


def principal_components_regression(
        X,  # Predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        Y,  # Predictand Dataset in an Xarray DataArray with three dimensions, XYT 
        F=None, # New Out of sample (forecast) predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        crossvalidation_window=5,  # number of samples to leave out in each cross-validation step 
        transform_predictand=None,  # transformation to apply to the predictand dataset - None, 'Empirical', 'Gamma'
        tailoring=None,  # tailoring None, Anomaly, StdAnomaly, or SPI (SPI only available on Gamma)
        x_eof_modes=(1,5), # minimum and maximum of allowed X Principal Componenets 
        output_files={}, # a dictionary specifying where outputs should go - default filenames will be updated by this 
        cpt_kwargs={}, # a dict of kwargs that will be passed to CPT 
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
    x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim )
    check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
    X = X.squeeze()  # drop all size-one dimensions 

    y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim = guess_coords(Y, y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim )
    check_all(Y, y_lat_dim, y_lon_dim, y_sample_dim, y_feature_dim)
    Y = Y.squeeze() # drop all size-one dimensions 

    if F is not None: 
        f_lat_dim, f_lon_dim, f_sample_dim,  f_feature_dim = guess_coords(F, f_lat_dim, f_lon_dim, f_sample_dim,  f_feature_dim )
        check_all(F, f_lat_dim, f_lon_dim, f_sample_dim, f_feature_dim)
        #F = F.squeeze() #drop all size-one dimensions 
        
    default_output_files.update(output_files)
    output_files = default_output_files

    cpt = CPT(**cpt_kwargs)
    cpt.write(612) # activate CCA MOS 
    
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
    to_cptv10(X.fillna(-999), output_files['original_predictor'], row=x_lat_dim, col=x_lon_dim, T=x_sample_dim)
    cpt.write(1)
    cpt.write(output_files['original_predictor'].absolute())
    x_first_year, x_final_year = pd.Timestamp(min(X.coords[x_sample_dim].values)).year, pd.Timestamp(max(X.coords[x_sample_dim].values)).year
    if len(X.coords) >= 3: # then this is gridded data
        cpt.write( max(X.coords[x_lat_dim].values)) # North
        cpt.write( min(X.coords[x_lat_dim].values)) # South
        cpt.write( min(X.coords[x_lon_dim].values)) # West
        cpt.write( max(X.coords[x_lon_dim].values)) # East 
    cpt.write(x_eof_modes[0])
    cpt.write(x_eof_modes[1])

    # load F dataset if present 
    if F is not None: 
        to_cptv10(F.fillna(-999), output_files['out_of_sample_predictor'], row=f_lat_dim, col=f_lon_dim, T=f_sample_dim)
        cpt.write(3)
        cpt.write(output_files['out_of_sample_predictor'].absolute())

    # load Y Dataset 
    to_cptv10(Y.fillna(-999), output_files['original_predictand'], row=y_lat_dim, col=y_lon_dim, T=y_sample_dim)
    cpt.write(2)
    cpt.write(output_files['original_predictand'].absolute())
    y_first_year, y_final_year = pd.Timestamp(min(X.coords[x_sample_dim].values)).year, pd.Timestamp(max(X.coords[x_sample_dim].values)).year
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
    cpt.write(output_files['goodness_index'].absolute())

    #initiate analysis 
    cpt.write(311)

    # save all deterministic skill scores 
    for skill in ['pearson', 'spearman', '2afc', 'roc_below', 'roc_above']: 
        cpt.write(413)
        cpt.write(CPT_SKILL_R[skill.upper()])
        cpt.write(output_files[skill].absolute())

    
    cpt.write('111')
    cpt.write('201')
    cpt.write( output_files['crossvalidated_hindcasts'].absolute())
    cpt.write('0') 

    if F is not None: 
        cpt.write(454) # deterministic forecasts 

        cpt.write(111)
        cpt.write(511)
        cpt.write(output_files['forecast_values'].absolute())
        cpt.write(0)

        cpt.write(111)
        cpt.write(514)
        cpt.write(output_files['prediction_error_variance'].absolute())
        cpt.write(0)

        cpt.write(455) # probabilistic forecasts 
        cpt.write(111)
        cpt.write(501)
        cpt.write(output_files['forecast_probabilities'].absolute())
        cpt.write(0)


    for data in [ 'eof_x_timeseries', 'eof_x_loadings' ]:
        cpt.write('111')
        cpt.write(CPT_OUTPUT_NEW[data])
        cpt.write(output_files[data].absolute())
        cpt.write(0)

    #cpt.kill()

    det_fcst, prob_fcst = None, None 
    if F is not None: 
        prob_fcst = open_cptdataset(str(output_files['forecast_probabilities'].absolute()) + '.txt')
        det_fcst = open_cptdataset(str(output_files['forecast_values'].absolute()) + '.txt')
    hcsts = open_cptdataset(str(output_files['crossvalidated_hindcasts'].absolute()) + '.txt' ) 
    skill_values = [open_cptdataset(str(output_files['pearson'].absolute()) + '.txt'), open_cptdataset(str(output_files['spearman'].absolute()) + '.txt'), open_cptdataset(str(output_files['2afc'].absolute()) + '.txt'), open_cptdataset(str(output_files['roc_below'].absolute()) + '.txt'), open_cptdataset(str(output_files['roc_above'].absolute()) + '.txt') ] 
    skill_values = xr.merge(skill_values)
    pattern_values = [ open_cptdataset(str(output_files['eof_x_timeseries'].absolute()) + '.txt'),  open_cptdataset(str(output_files['eof_x_loadings'].absolute()) + '.txt'), open_cptdataset(str(output_files['prediction_error_variance'].absolute()) + '.txt')]
    pattern_values = xr.merge(pattern_values)
    return hcsts, det_fcst, prob_fcst, skill_values, pattern_values 

