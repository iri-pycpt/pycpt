
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
    'pct_variance': Path.home()/ '.pycpt_workspace' / 'pct_variance',
    'variance_ratio': Path.home() / '.pycpt_workspace' / 'variance_ratio', 
    'mean_bias': Path.home() / '.pycpt_workspace' / 'mean_bias', 
    'root_mean_squared_error': Path.home() / '.pycpt_workspace' / 'root_mean_squared_error', 
    'mean_absolute_error': Path.home() / '.pycpt_workspace' / 'mean_absolute_error', 
    'hit_score': Path.home() / '.pycpt_workspace' / 'hit_score', 
    'hit_skill_score': Path.home() / '.pycpt_workspace' / 'hit_skill_score', 
    'leps': Path.home() / '.pycpt_workspace' / 'leps', 
    'gerrity_score': Path.home() / '.pycpt_workspace' / 'gerrity_score', 
    '2afc_fcst_categories': Path.home() / '.pycpt_workspace' / '2afc_fcst_categories', 
    '2afc_continuous_fcsts': Path.home() / '.pycpt_workspace' / '2afc_continuous_fcsts', 
    'roc_below': Path.home() / '.pycpt_workspace' / 'roc_below', 
    'roc_above': Path.home() / '.pycpt_workspace' / 'roc_above', 
    'generalized_roc': Path.home() / '.pycpt_workspace' / 'generalized_roc', 
    'rank_probability_skill_score': Path.home() / '.pycpt_workspace' / 'rank_probability_skill_score', 
    'ignorance': Path.home() / '.pycpt_workspace' / 'ignorance', 
    'pearson': Path.home() / '.pycpt_workspace' / 'pearson', 
}


def deterministic_skill(
        X,  # Predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        Y,  # Predictand Dataset in an Xarray DataArray with three dimensions, XYT 
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
    ):
    x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_coords(X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim )
    check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
    X = X.squeeze()  # drop all size-one dimensions 

    y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim = guess_coords(Y, y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim )
    check_all(Y, y_lat_dim, y_lon_dim, y_sample_dim, y_feature_dim)
    Y = Y.squeeze() # drop all size-one dimensions 

  
        
    default_output_files.update(output_files)
    output_files = default_output_files

    cpt = CPT(**cpt_kwargs)
    cpt.write(614) # activate GCM Validation MOS 
    cpt.write(527) # GCM Options 
    cpt.write(1) # interpolate ? 
    cpt.write(1) # No correction 
    cpt.write(1) # uncalibrated ensemble average 
    
    # Load X dataset 
    to_cptv10(X.fillna(-999), output_files['original_predictor'], row=x_lat_dim, col=x_lon_dim, T=x_sample_dim)
    cpt.write(1)
    cpt.write(output_files['original_predictor'].absolute())
    if len(X.coords) >= 3: # then this is gridded data
        cpt.write( max(X.coords[x_lat_dim].values)) # North
        cpt.write( min(X.coords[x_lat_dim].values)) # South
        cpt.write( min(X.coords[x_lon_dim].values)) # West
        cpt.write( max(X.coords[x_lon_dim].values)) # East 


    # load Y Dataset 
    to_cptv10(Y.fillna(-999), output_files['original_predictand'], row=y_lat_dim, col=y_lon_dim, T=y_sample_dim)
    cpt.write(2)
    cpt.write(output_files['original_predictand'].absolute())
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
    for skill in ['pearson', 'spearman', '2afc', 'pct_variance', 'variance_ratio', 'mean_bias', 'root_mean_squared_error', 'mean_absolute_error', 'hit_score', 'hit_skill_score', 'leps', 'gerrity_score', '2afc_fcst_categories', '2afc_continuous_fcsts', 'roc_below', 'roc_above']: 
        cpt.write(413)
        cpt.write(CPT_SKILL_R[skill.upper()])
        cpt.write(output_files[skill].absolute())

    skill_values = [open_cptdataset(str(output_files[i].absolute()) + '.txt') for i in ['pearson', 'spearman', '2afc', 'pct_variance', 'variance_ratio', 'mean_bias', 'root_mean_squared_error', 'mean_absolute_error', 'hit_score', 'hit_skill_score', 'leps', 'gerrity_score', '2afc_fcst_categories', '2afc_continuous_fcsts', 'roc_below', 'roc_above'] ]
    skill_values = xr.merge(skill_values)
    return  skill_values

