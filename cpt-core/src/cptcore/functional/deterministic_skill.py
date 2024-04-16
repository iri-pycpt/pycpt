
from ..utilities import CPT_GOODNESS_INDICES_R, CPT_DEFAULT_VERSION, CPT_TAILORING_R, CPT_OUTPUT_NEW,  CPT_SKILL_R, CPT_TRANSFORMATIONS_R
from ..base import CPT
from cptio import open_cptdataset, to_cptv10, is_valid_cptv10_xyt
import xarray as xr 


def deterministic_skill(
        X,  # Predictor Dataset in an Xarray DataArray with three coordinates, XYT
        Y,  # Predictand Dataset in an Xarray DataArray with three coordinates, XYT
        synchronous_predictors=False,
        cpt_kwargs={}, # a dict of kwargs that will be passed to CPT 
        **kwargs
    ):
    is_valid_cptv10_xyt(X)
    is_valid_cptv10_xyt(Y)

    X.name = Y.name

    cpt = CPT(**cpt_kwargs)
    cpt.write(614) # activate GCM Validation MOS 
    if synchronous_predictors: 
        cpt.write(545)

    cpt.write(544) # missing value settings 
    cpt.write(X.attrs['missing'])
    cpt.write(10)
    cpt.write(10)
    cpt.write(1)
    cpt.write(4 )
    cpt.write(Y.attrs['missing'])
    cpt.write(10)
    cpt.write(10)
    cpt.write(1)
    cpt.write(4 )

    cpt.write(527) # GCM Options 
    cpt.write(1) # interpolate ? 
    cpt.write(1) # No correction 
    cpt.write(1) # uncalibrated ensemble average 
    
    # Load X dataset 
    to_cptv10(X, cpt.outputs['original_predictor'])
    cpt.write(1)
    cpt.write(cpt.outputs['original_predictor'].absolute())
    if 'X' in X.coords:
        assert 'Y' in X.coords
        cpt.write( max(X.coords['Y'].values)) # North
        cpt.write( min(X.coords['Y'].values)) # South
        cpt.write( min(X.coords['X'].values)) # West
        cpt.write( max(X.coords['X'].values)) # East 


    # load Y Dataset 
    to_cptv10(Y, cpt.outputs['original_predictand'])
    cpt.write(2)
    cpt.write(cpt.outputs['original_predictand'].absolute())
    if 'X' in Y.coords:
        assert 'Y' in Y.coords
        cpt.write( max(Y.coords['Y'].values)) # North
        cpt.write( min(Y.coords['Y'].values)) # South
        cpt.write( min(Y.coords['X'].values)) # West
        cpt.write( max(Y.coords['X'].values)) # East 

    # set up cpt missing values and goodness index 
    cpt.write(131) # set output fmt to text for goodness index because grads doesnot makes sense
    cpt.write(2)
    # set sigfigs to 6
    cpt.write(132)
    cpt.write(6) 
    cpt.write(531) # Kendalls Tau goodness index 
    cpt.write(3)
    cpt.write(544) # missing value settings 
    cpt.write(X.attrs['missing'])
    cpt.write(10)
    cpt.write(10)
    cpt.write(1)
    cpt.write(4 )
    cpt.write(Y.attrs['missing'])
    cpt.write(10)
    cpt.write(10)
    cpt.write(1)
    cpt.write(4 )

    cpt.write(112) 
    cpt.write(cpt.outputs['goodness_index'].absolute())

    #initiate analysis 
    cpt.write(311)

    # save all deterministic skill scores 
    for skill in ['pearson', 'spearman', '2afc', 'pct_variance', 'variance_ratio', 'mean_bias', 'root_mean_squared_error', 'mean_absolute_error', 'hit_score', 'hit_skill_score', 'leps', 'gerrity_score', '2afc_fcst_categories', '2afc_continuous_fcsts', 'roc_below', 'roc_above']: 
        cpt.write(413)
        cpt.write(CPT_SKILL_R[skill.upper()])
        cpt.write(cpt.outputs[skill].absolute())
    cpt.wait_for_files()

    skill_values = [open_cptdataset(str(cpt.outputs[i].absolute()) + '.txt') for i in ['pearson', 'spearman', '2afc', 'pct_variance', 'variance_ratio', 'mean_bias', 'root_mean_squared_error', 'mean_absolute_error', 'hit_score', 'hit_skill_score', 'leps', 'gerrity_score', '2afc_fcst_categories', '2afc_continuous_fcsts', 'roc_below', 'roc_above'] ]
    skill_values = [ getattr(i, [ii for ii in i.data_vars][0]) for i in skill_values]
    for i in range(len(skill_values)):
        skill_values[i].name = ['pearson', 'spearman', '2afc', 'pct_variance', 'variance_ratio', 'mean_bias', 'root_mean_squared_error', 'mean_absolute_error', 'hit_score', 'hit_skill_score', 'leps', 'gerrity_score', '2afc_fcst_categories', '2afc_continuous_fcsts', 'roc_below', 'roc_above'][i] 
    skill_values = xr.merge(skill_values).mean('Mode')
    return  skill_values

