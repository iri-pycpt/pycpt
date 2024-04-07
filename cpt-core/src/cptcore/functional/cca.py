from ..utilities import CPT_GOODNESS_INDICES_R, CPT_DEFAULT_VERSION, CPT_TAILORING_R, CPT_OUTPUT_NEW,  CPT_SKILL_R, CPT_TRANSFORMATIONS_R, CPT_PFV_R
from ..base import CPT
from cptio import open_cptdataset, to_cptv10, guess_cptv10_coords, is_valid_cptv10, convert_np64_datetime
import xarray as xr 
import datetime as dt 
import numpy as np 

PFV_METRICS = ['generalized_roc', 'ignorance', 'rank_probability_skill_score']

def canonical_correlation_analysis(
        X,  # Predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        Y,  # Predictand Dataset in an Xarray DataArray with three dimensions, XYT 
        F=None, # New Out of sample (forecast) predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        transform_predictand=None,  # transformation to apply to the predictand dataset - None, 'Empirical', 'Gamma'
        tailoring=None,  # tailoring None, Anomaly, StdAnomaly, or SPI (SPI only available on Gamma)
        cca_modes=(1,5), # minimum and maximum of allowed CCA modes 
        x_eof_modes=(1,5), # minimum and maximum of allowed X Principal Componenets 
        y_eof_modes=(1,5), # minimum and maximum of allowed Y Principal Components 
        crossvalidation_window=5,  # number of samples to leave out in each cross-validation step 
        retroactive_initial_training_period=45, # percent of samples to be used as initial training period for retroactive validation
        retroactive_step=10, # percent of samples to increment retroactive training period by each time. 
        validation='crossvalidation', #type of leave-n-out crossvalidation to use
        drymask=False,
        drymask_value=0, 	# added by AWR 04/06/24
        skillmask=False,	# added by AWR 04/06/24
        skillmask_value=0, 	# added by AWR 04/06/24
        scree=False,
        synchronous_predictors=False,
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
        **kwargs
    ):
    assert validation.upper() in ['DOUBLE-CROSSVALIDATION', 'CROSSVALIDATION', 'RETROACTIVE'], "validation must be one of ['DOUBLE-CROSSVALIDATION', 'CROSSVALIDATION', 'RETROACTIVE']"
    assert isinstance(crossvalidation_window, int) and crossvalidation_window %2 == 1, "crossvalidation window must be odd integer"
    assert 0 < retroactive_initial_training_period < 100, 'retroactive_initial_training_period must be a percentage between 0 and 100'
    assert 0 < retroactive_step < 100, 'retroactive_step must be a percentage between 0 and 1'

    x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim = guess_cptv10_coords(X, x_lat_dim, x_lon_dim, x_sample_dim,  x_feature_dim )
    is_valid_cptv10(X)

    y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim = guess_cptv10_coords(Y, y_lat_dim, y_lon_dim, y_sample_dim,  y_feature_dim )
    is_valid_cptv10(Y)

    if F is not None: 
        f_lat_dim, f_lon_dim, f_sample_dim,  f_feature_dim = guess_cptv10_coords(F, f_lat_dim, f_lon_dim, f_sample_dim,  f_feature_dim )
        is_valid_cptv10(F)
        
    retroactive_initial_training_period = int(retroactive_initial_training_period / 100 * X.shape[list(X.dims).index(x_sample_dim)])
    retroactive_step = int(retroactive_step / 100 * X.shape[list(X.dims).index(x_sample_dim)])

    cpt = CPT(**cpt_kwargs)
    cpt.write(611) # activate CCA MOS 

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

    if synchronous_predictors: 
        cpt.write(545)
    # apply tailoring 
    tailoring = str(tailoring)
    assert  (tailoring.upper() == 'SPI' and transform_predictand.upper()  == 'GAMMA')  or  tailoring != 'SPI', 'Standard Precipitation Index (SPI) tailoring is only available if transform_predictand=Gamma'
    
    cpt.write(533)
    cpt.write(CPT_TAILORING_R[tailoring.upper()])
    cpt.write(1) # climatologicial probability thresholds 
    cpt.write(0.33) # size of AN category 
    cpt.write(0.33) # size of BN category  
    if drymask:
        cpt.write(5371)
        cpt.write('Y')
        cpt.write(drymask_value)		# added by AWR 04/06/24
#        cpt.write(Y.attrs['missing'])

    if skillmask:	# added by AWR 04/06/24
        cpt.write(5372)
        cpt.write('Y')
        cpt.write(1)	# for Pearson
        cpt.write(skillmask_value)	

    # set cross validation window
    assert type(crossvalidation_window) == int and crossvalidation_window % 2 == 1 # xval window must be an odd integer 
    cpt.write(534)
    cpt.write(crossvalidation_window)

    # apply transform_predictand 
    if transform_predictand is not None: 
        cpt.write(554) #set transform 
        cpt.write(CPT_TRANSFORMATIONS_R[transform_predictand.upper()])
        cpt.write(541) #activate transform 
    
    # Load X dataset 
    to_cptv10(X, cpt.outputs['original_predictor'], row=x_lat_dim, col=x_lon_dim, T=x_sample_dim)
    cpt.write(1)
    cpt.write(cpt.outputs['original_predictor'].absolute())
    if len(X.coords) >= 3: # then this is gridded data
        cpt.write( max(X.coords[x_lat_dim].values)) # North
        cpt.write( min(X.coords[x_lat_dim].values)) # South
        cpt.write( min(X.coords[x_lon_dim].values)) # West
        cpt.write( max(X.coords[x_lon_dim].values)) # East 
    cpt.write(x_eof_modes[0])
    cpt.write(x_eof_modes[1])

  

    # load Y Dataset 
    to_cptv10(Y, cpt.outputs['original_predictand'], row=y_lat_dim, col=y_lon_dim, T=y_sample_dim)
    cpt.write(2)
    cpt.write(cpt.outputs['original_predictand'].absolute())
    if len(Y.coords) >= 3: # then this is gridded data
        cpt.write( max(Y.coords[y_lat_dim].values)) # North
        cpt.write( min(Y.coords[y_lat_dim].values)) # South
        cpt.write( min(Y.coords[y_lon_dim].values)) # West
        cpt.write( max(Y.coords[y_lon_dim].values)) # East 
    cpt.write(y_eof_modes[0])
    cpt.write(y_eof_modes[1])
    cpt.write(cca_modes[0])
    cpt.write(cca_modes[1])

    # set up cpt missing values and goodness index 
    cpt.write(131) # set output fmt to text for goodness index because grads doesnot makes sense
    cpt.write(2)
    # set sigfigs to 6
    cpt.write(132)
    cpt.write(6) 
    cpt.write(531) # Kendalls Tau goodness index 
    cpt.write(3)


    cpt.write(112) 
    cpt.write(cpt.outputs['goodness_index'].absolute())

    val = validation.upper()

    #initiate analysis 
    if val == 'CROSSVALIDATION':
        cpt.write(311)
    elif val == 'DOUBLE-CROSSVALIDATION':
        cpt.write(314)
    elif val == 'RETROACTIVE':
        cpt.write(312)
        cpt.write(retroactive_initial_training_period)
        cpt.write(retroactive_step)
    else:
        assert False, 'INVALID VALIDATION OPTION'

    # save all deterministic skill scores 
    for skill in ['pearson', 'spearman', '2afc', 'roc_below', 'roc_above']: 
        if val in ['CROSSVALIDATION', 'DOUBLE-CROSSVALIDATION']:
            cpt.write(413)
        elif val == 'RETROACTIVE':
            cpt.write(423)
        else:
            assert False
        cpt.write(CPT_SKILL_R[skill.upper()])
        cpt.write(cpt.outputs[skill].absolute())

    # save all probabilistic skill scores, if applicable
    if val in ['DOUBLE-CROSSVALIDATION', 'RETROACTIVE']:
        for skill in PFV_METRICS:
            cpt.write(437)
            cpt.write(CPT_PFV_R[skill.upper()])
            cpt.write(cpt.outputs[skill].absolute())
        cpt.wait_for_files()

    # output predictions
    cpt.write(111)
    if val == 'CROSSVALIDATION':
        cpt.write(201)
    elif val == 'DOUBLE-CROSSVALIDATION':
        cpt.write('221')
    elif val == 'RETROACTIVE':
        cpt.write(211)
    else:
        assert False
    cpt.write( cpt.outputs['hindcast_values'].absolute())
    cpt.write('0') 

    # output hindcast probabilistic information if available
    if val == 'CROSSVALIDATION':
        # not available
        pass
    elif val == 'DOUBLE-CROSSVALIDATION':
        cpt.write('111')
        cpt.write('226')
        cpt.write( cpt.outputs['hindcast_prediction_error_variance'].absolute())
        cpt.write('0') 

        cpt.write('111')
        cpt.write('224')
        cpt.write( cpt.outputs['hindcast_probabilities'].absolute())
        cpt.write('0')
    elif val == 'RETROACTIVE':
        cpt.write(111)
        cpt.write(216)
        cpt.write(cpt.outputs['hindcast_prediction_error_variance'].absolute())
        cpt.write(0)

        cpt.write('111')
        cpt.write('214')
        cpt.write( cpt.outputs['hindcast_probabilities'].absolute())
        cpt.write('0')
    else:
        assert False

    if F is not None: 
        # load F dataset if present 
        to_cptv10(F, cpt.outputs['out_of_sample_predictor'], row=f_lat_dim, col=f_lon_dim, T=f_sample_dim)
        cpt.write(3)
        cpt.write(cpt.outputs['out_of_sample_predictor'].absolute())
        
        cpt.write(454) # deterministic forecasts 

        cpt.write(111)
        cpt.write(511)
        cpt.write(cpt.outputs['forecast_values'].absolute())
        cpt.write(0)

        cpt.write(111)
        cpt.write(514)
        cpt.write(cpt.outputs['forecast_prediction_error_variance'].absolute())
        cpt.write(0)

        cpt.write(455) # probabilistic forecasts 
        cpt.write(111)
        cpt.write(501)
        cpt.write(cpt.outputs['forecast_probabilities'].absolute())
        cpt.write(0)


    for data in ['cca_x_timeseries', 'cca_y_timeseries', 'cca_canonical_correlation', 'eof_x_timeseries', 'eof_y_timeseries', 'eof_x_loadings', 'eof_y_loadings', 'cca_x_loadings', 'cca_y_loadings']:
        cpt.write('111')
        cpt.write(CPT_OUTPUT_NEW[data])
        cpt.write(cpt.outputs[data].absolute())
        cpt.write(0)
    
    if scree: 
        cpt.write('111')
        cpt.write(CPT_OUTPUT_NEW['eof_y_explained_variance'])
        cpt.write(cpt.outputs['eof_y_explained_variance'].absolute())
        cpt.write(0)

        cpt.write('111')
        cpt.write(CPT_OUTPUT_NEW['eof_x_explained_variance'])
        cpt.write(cpt.outputs['eof_x_explained_variance'].absolute())
        cpt.write(0)

    #cpt.kill()
    cpt.wait_for_files()
    fcsts = None
    if F is not None: 
        prob_fcst = open_cptdataset(str(cpt.outputs['forecast_probabilities'].absolute()) + '.txt')
        prob_fcst = getattr(prob_fcst, [i for i in prob_fcst.data_vars][0])
        prob_fcst.name = 'probabilistic'

        det_fcst = open_cptdataset(str(cpt.outputs['forecast_values'].absolute()) + '.txt')
        det_fcst = getattr(det_fcst, [i for i in det_fcst.data_vars][0])
        det_fcst.name = 'deterministic'

        pev = open_cptdataset(str(cpt.outputs['forecast_prediction_error_variance'].absolute()) + '.txt')
        pev = getattr(pev, [i for i in pev.data_vars][0])
        pev.name = 'prediction_error_variance'
        fcsts = xr.merge([det_fcst, prob_fcst, pev])
        # CPT doesn't pass through the S coordinate, so put it back on
        # if we have it.  (We don't have it yet in the subseasonal
        # case. Have to get rid of the fake T grid hack first in order
        # to fix that.)
        if 'S' in F.coords:
            assert len(F['S']) == len(fcsts.coords['T'])
            fcsts = fcsts.assign_coords(S=('T', F['S'].data))

    hcsts = open_cptdataset(str(cpt.outputs['hindcast_values'].absolute()) + '.txt' )
    hcsts = getattr(hcsts, [i for i in hcsts.data_vars][0])
    hcsts.name = 'deterministic' 

    if val in ['DOUBLE-CROSSVALIDATION', 'RETROACTIVE']:
        hcst_pr = open_cptdataset(str(cpt.outputs['hindcast_probabilities'].absolute()) + '.txt' )
        hcst_pr = getattr(hcst_pr, [i for i in hcst_pr.data_vars][0])
        hcst_pr.name = 'probabilistic'

        hcst_pev = open_cptdataset(str(cpt.outputs['hindcast_prediction_error_variance'].absolute()) + '.txt' )
        hcst_pev = getattr(hcst_pev, [i for i in hcst_pev.data_vars][0])
        hcst_pev.name = 'prediction_error_variance' 
        if 'T' in hcst_pev.coords:
            hcst_pev = hcst_pev.drop('T')
        if 'S' in hcst_pev.coords: 
            hcst_pev = hcst_pev.drop('S')
        if 'Ti' in hcst_pev.coords: 
            hcst_pev = hcst_pev.drop('Ti')
        if 'Tf' in hcst_pev.coords: 
            hcst_pev = hcst_pev.drop('Tf')
        hcst_pev = hcst_pev.assign_coords({'T': hcst_pr.coords['T']})

        hcsts = xr.merge([hcsts, hcst_pr, hcst_pev])
    else:
        hcsts = xr.merge([hcsts])

    # CPT doesn't pass through the S coordinate, so put it back on.
    if 'S' in X.coords:
        assert len(X['S']) == len(hcsts.coords['T']), f"Calibrated hindcast doesn't have the same number of years as the original: {X['S']}\n{hcsts['T']}"
        hcsts = hcsts.assign_coords(S=('T', X['S'].data))

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
    roc_below.name = 'roc_area_below_normal'
    roc_above = open_cptdataset(str(cpt.outputs['roc_above'].absolute()) + '.txt')
    roc_above = getattr(roc_above, [i for i in roc_above.data_vars][0])
    roc_above.name = 'roc_area_above_normal'
    skill_values = [pearson, spearman, two_afc, roc_below, roc_above]

    if val in ['DOUBLE-CROSSVALIDATION', 'RETROACTIVE']:
        hcst_pr_skill = [
            next(iter(
                open_cptdataset(str(cpt.outputs[metric].absolute()) + '.txt').data_vars.values()
            )).rename(metric)
            for metric in PFV_METRICS
        ]
        skill_values += hcst_pr_skill

    skill_values = xr.merge(skill_values).mean('Mode')


    x_cca_scores = open_cptdataset(str(cpt.outputs['cca_x_timeseries'].absolute()) + '.txt')
    x_cca_scores = getattr(x_cca_scores, [i for i in x_cca_scores.data_vars][0])
    x_cca_scores.name = "x_cca_scores"
    x_cca_scores = x_cca_scores.rename({'index':'Mode'})

    y_cca_scores = open_cptdataset(str(cpt.outputs['cca_y_timeseries'].absolute()) + '.txt')
    y_cca_scores = getattr(y_cca_scores, [i for i in y_cca_scores.data_vars][0])
    y_cca_scores.name = "y_cca_scores"
    y_cca_scores = y_cca_scores.rename({'index':'Mode'})

    x_cca_loadings = open_cptdataset(str(cpt.outputs['cca_x_loadings'].absolute()) + '.txt')
    x_cca_loadings = getattr(x_cca_loadings, [i for i in x_cca_loadings.data_vars][0])
    x_cca_loadings.name = "x_cca_loadings"
    x_cca_loadings.coords['Mode'] = x_cca_scores.coords['Mode'].values

    y_cca_loadings = open_cptdataset(str(cpt.outputs['cca_y_loadings'].absolute()) + '.txt')
    y_cca_loadings = getattr(y_cca_loadings, [i for i in y_cca_loadings.data_vars][0])
    y_cca_loadings.name = "y_cca_loadings"
    y_cca_loadings.coords['Mode'] = y_cca_scores.coords['Mode'].values

    x_eof_scores = open_cptdataset(str(cpt.outputs['eof_x_timeseries'].absolute()) + '.txt')
    x_eof_scores = getattr(x_eof_scores, [i for i in x_eof_scores.data_vars][0])
    x_eof_scores.name = "x_eof_scores"
    x_eof_scores = x_eof_scores.rename({'index':'Mode'})

    y_eof_scores = open_cptdataset(str(cpt.outputs['eof_y_timeseries'].absolute()) + '.txt')
    y_eof_scores = getattr(y_eof_scores, [i for i in y_eof_scores.data_vars][0])
    y_eof_scores.name = "y_eof_scores"
    y_eof_scores = y_eof_scores.rename({'index':'Mode'})

    x_eof_loadings = open_cptdataset(str(cpt.outputs['eof_x_loadings'].absolute()) + '.txt')
    x_eof_loadings = getattr(x_eof_loadings, [i for i in x_eof_loadings.data_vars][0])
    x_eof_loadings.name = "x_eof_loadings"
    x_eof_loadings.coords['Mode'] = x_eof_scores.coords['Mode'].values

    y_eof_loadings = open_cptdataset(str(cpt.outputs['eof_y_loadings'].absolute()) + '.txt')
    y_eof_loadings = getattr(y_eof_loadings, [i for i in y_eof_loadings.data_vars][0])
    y_eof_loadings.name = "y_eof_loadings"
    y_eof_loadings.coords['Mode'] = y_eof_scores.coords['Mode'].values
    
    if scree:
        x_varfile = np.genfromtxt(str(cpt.outputs['eof_x_explained_variance']) + '.txt', skip_header=3, delimiter='\t', dtype=float)
        y_varfile = np.genfromtxt( str( cpt.outputs['eof_y_explained_variance']) +'.txt', skip_header=3, delimiter='\t', dtype=float)
        x_explained_variance = xr.DataArray(name='x_explained_variance', data=x_varfile[:,-2].squeeze(), dims=('Mode'), coords={'Mode': x_varfile[:, 0].squeeze()})
        y_explained_variance = xr.DataArray(name='y_explained_variance', data=y_varfile[:,-2].squeeze(), dims=('Mode'), coords={'Mode': y_varfile[:, 0].squeeze()})

        x_pattern_values = [ x_cca_scores,  x_eof_scores, x_cca_loadings,  x_eof_loadings, x_explained_variance]
        y_pattern_values = [y_cca_scores, y_eof_scores, y_cca_loadings,   y_eof_loadings, y_explained_variance ]
    else: 
        x_pattern_values = [ x_cca_scores,  x_eof_scores, x_cca_loadings,  x_eof_loadings]
        y_pattern_values = [y_cca_scores, y_eof_scores, y_cca_loadings,   y_eof_loadings ]

    x_pattern_values = xr.merge(x_pattern_values)
    x_pattern_values.coords[x_sample_dim] = [convert_np64_datetime(i) for i in x_pattern_values.coords[x_sample_dim].values]

    y_pattern_values = xr.merge(y_pattern_values)
    y_pattern_values.coords[y_sample_dim] = [convert_np64_datetime(i) for i in y_pattern_values.coords[y_sample_dim].values]

    return hcsts, fcsts, skill_values, x_pattern_values, y_pattern_values  

