
from ..utilities import CPT_GOODNESS_INDICES_R, CPT_PFV_R, CPT_DEFAULT_VERSION, CPT_TAILORING_R, CPT_OUTPUT_NEW,  CPT_SKILL_R, CPT_TRANSFORMATIONS_R
from ..base import CPT
from cptio import open_cptdataset, to_cptv10, is_valid_cptv10_xyt
import xarray as xr 



def probabilistic_forecast_verification(
        X,  # Predictor Dataset in an Xarray DataArray with three dimensions, XYT 
        Y,  # Predictand Dataset in an Xarray DataArray with three dimensions, XYT 
        synchronous_predictors=False,
        cpt_kwargs=None, # a dict of kwargs that will be passed to CPT 
        **kwargs
    ):
    if cpt_kwargs is None:
        cpt_kwargs = {}

    is_valid_cptv10_xyt(X)
    is_valid_cptv10_xyt(Y)

    X.name = Y.name

    cpt = CPT(**cpt_kwargs)
    cpt.write(621) # activate CCA MOS 
    if synchronous_predictors: 
        cpt.write(545)
   
    cpt.write(544) # missing value settings 
    cpt.write(X.attrs['missing'])
    cpt.write(10)
    cpt.write(10)

    cpt.write(Y.attrs['missing'])
    cpt.write(10)
    cpt.write(10)
    cpt.write(1)
    cpt.write(4 )
    
    # Load X dataset 
    to_cptv10(X, cpt.outputs['original_predictor'])
    cpt.write(1)
    cpt.write(cpt.outputs['original_predictor'].absolute())

    cpt.write( max(X.coords['Y'].values)) # North
    cpt.write( min(X.coords['Y'].values)) # South
    cpt.write( min(X.coords['X'].values)) # West
    cpt.write( max(X.coords['X'].values)) # East
    
    # load Y Dataset 
    to_cptv10(Y, cpt.outputs['original_predictand'])
    cpt.write(2)
    cpt.write(cpt.outputs['original_predictand'].absolute())

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

    #initiate analysis 
    cpt.write(313)

    # save all probabilistic skill scores 
    for skill in ['generalized_roc', 'ignorance', 'rank_probability_skill_score']: 
        cpt.write(437)
        cpt.write(CPT_PFV_R[skill.upper()])
        cpt.write(cpt.outputs[skill].absolute())
    cpt.wait_for_files()

    skill_values = [ open_cptdataset(str(cpt.outputs[i].absolute()) + '.txt') for i in ['generalized_roc', 'ignorance', 'rank_probability_skill_score'] ]
    skill_values = [ getattr(i, [ii for ii in i.data_vars][0]) for i in skill_values]
    for i in range(len(skill_values)):
        skill_values[i].name = ['generalized_roc', 'ignorance', 'rank_probability_skill_score'][i] 
    skill_values = xr.merge(skill_values).mean('Mode')
    return skill_values 

