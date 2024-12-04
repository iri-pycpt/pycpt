from ..utilities import CPT_SKILL_R, snap_to
from ..base import CPT
from cptio import open_cptdataset, to_cptv10, is_valid_cptv10_xyt
import xarray as xr 


def deterministic_skill(
        X,  # Predictor Dataset in an Xarray DataArray with three coordinates, XYT
        Y,  # Predictand Dataset in an Xarray DataArray with three coordinates, XYT
        synchronous_predictors=False,
        cpt_kwargs=None, # a dict of kwargs that will be passed to CPT
        **_
    ):
    if cpt_kwargs is None:
        cpt_kwargs = {}

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
        cpt.write( "{:#g}".format(max(X.coords['Y'].values))) # North
        cpt.write( "{:#g}".format(min(X.coords['Y'].values))) # South
        cpt.write( "{:#g}".format(min(X.coords['X'].values))) # West
        cpt.write( "{:#g}".format(max(X.coords['X'].values))) # East


    # load Y Dataset 
    to_cptv10(Y, cpt.outputs['original_predictand'])
    cpt.write(2)
    cpt.write(cpt.outputs['original_predictand'].absolute())
    if 'X' in Y.coords:
        assert 'Y' in Y.coords
        cpt.write( "{:#g}".format(max(Y.coords['Y'].values))) # North
        cpt.write( "{:#g}".format(min(Y.coords['Y'].values))) # South
        cpt.write( "{:#g}".format(min(Y.coords['X'].values))) # West
        cpt.write( "{:#g}".format(max(Y.coords['X'].values))) # East

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
    metrics = ['pearson', 'spearman', 'two_alternative_forced_choice', 'roc_area_below_normal', 'roc_area_above_normal', 'root_mean_squared_error']
    for skill in metrics:
        cpt.write(413)
        cpt.write(CPT_SKILL_R[skill.upper()])
        cpt.write(cpt.outputs[skill].absolute())
    cpt.wait_for_files()

    skill_values = [open_cptdataset(str(cpt.outputs[i].absolute()) + '.txt') for i in metrics ]
    skill_values = [ getattr(i, [ii for ii in i.data_vars][0]) for i in skill_values]
    for i in range(len(skill_values)):
        skill_values[i].name = metrics[i] 
    skill_values = xr.merge(skill_values).mean('Mode')
    return  snap_to(Y, skill_values)
