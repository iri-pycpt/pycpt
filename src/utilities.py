import  os, time, copy 
from pathlib import Path 
import datetime as dt 
import subprocess, platform 
import shutil

CPT_SECRET  = 'C\bP\bT\b \b'


#CPT keys 
CPT_MOS = {
    'CCA': 611, #'Canonical Correlation Analysis'], 
    #'MLR': 613, # 'Multiple Linear Regression'],  #not supporting MLR right now
    'PCR': 612, #'Principal Components Regression'], 
    'GCM':614,#'GCM Validation'], 
    #'PFV': 621, #'Probabilistic Forecast Verification'],
    'None': 614
 } # 'No Mos (GCM Validation)']}

CPT_MOS_R = {CPT_MOS[k]: k  for k in CPT_MOS.keys()}


CPT_GOODNESS_INDICES = {
    1: 'PEARSON_GOODNESS', 
    2: 'SPEARMAN_GOODNESS', 
    3: 'KENDALL_GOODNESS',
    4: 'AIC_GOODNESS', 
    5: 'BIC_GOODNESS', 
    6: 'HQC_GOODNESS'
}

CPT_GOODNESS_INDICES_R = {CPT_GOODNESS_INDICES[k]:k for k in CPT_GOODNESS_INDICES.keys()}

CPT_MISSING_VALUE_REPLACEMENTS = {
    1: 'LONG_TERM_MEANS', 
    2: 'LONG_TERM_MEDIANS', 
    3: 'RANDOM', 
    4: 'BEST_NEAREST_NEIGHBORS' 
}   

CPT_MISSING_VALUE_REPLACEMENTS_R = {CPT_MISSING_VALUE_REPLACEMENTS[k]:k for k in CPT_MISSING_VALUE_REPLACEMENTS.keys()}


CPT_OUTPUT_FMT = {
    1: 'UNFORMATTED', 
    2: 'ASCII', 
    3: 'GRADS'
}

CPT_OUTPUT_FMT_R = {CPT_OUTPUT_FMT[k]:k for k in CPT_OUTPUT_FMT.keys()}

CPT_SKILL = {
    1: 'PEARSON', 
    2: 'SPEARMAN', 
    3: '2AFC', 
    4: 'PCT_VARIANCE', 
    5: 'VARIANCE_RATIO', 
    6: 'MEAN_BIAS', 
    7: 'ROOT_MEAN_SQUARED_ERROR', 
    8: 'MEAN_ABSOLUTE_ERROR', 
    9: 'HIT_SCORE', 
    10: 'HIT_SKILL_SCORE', 
    11: 'LEPS', 
    12: 'GERRITY_SCORE', 
    13: '2AFC_FCST_CATEGORIES', 
    14: '2AFC_CONTINUOUS_FCSTS', 
    16: 'ROC_ABOVE', 
    15: 'ROC_BELOW',
} 
CPT_SKILL_R = {CPT_SKILL[k]:k for k in CPT_SKILL.keys()}


CPT_TAILORING = {
    1: 'NONE', 
    2: 'ANOMALY',
    3: 'STDANOMALY',
    4: 'SPI',
}
CPT_TAILORING_R = { CPT_TAILORING[k]: k for k in CPT_TAILORING.keys()}

CPT_TRANSFORMATIONS = { 
    1: 'EMPIRICAL',
    2: 'GAMMA',
}

CPT_TRANSFORMATIONS_R = { CPT_TRANSFORMATIONS[k]: k for k in CPT_TRANSFORMATIONS.keys()}

CPT_OUTPUT_NEW = {
    'cca_x_timeseries': 412,
    'cca_y_timeseries': 422,
    'cca_canonical_correlation': 401,
    'eof_x_timeseries': 303,
    'eof_y_timeseries': 313,
    'eof_x_loadings': 302,
    'eof_y_loadings': 312,
    'cca_x_loadings': 411,
    'cca_y_loadings': 421,
}


CPT_OUTPUT = {
    101: "X_INPUT",
    102: "Y_INPUT",
    121: "Y_CATEGORIES",
    122: "RETROACTIVE_CATEGORIES",
    201: "CROSSVALIDATED_PREDICTIONS",
    202: "RETROACTIVE_PREDICTIONS",
    203: "RETROACTIVE_FCST_PROBABILITIES",
    204: "RETROACTIVE_PREDICTION_LIMITS",
    301: "X_EIGENVALUES",
    302: "X_SPATIAL_LOADINGS",
    303: "X_TEMPORAL_SCORES",
    312: 'Y_SPATIAL_LOADINGS',
    313: 'Y_TEMPORAL_SCORES', 
    311: 'Y_EIGENVALUES',
    401: 'CANONICAL_CORRELATIONS',
    411: 'X_CCA_MAP_LOADINGS',
    412: 'X_CCA_MAP_SERIES', 
    421: 'Y_CCA_MAP_LOADINGS', 
    422: 'Y_CCA_MAP_SERIES',
    431: "REGRESSION_COEFFICIENTS",
    432: "PC_REGRESSION_COEFFICIENTS",
    501: "FORECAST_PROBABILITIES",
    502: "FORECAST_ODDS",
    511: "FORECASTS",
    512: "FORECAST_ENSEMBLES",
    513: "PREDICTION_LIMITS",
    514: "PREDICTION_ERROR_VARIANCES",
    531: "PREDICTOR_TIME_SCORES",
    532: 'Z', #manually added from seasonalv1.9, havent tested yet 
    601: "THRESHOLDS",
    602: "AVERAGES",
    604: "STANDARD_DEVIATIONS",
    605: "COEFFICIENTS_OF_VARIATION" 
}
CPT_OUTPUT_R = {CPT_OUTPUT[k]:k for k in CPT_OUTPUT.keys()}


CPT_PFV = {
    122: 'RANK_PROBABILITY_SKILL_SCORE', 
    101: 'IGNORANCE', 
    131: 'GENERALIZED_ROC', 
    203: 'IGNORANCE_RESOLUTION_PFV', 
    202: 'IGNORANCE_RELIABILITY_PFV', 
    201: 'IGNORANCE_AN_PFV'
}

CPT_PFV_R = {CPT_PFV[k]:k for k in CPT_PFV.keys()}


CPT_REGRESSIONS = { 
    1: 'OLS', 
    2: 'LOGISTIC', 
    3: 'BINOMIAL', 
    4: 'POISSON', 
    5: 'GAMMA'
}

CPT_REGRESSIONS_R = {CPT_REGRESSIONS[k]:k for k in CPT_REGRESSIONS.keys()}


CPT_LINKS = { 
    1: 'IDENTITY', 
    3: 'INVERSE',
    4: 'LOG',
    5: 'SQRT' 
}
CPT_LINKS_R = {CPT_LINKS[k]:k for k in CPT_LINKS.keys()}

CPT_DEFAULT_CONFIGURATION = {
    "EOF_XMODES_MIN":  1,  # int between 1 and 10
    "EOF_XMODES_MAX":  10,  # int > EOF_XMODES_MIN and less than 11
    "EOF_YMODES_MIN":  1,   #int between 1 and 10 
    "EOF_YMODES_MAX":  10,  # int > EOF_YMODES_MIN and less than 11
    "CCA_MODES_MIN":  1, #int between 1 and 10 
    "CCA_MODES_MAX":  5,  # int > EOF_XMODES_MIN and less than 6
    "GOODNESS_INDEX":  3, #1, 2, 3, 4, 5 or 6  as specified in CPT_GOODNESS_INDICES
    "CROSSVALIDATION_WINDOW":  5, # odd int < sample size 
    "TRANSFORM_PREDICTAND":  False,  # bool whether or not to transform the predictand
    "SYNCHRONOUS_PREDICTORS":  False,  # bool whether or not to use synchronous predictors
    "ENSEMBLE_AVERAGE":  False,     # bool whether or not to take ensemble average
    "SORT_ENSEMBLE_MEMBERS":  False,  # bool whether or not to sort ensemble members
    "ZERO_BOUND":  False,    # bool whether or not to bound values to positive domain
    "X_MISSING_VALUE":  -999, # int missing value for predictors
    "X_MISSING_VALUE_PERCENT_LIMIT":  10,  # int 1-100 max percent missing values
    "X_MISSING_GRIDPOINT_PERCENT_LIMIT":  10,  #int 1-100 max percent missing gridpoints
    "X_MISSING_VALUE_REPLACEMENT":  4,   # missing value replacement strategy, 1,2, 3, or 4 
    "X_KNN_REPLACEMENT_NUM_NEIGHBORS":  1, # number of near neighbors for KNN replacement stratagy
    "Y_MISSING_VALUE":  -999,   # int missing value for predictands
    "Y_MISSING_VALUE_PERCENT_LIMIT":  10, # int 1-100 max percent missing values
    "Y_MISSING_GRIDPOINT_PERCENT_LIMIT":  10, #int 1-100 max percent missing gridpoints
    "Y_MISSING_VALUE_REPLACEMENT": 4, # missing value replacement strategy, 1,2, 3, or 4 
    "Y_KNN_REPLACEMENT_NUM_NEIGHBORS":  1, # number of near neighbors for KNN replacement stratagy
    "TAILORING": 1, # one of 1- No Tailoring 2- Anomalies 3- Standard Anomalies 4- Standard Precipitation INdex 
    "THRESHOLD_TYPE": 1, # one of 1 - climatological probabilities 2- absolute thresholds
    "ABOVE_NORMAL_THRESHOLD": 0.33, # the threshold indicating the bottom of the 'above normal' range
    "BELOW_NORMAL_THRESHOLD": 0.33, # the threshold indicating the top of the 'below normal' range
    "INITIAL_TRAINING_PERIOD": 35, # the initial trainig period to use for retroactive forecasts 
    "TRAINING_PERIOD_INCREMENT": 10 # the amount by which to increas the training period each time for retroactive forecasts
}

def check_configuration(config):
    for key in config.keys():
        assert key in CPT_DEFAULT_CONFIGURATION.keys(), 'UNKOWN CONFIGURATION SETTING: {}'.format(key)
    for key in CPT_DEFAULT_CONFIGURATION.keys():
        assert key in config.keys(), 'MISSING CONFIGURATION SETTING: {}'.format(key)
    assert 0 < config["EOF_XMODES_MIN"] < config["EOF_XMODES_MAX"] <= 10, 'EOF_XMODES_MIN and EOF_XMODES_MAX must be [1,10] and MIN < MAX'
    assert 0 < config["EOF_YMODES_MIN"] < config["EOF_YMODES_MAX"] <= 10, 'EOF_YMODES_MIN and EOF_YMODES_MAX must be [1,10] and MIN < MAX'
    assert 0 < config["CCA_MODES_MIN"] < config["CCA_MODES_MAX"] <= 10, 'CCA_MODES_MIN and CCA_MODES_MAX must be [1,10] and MIN < MAX'

    assert config['GOODNESS_INDEX'] in CPT_GOODNESS_INDICES.keys() or config['GOODNESS_INDEX'] in CPT_GOODNESS_INDICES_R.keys(),  "GOODNESS INDEX MUST BE IN [1,2,3,4,5,6] according to  {}".format(CPT_GOODNESS_INDICES)
    if config['GOODNESS_INDEX'] in CPT_GOODNESS_INDICES_R.keys(): 
        config['GOODNESS_INDEX'] = CPT_GOODNESS_INDICES_R[config['GOODNESS_INDEX']]
    assert config["CROSSVALIDATION_WINDOW"] % 2 == 1, "CROSSVALIDATION WINDOW MUST BE ODD INT"
    assert config["TRANSFORM_PREDICTAND"] in [True, False], "TRANSFORM_PREDICTAND MUST BE BOOL"
    assert config["SYNCHRONOUS_PREDICTORS"] in [True, False], "SYNCHRONOUS_PREDICTORS MUST BE BOOL"
    assert config["ENSEMBLE_AVERAGE"] in [True, False], "ENSEMBLE_AVERAGE MUST BE BOOL"
    assert config["SORT_ENSEMBLE_MEMBERS"] in [True, False], "SORT_ENSEMBLE_MEMBERS MUST BE BOOL"
    assert config["ZERO_BOUND"] in [True, False], "ZERO_BOUND MUST BE BOOL"
    assert type(config["X_MISSING_VALUE"]) in [int, float], "MISSING_VALUE MUST BE INT" 
    assert type(config["Y_MISSING_VALUE"]) in [int, float], "MISSING_VALUE MUST BE INT" 
    assert 1 <= config["X_MISSING_VALUE_PERCENT_LIMIT"]  <= 100, "X_MISSING_VALUE_PERCENT_LIMIT must be INT [1,100]"
    assert 1 <= config["Y_MISSING_VALUE_PERCENT_LIMIT"]  <= 100, "Y_MISSING_VALUE_PERCENT_LIMIT must be INT [1,100]"
    assert 1 <= config["Y_MISSING_GRIDPOINT_PERCENT_LIMIT"]  <= 100, "Y_MISSING_GRIDPOINT_PERCENT_LIMIT must be INT [1,100]"
    assert 1 <= config["X_MISSING_GRIDPOINT_PERCENT_LIMIT"]  <= 100, "X_MISSING_GRIDPOINT_PERCENT_LIMIT must be INT [1,100]"
    assert config['X_MISSING_VALUE_REPLACEMENT'] in CPT_MISSING_VALUE_REPLACEMENTS.keys() or config['X_MISSING_VALUE_REPLACEMENT'] in CPT_MISSING_VALUE_REPLACEMENTS_R.keys(), "X_MISSING_VALUE_REPLACEMENT must be 1,2,3,4 according to {}".format(CPT_MISSING_VALUE_REPLACEMENTS)
    assert config['Y_MISSING_VALUE_REPLACEMENT'] in CPT_MISSING_VALUE_REPLACEMENTS.keys() or config['X_MISSING_VALUE_REPLACEMENT'] in CPT_MISSING_VALUE_REPLACEMENTS_R.keys(), "Y_MISSING_VALUE_REPLACEMENT must be 1,2,3,4 according to {}".format(CPT_MISSING_VALUE_REPLACEMENTS)
    if config['X_MISSING_VALUE_REPLACEMENT'] in CPT_MISSING_VALUE_REPLACEMENTS_R.keys():
        config['X_MISSING_VALUE_REPLACEMENT'] = CPT_MISSING_VALUE_REPLACEMENTS_R[config['X_MISSING_VALUE_REPLACEMENT']]
    if config['Y_MISSING_VALUE_REPLACEMENT'] in CPT_MISSING_VALUE_REPLACEMENTS_R.keys():
        config['Y_MISSING_VALUE_REPLACEMENT'] = CPT_MISSING_VALUE_REPLACEMENTS_R[config['Y_MISSING_VALUE_REPLACEMENT']]
    assert type(config["Y_KNN_REPLACEMENT_NUM_NEIGHBORS"]) == int and config["Y_KNN_REPLACEMENT_NUM_NEIGHBORS"] > 0, "Y KNN missing value Replacement needs at least one neighboer "
    assert type(config["X_KNN_REPLACEMENT_NUM_NEIGHBORS"]) == int and config["X_KNN_REPLACEMENT_NUM_NEIGHBORS"] > 0, "X KNN missing value Replacement needs at least one neighboer "
    assert config["TAILORING"] in [1, 2, 3, 4], "invalid tailoring selection - must be 1, 2, 3,4"
    assert config['THRESHOLD_TYPE'] in [ 1, 2], 'INVALID THRESHOLD SELECTION - must be 1, 2'
    assert config['ABOVE_NORMAL_THRESHOLD'] > config['BELOW_NORMAL_THRESHOLD'], 'ABOVE NORMAL THRESHOLD MUST BE GREAATER THAN BELOW NORMAL THRESHOLD'

def detect_changes(outputdir, wait=0.2):
    cur_sizes = {}    
    cur_files = [ f for f in Path(outputdir).glob('*') ]
    for f in cur_files:
        cur_sizes[f] = os.stat(str(f)).st_size

    detected_changes, i = True, 0
    while detected_changes:
        time.sleep(wait)
        i += 1
        new_sizes = {}
        new_files = [ f for f in Path(outputdir).glob('*') ]
        for f in new_files:
            new_sizes[f] = os.stat(str(f)).st_size
        
        detected_changes = False
        for key in new_sizes.keys():
            if key not in cur_sizes.keys() or cur_sizes[key] != new_sizes[key]:
                detected_changes = True 
        cur_sizes = copy.deepcopy(new_sizes )
    return dt.timedelta(seconds=i*wait)

CPT_DEFAULT_VERSION = '17.7.0'
CPT_VALID_VERSIONS = ['17.7.0']

def install_cpt_unix(version=CPT_DEFAULT_VERSION):
    assert platform.system() != 'Windows', 'On Windows, you should be using install_cpt_windows'
    assert version in CPT_VALID_VERSIONS, f'PyCPT Requires CPT version to be one of {CPT_VALID_VERSIONS}'
    CPT_SPACE = Path().home().absolute() / '.pycpt_worker_space'

    #if CPT_SPACE.is_dir():
    #    rmrf(CPT_SPACE)
    CPT_SPACE.mkdir(exist_ok=True, parents=True)

    ORIGINAL_CPT_TARBALL = Path(__file__).parents[0]/ 'fortran' / f'CPT.{version}.tar.gz'
    CPT_TARBALL = CPT_SPACE / f'CPT.{version}.tar.gz'
    CPT_BIN_DIR =  CPT_SPACE / 'CPT' / f'{version}'
    CPT_EXECUTABLE = CPT_BIN_DIR / 'CPT.x'
    shutil.copy(ORIGINAL_CPT_TARBALL,  CPT_TARBALL)

    subprocess.call(['tar','xf', f'CPT.{version}.tar.gz' ], cwd=str(CPT_SPACE.absolute()))
    assert CPT_BIN_DIR.is_dir(), 'FAILED TO UNPACK CPT TARBALL'
    print('PyCPT CPT DISTRIBUTION NOT DETECTED - COMPILING CPT FROM SOURCE')
    if version in [ '17.7.0' ]: 
        subprocess.call(['make', 'clean'], cwd=str(CPT_BIN_DIR.absolute() / 'lapack' / 'lapack'))
        subprocess.call(['cp', 'make.inc.example', 'make.inc'], cwd=str(CPT_BIN_DIR.absolute() / 'lapack' / 'lapack'))
    subprocess.call(['make'], cwd=str(CPT_BIN_DIR.absolute()))
    assert CPT_EXECUTABLE.is_file(), 'FAILED TO COMPILE CPT'
    return CPT_EXECUTABLE.parents[0]

def install_cpt_windows(version=CPT_DEFAULT_VERSION):
    assert platform.system() == 'Windows', 'On Unix, you should be using install_cpt_unix'
    assert version in CPT_VALID_VERSIONS, f'PyCPT Requires CPT version to be one of {CPT_VALID_VERSIONS}'
    print('PyCPT CPT DISTRIBUTION NOT DETECTED - COMPILING CPT FROM SOURCE')
    CPT_SPACE = Path().home().absolute() / '.pycpt_worker_space'
    CPT_INSTALLER = Path(__file__).parents[0]/ 'fortran' / f'CPT_batch_installation_{version}.exe'
    #if CPT_SPACE.is_dir():
    #    rmrf(CPT_SPACE)
    CPT_SPACE.mkdir(exist_ok=True, parents=True)
    cptspacevar = str(CPT_SPACE) if str(CPT_SPACE)[:2] != "C:" else str(CPT_SPACE)[2:] 
    print(f'/DIR={cptspacevar}')
    subprocess.call([str(CPT_INSTALLER.absolute()), '/SP-', '/VERYSILENT', '/NOCANCEL', f'/DIR={cptspacevar}'])
    CPT_EXECUTABLE = CPT_SPACE / 'CPT_batch.exe'
    assert CPT_EXECUTABLE.is_file(), 'FAILED TO COMPILE CPT'
    return CPT_EXECUTABLE.parents[0]

def install_cpt_windows2():
    assert platform.system() == 'Windows', 'On Unix, you should be using install_cpt_unix'
    CPT_INSTALLER = Path(str(Path(__file__).parents[0]/ 'fortran' / f'CPT_batch_installation_17.7.4.exe').replace('.egg', ''))
    cptspacevar = str(Path(__file__).parents[0]/ 'fortran' / platform.system()).replace('.egg', '') 
    cptspacevar = str(cptspacevar) if str(cptspacevar)[:2] != "C:" else str(cptspacevar)[2:] 
    subprocess.call([str(CPT_INSTALLER.absolute()), '/SP-', '/VERYSILENT', '/NOCANCEL', f'/DIR={cptspacevar}'])
    CPT_EXECUTABLE = Path(str(Path(__file__).parents[0]/ 'fortran' / platform.system() / 'CPT_batch.exe').replace('.egg', ''))
    assert CPT_EXECUTABLE.is_file(), 'FAILED TO COMPILE CPT'
    return CPT_EXECUTABLE



def find_cpt(version=CPT_DEFAULT_VERSION):
    assert version in CPT_VALID_VERSIONS, f'PyCPT Requires CPT version to be one of {CPT_VALID_VERSIONS}'
    if platform.system() == 'Windows':
        CPT_SPACE = Path().home().absolute() / '.pycpt_worker_space'
        CPT_BIN_DIR =  CPT_SPACE / 'CPT' / f'{version}'
        CPT_EXECUTABLE = CPT_SPACE / 'CPT_batch.exe'
    else: 
        CPT_SPACE = Path().home().absolute() / '.pycpt_worker_space'
        CPT_BIN_DIR =  CPT_SPACE / 'CPT' / f'{version}'
        CPT_EXECUTABLE = CPT_BIN_DIR / 'CPT.x'
    return CPT_EXECUTABLE.parents[0] if CPT_EXECUTABLE.is_file() else False 
