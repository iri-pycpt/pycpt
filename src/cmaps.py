import matplotlib.cm as cm 
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.pyplot as plt 


def make_cmap(colors, name, reverse=True):
    colors = [ (colors[i][0] / 255.0, colors[i][1] / 255.0, colors[i][2] / 255.0) for i in range(len(colors))]
    if reverse:
        colors.reverse()
    return LinearSegmentedColormap.from_list( name, colors)

def colormap_info():
    ret = "Colormaps starting with 'cpt.' come from\nthe International Research Institute\nfor Climate & Society's Climate Predictability Tool\n\nColormaps starting with 'pycpt.' come from PyCPT.\n\nColormaps starting with 'cmc.' come\nfrom cmcrameri, all credit to Fabio Crameri\nsee their message:\n\n" + cmc.__doc__ + '\nAll other colormaps come from matplotlib\nPlease Cite Colormaps accordingly!'
    print(ret)

correlation = [(238, 43, 51), (255, 57, 67),(253, 123, 91),(248, 175, 123),(254, 214, 158),(252, 239, 188),(255, 254, 241),(244, 255,255),(187, 252, 255),(160, 235, 255),(123, 210, 255),(89, 179, 238),(63, 136, 254),(52, 86, 254)]
probability_red = [(151, 30, 39), (198, 48, 55),(227, 63, 63),(236, 79, 71),(238, 127, 95),(244, 174, 126),(248, 214, 160),(253, 241,194),(255, 255, 235)]
probability_green = [(208, 238, 203), (178, 208, 173),(148, 178, 143),(118, 148, 113)]
probability_blue = [(238, 254, 255), (200, 247, 253),(170, 231, 252),(139, 209, 250), (107, 176, 249), (78, 136, 247), (46, 93, 246), (12, 46, 237), (25, 29, 207)]
probability = [(146, 179, 249), (194, 212, 251), (231, 237, 254), (254,254,165), (255, 253, 84), (248, 203, 70), (242, 157, 57), (237, 111, 45), (188, 39, 26), (117,21,12)]
loadings = [(0, 18, 134), (0, 35, 241), (66, 116,246), (99, 146, 242), (146, 179, 249), (194, 212, 251), (231, 237, 254), (254,254,165), (255, 253, 84), (248, 203, 70), (242, 157, 57), (237, 111, 45), (188, 39, 26), (117,21,12)]
pycpt_blue = [(244, 255,255), (187, 252, 255), (160, 235, 255), (123, 210, 255), (89, 179, 238), (63, 136, 254), (52, 86, 254)]

cmaps = {
    'cpt.correlation': correlation,
    'cpt.pr_red': probability_red, 
    'cpt.pr_green': probability_green,
    'cpt.pr_blue': probability_blue,
    'cpt.probability': probability,
    'cpt.loadings': loadings,
    'pycpt.blue': pycpt_blue
}

for key in cmaps.keys():
    cm.register_cmap(name=key, cmap=make_cmap(cmaps[key], key))
    cm.register_cmap(name=key+'_r', cmap= make_cmap(cmaps[key], key+'_r', reverse=False))




CPT_SKILL_CMAPS = {
    'PEARSON': plt.get_cmap('cpt.correlation', 10), 
    'SPEARMAN': plt.get_cmap('cpt.correlation', 10), 
    '2AFC': plt.get_cmap('cpt.probability', 10),
    'PCT_VAR': plt.get_cmap('cpt.probability', 10),
    'VAR_RATIO': plt.get_cmap('cpt.probability', 10),
    'MEAN_BIAS': plt.get_cmap('cpt.loadings', 10),
    'RMSE': plt.get_cmap('Reds', 10),
    'MAE': plt.get_cmap('Reds', 10),
    'HIT_SCORE': plt.get_cmap('cpt.probability', 10),
    'HIT_SS': plt.get_cmap('cpt.loadings', 10),
    'LEPS': plt.get_cmap('cpt.loadings', 10),
    'GERRITY': plt.get_cmap('cpt.loadings', 10),
    'GROC': plt.get_cmap('cpt.probability', 10),
    'ROC_ABOVE': plt.get_cmap('cpt.probability', 10),
    'ROC_BELOW': plt.get_cmap('cpt.probability', 10),
    '2AFC_CAT':  plt.get_cmap('cpt.probability', 10),
}

CPT_SKILL_LIMS = {
    'PEARSON': (-1, 1), 
    'SPEARMAN': (-1, 1), 
    '2AFC': (0,100),
    'PCT_VAR': (0, 100),
    'VAR_RATIO': (0, 1),
    'MEAN_BIAS': (None, None),
    'RMSE': (None, None),
    'MAE': (None, None),
    'HIT_SCORE': (0, 100),
    'HIT_SS': (-100, 100),
    'LEPS': (-100, 100),
    'GERRITY': (-100, 100),
    'GROC': (0, 100), 
    'ROC_ABOVE': (0, 1), 
    'ROC_BELOW': (0, 1),
    '2AFC_CAT': (0, 100)
}
