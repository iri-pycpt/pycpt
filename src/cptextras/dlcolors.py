import matplotlib.cm as cm
from matplotlib.colors import LinearSegmentedColormap
import matplotlib.pyplot as plt
#
from typing import Tuple, List, Literal, Optional, Union, Callable, Iterable as Iterable
from typing import NamedTuple
import numpy as np
import collections


def graphorientation(horizontal,vertical):
    if horizontal>=vertical:
        return 'horizontal'
    else:
        return 'vertical'

def standardized_range(min_val, max_val):
    min_val=round(float(min_val),3)
    max_val=round(float(max_val),3)

    max_val=max(abs(min_val), max_val)
    min_val=max_val*-1

    return min_val, max_val

def make_cmap(colors, name, reverse=True):
    colors = [ (colors[i][0] / 255.0, colors[i][1] / 255.0, colors[i][2] / 255.0) for i in range(len(colors))]

    if reverse:
        colors.reverse()
    return LinearSegmentedColormap.from_list( name, colors)

def colormap_info():
    ret = "Colormaps starting with 'cpt.' come from\nthe International Research Institute\nfor Climate & Society's Climate Predictability Tool\n\nColormaps starting with 'pycpt.' come from PyCPT.\n\nColormaps starting with 'cmc.' come\nfrom cmcrameri, all credit to Fabio Crameri\nsee their message:\n\n" + cmc.__doc__ + '\nAll other colormaps come from matplotlib\nPlease Cite Colormaps accordingly!'
    print(ret)
#if user_color != None:
def prepare_canvas(tailoring=None,predictand='PRCP',type=None,user_color=None):
    if tailoring != None and (type==None or type=='deterministic'):
        if tailoring == 'Anomaly' :
            if 'PRCP' in predictand:
                vmin= -200
                vmax=None
                barcolor=cmaps['DL_PRCP_ANOMALY']
            elif any(x in predictand for x in ['TMAX','TMIN','TMEAN','TMED']):
                vmin= -2
                vmax=None
                barcolor=cmaps['DL_TEMP_ANOMALY_COLORS']
            for_title='('+tailoring+')'

        elif tailoring ==  'StdAnomaly':
            for_title='(Standardized Anomaly)'
            if 'PRCP' in predictand:
                vmin= -3
                vmax=None
                barcolor=cmaps['DL_PRCP_ANOMALY']
            elif any(x in predictand for x in ['TMAX','TMIN','TMEAN','TMED']):
                vmin= -2
                vmax=None
                barcolor=cmaps['DL_TEMP_ANOMALY_COLORS']

        elif tailoring ==  'SPI':
            for_title='(Standardized Anomaly)'
            if 'PRCP' in predictand:
                vmin= -3
                vmax=None
                barcolor=cmaps['DL_DM_SPI_2p5_COLORS']
            elif any(x in predictand for x in ['TMAX','TMIN','TMEAN','TMED']):
                vmin= -1.5
                vmax=None
                barcolor=cmaps['DL_TEMP_ANOMALY_COLORS_GCM']

        elif tailoring ==  'POE':
            for_title=''
            if 'PRCP' in predictand:
                vmin= 0
                vmax= 1
                mark='red'
                barcolor=cmaps['DL_RAIN_POE_COLORMAP']
            elif any(x in predictand for x in ['TMAX','TMIN','TMEAN','TMED']):
                vmin= 0
                vmax= 1
                mark='black'
                barcolor=cmaps['DL_TEMP_POE_COLORMAP']

            if not user_color == None:
                barcolor=cmaps[user_color]
            return for_title, vmin, vmax, mark, barcolor
        else:
            for_title='('+tailoring+')'
            if 'PRCP' in predictand:
                vmin=0
                vmax=None
                barcolor=cmaps['DL_RAINFALL_COLORMAP']

        if not user_color == None:
            barcolor=cmaps[user_color]
        return for_title, vmin, vmax, barcolor

    elif type=='probabilistic' :
        if 'PRCP' in predictand:
            cmapB=cmaps['pycpt_probability_red_prec']
            cmapN=cmaps['pycpt_probability_green_prec']
            cmapA=cmaps['pycpt_probability_blue_prec']
        elif any(x in predictand for x in ['TMAX','TMIN','TMEAN','TMED']):
            cmapB=cmaps['pycpt_probability_blue_temp']
            cmapN=cmaps['pycpt_probability_grey_temp']
            cmapA=cmaps['pycpt_probability_red_temp']
        return cmapB, cmapN, cmapA
    else:
        for_title=''
        if 'PRCP' in predictand:
            vmin= 0
            barcolor=cmaps['DL_RAINFALL_COLORMAP']
            vmax=None
        elif any(x in predictand for x in ['TMAX','TMIN','TMEAN','TMED']):
            vmin= -5
            vmax=40
            barcolor=cmaps['DL_TEMP_COLORS']

        if not user_color == None:
            barcolor=cmaps[user_color]
        return for_title, vmin, vmax, barcolor

#
class BGRA(NamedTuple):
    blue: int
    green: int
    red: int
    alpha: int

def to_dash_colorscale(s: str) -> List[str]:
    "Converts an Ingrid colormap to a dash colorscale"
    cm = parse_colormap(s)
    cs = []
    for x in cm:
        v = BGRA(*x)
        cs.append(f"#{v.red:02x}{v.green:02x}{v.blue:02x}{v.alpha:02x}")
    return cs

def parse_color_item(vs: List[BGRA], s: str) -> List[BGRA]:
    if s == "null":
        rs = [BGRA(0, 0, 0, 0)]
    elif s[0] == "[":
        rs = [parse_color(s[1:])]
    elif s[-1] == "]":
        n = int(s[:-1])
        assert 1 < n <= 255 and len(vs) >= 2
        first = vs[-2]
        last = vs[-1]
        vs = vs[:-2]
        rs = [
            BGRA(
                first.blue + (last.blue - first.blue) * i / n,
                first.green + (last.green - first.green) * i / n,
                first.red + (last.red - first.red) * i / n,
                255
            )
            for i in range(n + 1)
        ]
    else:
        rs = [parse_color(s)]
    return vs + rs
def parse_color(s: str) -> BGRA:
    v = int(s, 0)  # 0 tells int() to guess radix
    return BGRA(v >> 16 & 0xFF, v >> 8 & 0xFF, v >> 0 & 0xFF, 255)

def parse_colormap(s: str) -> np.ndarray:
    "Converts an Ingrid colormap to a cv2 colormap"
    vs = []
    for x in s[1:-1].split(" "):
        vs = parse_color_item(vs, x)

    rs = np.array([vs[int(i / 256.0 * len(vs))] for i in range(0, 256)], np.uint8)
    return rs

#DL-Python_Colors
color_arrays = {
    "DL_RAINBOW_COLORMAP" : "[16711680 [16776960 51] [65280 51] [65535 51] [255 51] [16711935 51]]",
    "DL_RAINFALL_COLORMAP" : "[16777215 16777215 14155730 [14155730 21] 12841150 [12841150 23] 10215830 [10215830 23] 7262835 [7262835 22] 6931300 [6931300 23] 5944410 [5944410 23] 5285200 [5285200 23] 4625990 [4625990 22] 3966780 [3966780 23] 3307570 [3307570 23] 2648360 [2648360 23] 1989150]",
    "DL_RAIN_POE_COLORMAP" : "[0 [2763429 38] [42495 39] [65535 39] 11920639 [11920639 24] [3329330 3] [13688896 37] [16711680 38] [15736992 39]]",
    "DL_RAIN_PNE_COLORMAP" : "[15736992 [16711680 38] [13688896 39] [3329330 39] 11920639 [11920639 24] [65535 3] [42495 37] [2763429 38] [0 39]]",
    "DL_TEMP_POE_COLORMAP" : "[15736992 [16711680 38] [16436871 39] [16777200 39] 11920639 [11920639 24] [65535 3] [42495 37] [255 38] [6053069 39] 6053069]",
    "DL_CORRELATION_COLORMAP" : "[8388608 [16711680 25] [16760576 26] [13959039 39] [10025880 26] 11920639 [11920639 26] 65535 [36095 38] [255 38] [128 39]]",
    "DL_PRCP_ANOMALY" : "[5266050 5266050 5266050 [5266050 15] 6581910 [6581910 16] 7897770 [7897770 15] 8555700 [8555700 16] 9213630 [9213630 16] 9871560 [9871560 15] 11516380 [11516380 16] 13817840 [13817840 12] 16777215 [16777215 7] 14155730 [14155730 12] 10217110 [10217110 16] 7590510 [7590510 15] 3322925 [3322925 16] 1681940 [1681940 16] 1021450 [1021450 15] 360960 [360960 16] 290304 [290304 16] 290304]",
    "DL_SSTA_COLORSCALE" : "[8388608 [16711680 66] [16760576 43] [15453831 16] 13959039 [13959039 7] 64636 [65535 4] [36095 24] [255 31] [128 60] [128 7] 2763429]",
    "DL_SST_COLORSCALE" : "[1973790 8388608 [8388608 14] 16711680 [16711680 7] 14772545 [14772545 7] 13458026 [13458026 7] 15624315 [15624315 6] 16740484 [16740484 7] 15570276 [15570276 7] 16748574 [16748574 7] 16760576 [16760576 7] 13749760 [13749760 6] 13688896 [13688896 7] 16776960 [16776960 7] 16777040 [16777040 7] 13959039 [13959039 7] 10025880 [10025880 6] 8388352 [8388352 7] 65407 [65407 7] 3329330 [3329330 7] 65280 [65280 7] 3145645 [3145645 7] 65535 [65535 6] 9234160 [9234160 7] 11200750 [11200750 7] 55295 [55295 7] 3195135 [3195135 7] 42495 [42495 6] 7504122 [7504122 7] 5275647 [5275647 7] 4678655 [4678655 7] 255 [255 7] 2237106 [2237106 6] 2763429 [2763429 7] 1710726 [1710726 7] 128 [128 7] 6303920 [6303920 7] 9639167 [9639167 7] 12632256]",
    "DL_TEMP_COLORS" : "[7208960 8519680 [8519680 15] 11146260 [11146260 16] 12469830 [12469830 15] 15111830 [15111830 16] 15903402 [15903402 16] 16760510 [16760510 15] 16768220 [16768220 16] 16774625 [16774625 16] 14155735 [14155735 15] 11200750 [11200750 16] 5304560 [5304560 15] 65535 [65535 16] 55295 [55295 16] 42495 [42495 15] 4678655 [4678655 16] 255 [255 16] 255]",
    "DL_TEMP_ANOMALY_COLORS" : "[15736992 15736992 [16776960 114] 16777215 [16777215 24] 65535 [255 113] 2237106]",
    "DL_TEMP_ANOMALY_COLORS_GCM" : "[15736992 15736992 [16776960 116] 16777184 [16777184 7] 16777215 [16777215 5] 13499135 [13499135 8] 65535 [255 115] 2237106]",
    "DL_WTs_COLORBAR" : "[8388608 [8388608 21] 13434880 [13434880 17] 16711680 [16711680 16] 16760576 [16760576 17] 15453575 [15453575 17] 15130797 [15130797 17] 16777215 [16777215 41] 13353215 [13353215 17] 11179775 [11179775 17] 9206015 [9206015 17] 255 [255 16] 220 [220 17] 150 [150 21] 128]",
    "DL_NEXTGEN" : "[12632256 12632256 12632256 12632256 42495 [42495 10] 65535 [65535 13] 7405514 [7405514 25] 5950882 [5950882 25] 52582 [52582 50] 2263842 [2263842 125] 25600]",
    "DL_STD_WASP_COLORS" : "[2970272 2970272 2970272 [2970272 17] 5266050 [5266050 18] 6910875 [6910875 18] 8555700 [8555700 18] 10200525 [10200525 18] 16777215 [16777215 71] 14153170 [14153170 18] 10217110 [10217110 18] 3322925 [3322925 18] 1021450 [1021450 18] 360960 [360960 18] 360960]",
    "DL_DM_SPI_2p5_COLORS" : "[115 [115 24] 230 [230 20] 39142 [39142 18] 8377343 [8377343 22] 65535 [65535 16] 16777215 [16777215 53] 7590510 [7590510 16] 1681940 [1681940 22] 360960 [360960 18] 25600 [25600 20] 17920 [17920 23] 17920]",
    "cpt_correlation" : [(238, 43, 51), (255, 57, 67),(253, 123, 91),(248, 175, 123),(254, 214, 158),(252, 239, 188),(255, 254, 241),(244, 255,255),(187, 252, 255),(160, 235, 255),(123, 210, 255),(89, 179, 238),(63, 136, 254),(52, 86, 254)],
    "pycpt_probability_red_temp" : "[13816575 [11184882 9] [7895290 8] [2631935 17] [1250198 17] 1250198 [1250198 23] 1250198]",
    "pycpt_probability_grey_temp" : "[16119285 [0 71]]",
    "pycpt_probability_blue_temp" : "[16445640 [16435889 9] [16425881 8] [16415873 17] [13129045 17] 13129045 [13129045 23]]",
    "pycpt_probability_blue_prec" : "[16777215 [16777215 28] 16776942 [16776942 25] 16644040 [16644040 25] 16574378 [16574378 25] 16437643 [16437643 25] 16363627 [16363627 25] 16222286 [16222286 25] 16145710 [16145710 25] 15543820 [15543820 25] 13573401 [13573401 25] 4163021]",
    "pycpt_probability_red_prec" : "[16777215 [16777215 28] 13434623 [13434623 25] 10546687 [10546687 25] 7788798 [7788798 25] 5026557 [5026557 25] 4034046 [4034046 25] 2772987 [2772987 25] 1842146 [1842146 25] 2490558 [2490558 25] 2490752 [2490752 25] 2490752]",
    "pycpt_probability_green_prec" : "[16777215 [16777215 66] 11001262 [11001262 62] 5545782 [5545782 62] 1852161 [1852161 63] 2490752]",
    "cpt_probability_red" : [(151, 30, 39), (198, 48, 55),(227, 63, 63),(236, 79, 71),(238, 127, 95),(244, 174, 126),(248, 214, 160),(253, 241,194),(255, 255, 235)],
    "cpt_probability_green" : [(208, 238, 203), (178, 208, 173),(148, 178, 143),(118, 148, 113)],
    "cpt_probability_blue" : [(238, 254, 255), (200, 247, 253),(170, 231, 252),(139, 209, 250), (107, 176, 249), (78, 136, 247), (46, 93, 246), (12, 46, 237), (25, 29, 207)],
    "pycpt_probability" : [(146, 179, 249), (194, 212, 251), (231, 237, 254), (254,254,165), (255, 253, 84), (248, 203, 70), (242, 157, 57), (237, 111, 45), (188, 39, 26), (117,21,12)],
    "pycpt_loadings" : [(0, 18, 134), (0, 35, 241), (66, 116,246), (99, 146, 242), (146, 179, 249), (194, 212, 251), (231, 237, 254), (254,254,165), (255, 253, 84), (248, 203, 70), (242, 157, 57), (237, 111, 45), (188, 39, 26), (117,21,12)],
    "pycpt_blue" : [(244, 255,255), (187, 252, 255), (160, 235, 255), (123, 210, 255), (89, 179, 238), (63, 136, 254), (52, 86, 254)],
    "pycpt_roc" : [(5, 48, 97),(7, 52, 102),(10, 58, 111),(12, 62, 117),(15, 69, 126),(18, 73, 132),(21, 79, 141),(24, 86, 149),(26, 90, 155),(30, 96, 164),(32, 100, 170),(36, 106, 174),(40, 111, 176),(43, 115, 178),(47, 120, 181),(49, 124, 183),\
                  (53, 129, 185),(56, 132, 187),(60, 138, 190),(64, 143, 193),(67, 147, 195),(76, 152, 198),(82, 156, 200),(91, 162, 203),(101, 168, 206),(107, 172, 208),(116, 178, 211),(122, 182, 214),(132, 188, 217),(138, 192, 219),(147, 197, 222),(154, 201, 224),\
                  (159, 203,225),(167, 207, 228),(171, 210, 229),(179, 213, 231),(186, 217, 233),(191, 220, 235),(199, 223, 237),(204, 226, 238),(210, 229, 240),(214, 231, 241),(217, 233, 241),(222, 235, 242),(225, 236, 243),(229, 238, 243),(232, 240, 244),\
                  (237, 242, 245),(241, 244, 246),(244, 245, 246),(247, 245, 244),(247, 243, 240),(248, 239, 234),(249, 236, 229),(249, 234, 225),(250, 231, 219),(250, 228, 215),(251, 225, 210),(252, 223, 206),(252, 220, 200),(252, 214, 193),(251, 210, 188),\
                  (250, 204, 180),(249, 199,174),(248, 193, 166),(247, 187, 158),(247, 183, 153),(245, 176, 144),(245, 172, 139),(244, 166, 131),(241, 158, 124),(238, 152, 120),(235, 144, 114),(232, 139, 110),(229, 131, 104),(226, 125, 99),(223, 117, 93),\
                  (219, 109, 87),(217, 104, 83),(214, 96, 77),(211, 90, 74),(206, 81, 70),(202, 73, 66),(199, 67, 63),(195, 59, 59),(192, 53, 56),(188, 45, 52),(185, 39, 50),(181, 31, 46),(176, 23, 42),(170, 21, 41),(161, 18, 40),(155, 16, 39),(147, 14, 38),\
                  (138, 11, 36),(132, 9, 35),(123, 6,34),(117, 4, 33),(108, 1, 31),(103, 0, 31)]
}
#Sort color_arrays for an intuitive display for the users
color_arrays = collections.OrderedDict(sorted(color_arrays.items()))

def make_matlab_cmap(name, colors):
    dash_colorscale = to_dash_colorscale(color_arrays[name])
    return LinearSegmentedColormap.from_list(name, dash_colorscale, N=len(dash_colorscale))


cmaps = {}
for name, colors in color_arrays.items():
    #Use LinearSegmentedColormap for making colorbar
    if type(colors) == type(str()):
        cmaps[name] = make_matlab_cmap(name, colors)
    #Use make_cmap for making colorbar
    elif type(colors) == type(list()):
        if name in ['cpt_probability_green','cpt_probability_blue','pycpt_roc']:
            cmaps[name] = make_cmap(colors, name, False)
        else:
            cmaps[name] = make_cmap(colors, name)



def get_colors_bars(cmaps=cmaps):
    rows=len(cmaps)
    #gradient = np.linspace(50,0)

    fig, axes = plt.subplots(figsize=(12, 6), nrows=rows, ncols=2) #constrained_layout=True

    data = [list(range(0,20))]

    for i, cmap in enumerate(cmaps):

        axes[i, 0].axis('off')
        axes[i, 1].axis('off')
        axes[i, 0].imshow(data, cmap=cmaps[cmap])
        axes[i, 1].text(0, 0.5, cmap, va='center', fontsize=10)
