import cartopy.crs as ccrs
import matplotlib.pyplot as plt 
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
import matplotlib.colors as colors
import xarray as xr
import xcast as xc 
import numpy as np 

def guess_coords_view_prob(X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None):
	assert type(X) == xr.DataArray, 'X must be a data array'
	common_x = ['LONGITUDE', 'LONG', 'X', 'LON']
	common_y = ['LATITUDE', 'LAT', 'LATI', 'Y']
	common_t = ['T', 'S', 'TIME', 'SAMPLES', 'SAMPLE', 'INITIALIZATION', 'INIT', "TARGET"]
	common_m = ['M', 'FEATURES', 'F', 'REALIZATION', 'MEMBER', 'Z', 'C', 'CAT']
	ret = {'lat': x_lat_dim, 'lon': x_lon_dim, 'samp': x_sample_dim, 'feat': x_feature_dim}
	for dim in X.dims: 
		for x in common_x: 
			if x in dim.upper() and ret['lon'] is None:
				ret['lon'] = dim 
		for y in common_y: 
			if y in dim.upper() and ret['lat'] is None: 
				ret['lat'] = dim 
		for t in common_t:
			if t in dim.upper() and ret['samp'] is None: 
				ret['samp'] = dim 
		for m in common_m:
			if m in dim.upper() and ret['feat'] is None:
				ret['feat'] = dim 
	#assert None not in ret.values(), 'Could not detect one or more dimensions: \n  LATITUDE: {lat}\n  LONGITUDE: {lon}\n  SAMPLE: {samp}\n  FEATURE: {feat}\n'.format(**ret)
	vals = []
	for val in ret.values():
		if val not in vals: 
			vals.append(val)
	#assert len(vals) == 4, 'Detection Faild - Duplicated Coordinate: \n  LATITUDE: {lat}\n  LONGITUDE: {lon}\n  SAMPLE: {samp}\n  FEATURE: {feat}\n'.format(**ret)
	return ret['lat'], ret['lon'], ret['samp'], ret['feat']


def view_probabilistic(X, x_lat_dim=None, x_lon_dim=None, x_sample_dim=None, x_feature_dim=None, cmap_an=plt.get_cmap('cpt.pr_blue_r', 9), cmap_bn=plt.get_cmap('cpt.pr_red', 9), cmap_nn=plt.get_cmap("cpt.pr_green_r", 4)):
	x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim = guess_coords_view_prob(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	assert x_sample_dim is None, 'View probabilistic requires you to select across sample dim to eliminate that dimension first'
	#check_all(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)
	assert x_lat_dim in X.coords.keys(), 'XCast requires a dataset_lat_dim to be a coordinate on X'
	assert x_lon_dim in X.coords.keys(), 'XCast requires a dataset_lon_dim to be a coordinate on X'
	assert x_feature_dim in X.coords.keys(), 'XCast requires a dataset_feature_dim to be a coordinate on X'
	xc.check_type(X, x_lat_dim, x_lon_dim, x_sample_dim, x_feature_dim)

	fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(7, 9), subplot_kw={'projection': ccrs.PlateCarree()})
	bounds = [40,45,50,55,60,65,70,75,80]
	nbounds = [40,45,50]
	mask = X.mean(x_feature_dim)
	mask = mask.where(np.isnan(mask), other=1)
	argmax = X.fillna(-999).argmax('M') * mask

	flat = mask.where(argmax != 2, other=X.isel(M=2))
	flat = flat.where(argmax != 1, other=X.isel(M=1))
	flat = flat.where(argmax != 0, other=X.isel(M=0)) * mask 

	CS3 = flat.where(argmax == 2, other=np.nan).plot(ax=ax, add_colorbar=False, vmin=0.35, vmax=0.85, cmap=cmap_an)
	CS1 = flat.where(argmax == 0, other=np.nan).plot(ax=ax, add_colorbar=False, vmin=0.35, vmax=0.85, cmap=cmap_bn)
	CS2 = flat.where(argmax == 1, other=np.nan).plot(ax=ax, add_colorbar=False, vmin=0.35, vmax=0.55, cmap=cmap_nn)

	ax.coastlines()
	axins_f_bottom = inset_axes(ax, width="35%", height="5%", loc='lower left', bbox_to_anchor=(-0, -0.15, 1, 1), bbox_transform=ax.transAxes,borderpad=0.1 )
	axins2_bottom = inset_axes(ax, width="20%",  height="5%", loc='lower center', bbox_to_anchor=(-0.0, -0.15, 1, 1), bbox_transform=ax.transAxes, borderpad=0.1 )
	axins3_bottom = inset_axes(ax, width="35%",  height="5%", loc='lower right', bbox_to_anchor=(0, -0.15, 1, 1), bbox_transform=ax.transAxes, borderpad=0.1 )


	cbar_fbl = fig.colorbar(CS1, ax=ax, cax=axins_f_bottom, orientation='horizontal')
	cbar_fbl.set_label('BN (%)') 
	cbar_fbl.set_ticks([i /100.0 for i in bounds])
	cbar_fbl.set_ticklabels(bounds)


	cbar_fbc = fig.colorbar(CS2, ax=ax,  cax=axins2_bottom, orientation='horizontal')
	cbar_fbc.set_label('NN (%)') 
	cbar_fbc.set_ticks([i /100.0 for i in nbounds])
	cbar_fbc.set_ticklabels(nbounds)

	cbar_fbr = fig.colorbar(CS3, ax=ax,  cax=axins3_bottom, orientation='horizontal')
	cbar_fbr.set_label('AN (%)') 
	cbar_fbr.set_ticks([i /100.0 for i in bounds])
	cbar_fbr.set_ticklabels(bounds)

	return fig, ax
