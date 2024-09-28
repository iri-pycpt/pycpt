import cartopy.crs as ccrs
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1.inset_locator import inset_axes
import matplotlib.colors as colors
import xarray as xr
import numpy as np


def view_probabilistic(X, cmap_an=plt.get_cmap('cpt.pr_blue_r', 9), cmap_bn=plt.get_cmap('cpt.pr_red', 9), cmap_nn=plt.get_cmap("cpt.pr_green_r", 4), orientation='horizontal'):
	assert 'T' not in X.coords or X['T'].shape == (), \
		 'view_probabilistic requires you to select across sample dim to eliminate that dimension first'
	assert 'C' in X.coords, 'view_probabilistic requires C to be a coordinate on X'
	assert isinstance(X, xr.DataArray), 'pycpt requires a dataset to be of type "Xarray.DataArray"'

	if orientation=='horizontal':
		fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(7, 9), subplot_kw={'projection': ccrs.PlateCarree()})
	else:
		fig, ax = plt.subplots(nrows=1, ncols=1, figsize=(7, 7), subplot_kw={'projection': ccrs.PlateCarree()})

	#bounds = [40,45,50,55,60,65,70,75,80]
	bounds = [40,50,60,70,80]
	nbounds = [40,45,50]
	mask = X.mean('C')
	mask = mask.where(np.isnan(mask), other=1)
	argmax = X.fillna(-999).argmax('C') * mask

	flat = mask.where(argmax != 2, other=X.isel(C=2))
	flat = flat.where(argmax != 1, other=X.isel(C=1))
	flat = flat.where(argmax != 0, other=X.isel(C=0)) * mask


	if 'station' in X.dims:
		CS3 = ax.scatter(flat['X'], flat['Y'], c=flat.where(argmax == 2, other=np.nan), vmin=0.35, vmax=0.85, cmap=cmap_an)
		CS1 = ax.scatter(flat['X'], flat['Y'], c=flat.where(argmax == 0, other=np.nan), vmin=0.35, vmax=0.85, cmap=cmap_bn)
		CS2 = ax.scatter(flat['X'], flat['Y'], c=flat.where(argmax == 1, other=np.nan), vmin=0.35, vmax=0.55, cmap=cmap_nn)
	else:
		CS3 = flat.where(argmax == 2, other=np.nan).plot(ax=ax, add_colorbar=False, vmin=0.35, vmax=0.85, cmap=cmap_an)
		CS1 = flat.where(argmax == 0, other=np.nan).plot(ax=ax, add_colorbar=False, vmin=0.35, vmax=0.85, cmap=cmap_bn)
		CS2 = flat.where(argmax == 1, other=np.nan).plot(ax=ax, add_colorbar=False, vmin=0.35, vmax=0.55, cmap=cmap_nn)

	ax.coastlines()
	if orientation=='horizontal':
		axins_f_bottom = inset_axes(ax, width="35%", height="5%", loc='lower left', bbox_to_anchor=(-0, -0.15, 1, 1), bbox_transform=ax.transAxes,borderpad=0.1 )
		axins2_bottom = inset_axes(ax, width="20%",  height="5%", loc='lower center', bbox_to_anchor=(-0.0, -0.15, 1, 1), bbox_transform=ax.transAxes, borderpad=0.1 )
		axins3_bottom = inset_axes(ax, width="35%",  height="5%", loc='lower right', bbox_to_anchor=(0, -0.15, 1, 1), bbox_transform=ax.transAxes, borderpad=0.1 )
	else:
		axins_f_bottom = inset_axes(ax, width="10%", height="35%", loc='lower right',bbox_to_anchor=(0.25, 0.03, 1, 1), bbox_transform=ax.transAxes, borderpad=-0.1) #bbox_to_anchor=(0, -0.15, 1, 1) ,bbox_transform=ax.transAxes,
		axins2_bottom = inset_axes(ax, width="10%",  height="20%", loc='center right',bbox_to_anchor=(0.25, -0.0, 1, 1), bbox_transform=ax.transAxes,borderpad=-0.1)
		axins3_bottom = inset_axes(ax, width="10%",  height="35%", loc='upper right',bbox_to_anchor=(0.25, -0.03, 1, 1), bbox_transform=ax.transAxes,borderpad=-0.1)

	#norm = mpl.colors.BoundaryNorm(bounds, CS1.N)
	cbar_fbl = fig.colorbar(CS1, ax=ax, cax=axins_f_bottom, orientation=orientation)
	cbar_fbl.set_label('BN (%)', size=16)
	cbar_fbl.set_ticks([i /100.0 for i in bounds])
	cbar_fbl.set_ticklabels(bounds)
	cbar_fbl.ax.tick_params(labelsize=15)


	cbar_fbc = fig.colorbar(CS2, ax=ax,  cax=axins2_bottom, orientation=orientation)
	cbar_fbc.set_label('NN (%)', size=16)
	cbar_fbc.set_ticks([i /100.0 for i in nbounds])
	cbar_fbc.set_ticklabels(nbounds)
	cbar_fbc.ax.tick_params(labelsize=15)

	cbar_fbr = fig.colorbar(CS3, ax=ax,  cax=axins3_bottom, orientation=orientation)
	cbar_fbr.set_label('AN (%)', size=16)
	cbar_fbr.set_ticks([i /100.0 for i in bounds])
	cbar_fbr.set_ticklabels(bounds)
	cbar_fbr.ax.tick_params(labelsize=15)

	return fig, ax
