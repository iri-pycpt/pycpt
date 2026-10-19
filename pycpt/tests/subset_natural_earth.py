'''Create small subsets of Natural Earth shapefiles for use in tests.

conftest.py points cartopy's data_dir at CARTOPY_DATA_DIR for the
duration of the test run, so that tests don't download shapefiles
from the Natural Earth server. If a new test needs a layer that isn't
there yet, cartopy downloads the full layer into CARTOPY_DATA_DIR and
it shows up in git status. Rather than committing it, add the layer
to LAYERS below and run this script by hand (outside of pytest, so
that it reads from the default cartopy data directory, downloading
if necessary), then track the output in version control.
'''
from pathlib import Path
import shutil

import cartopy.io.shapereader as shpreader
import shapefile  # type: ignore[import-untyped]
import shapely.geometry

CARTOPY_DATA_DIR = Path(__file__).absolute().parent / 'data' / 'cartopy'

# (resolution, category, name)
LAYERS = [
    ('10m', 'physical', 'coastline'),
    ('10m', 'cultural', 'admin_0_boundary_lines_land'),
]

# Extent of the maps plotted by test_automation.py (predictand_extent
# 0..1E, 2..3N), plus a margin.
MARGIN = 5
BOUNDING_BOX = shapely.geometry.box(0 - MARGIN, 2 - MARGIN, 1 + MARGIN, 3 + MARGIN)


def subset_layer(resolution, category, name):
    src = Path(shpreader.natural_earth(resolution=resolution, category=category, name=name))
    dest = CARTOPY_DATA_DIR / 'shapefiles' / 'natural_earth' / category / src.name
    dest.parent.mkdir(parents=True, exist_ok=True)

    with shapefile.Reader(src) as reader, shapefile.Writer(dest, shapeType=reader.shapeType) as writer:
        writer.fields = reader.fields[1:]  # skip DeletionFlag
        for shape_record in reader.iterShapeRecords():
            geom = shapely.geometry.shape(shape_record.shape.__geo_interface__)
            clipped = geom.intersection(BOUNDING_BOX)
            if clipped.is_empty:
                continue
            # Clipping a line can produce a mix of points and lines;
            # keep only the lines.
            lines = [g for g in getattr(clipped, 'geoms', [clipped]) if g.geom_type == 'LineString']
            if not lines:
                continue
            writer.shape(shapely.geometry.MultiLineString(lines).__geo_interface__)
            writer.record(*shape_record.record)

    for suffix in ('.prj', '.cpg'):
        shutil.copy(src.with_suffix(suffix), dest.with_suffix(suffix))
    print(f'wrote {dest}')


if __name__ == '__main__':
    for layer in LAYERS:
        subset_layer(*layer)
