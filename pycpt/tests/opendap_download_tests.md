# Test plan: `download()` in `opendap.py`

All tests go through the Source classes' public interface (`download()`).
There are two kinds:

1. **Specification cases**: requests with explicit expected results, run
   against both the Ingrid and the opendap implementations through the
   recording proxy. Ingrid is the reference for legacy behavior: when its
   cases pass, the specification is right.
2. **Synthetic tests**: opendap only, with a fake `opener`. For behavior
   that is specific to the opendap implementation or that real data can't
   exercise.

✅ = already written.

## 1. Specification cases (Ingrid and opendap)

### How they work
- A table of cases. Each has a source name, a request, and the expected
  coordinates (`S`, `T`, `Ti`, `Tf`, `Y`, `X` as relevant), written out
  explicitly rather than as rules.
- Parametrized over backend (`ingrid`, `opendap`), using the Ingrid and
  opendap sources of the same name.
- Every case also checks the output format: dims are `('T', 'Y', 'X')` and
  `Y` is decreasing.
- Values of the data variables are not checked (covered in another repo).
- Request dates stay a couple of months in the past, because the two servers
  sync on slightly different schedules.
- Cases Ingrid can't take part in (e.g. it returns a 404 for a zero-size
  selection) are marked opendap-only.
- Once the Ingrid cases all pass and the behavior is fully characterized, the
  Ingrid backend is turned off (marker or option), not deleted. The opendap
  cases keep running.

### Datasets
Ingrid twins exist for CanSIPSIC4, SPEAR, GEOSS2S, CFSv2 and CCSM4 (PRCP and
SST) and CESM1.PRCP. SEAS51c and UCSB.PRCP not checked yet. Before writing
the spatial cases, look up each dataset's grid: where the cell edges are,
the longitude range, and the seam.

### Forecast: temporal selection
- Only initializations in the requested month are returned
- The range of years is inclusive at both ends
- Lead and season length give the right `Ti`, `Tf` and `T` (midpoint)
- Jan initialization, MAM target: the right initialization years
- Dec initialization, MAM target (season starts the year after
  initialization): the right initialization years
- A season that spans the new year (e.g. DJF)
- A request spanning a hindcast/forecast split returns every year exactly once
- Incomplete seasons at the beginning of the history are removed (opendap
  will currently fail)
- Incomplete seasons in the middle of the history are removed (opendap will
  currently fail), if a dataset in the opendap catalog has a gap
- opendap-only: if no data falls in the requested range, the result has
  length 0 in `T` (will currently fail)

### Obs: temporal selection
- The right seasons are returned for the range of season start years,
  inclusive at both ends
- `Ti`, `Tf` and `T` (midpoint) are correct
- A season that spans the new year (e.g. DJF)
- Incomplete seasons at the beginning of the history are removed (opendap
  will currently fail)
- Incomplete seasons in the middle of the history are removed (opendap will
  currently fail), if a dataset in the opendap catalog has a gap

### Spatial: alignment of the bounding box with the grid
Cells that overlap the box are returned (legacy `RANGEEDGES` behavior).
- No bounding box: the full grid is returned
- Box edges fall on cell edges, including cells that only touch the box along an edge
- Box edges fall on cell centers
- Box edges fall strictly inside cells, off center
- Box edges are aligned in one dimension and not the other
- Box is smaller than one cell and inside it
- Box has zero width or height (west == east or south == north)
- Box extends past the grid's latitude range
- A grid with cell centers on whole degrees, if a dataset has one
- A grid whose cells are not 1° (e.g. 0.25° CHIRPS), if a dataset has one

### Spatial: longitude seam
- Box in the other longitude convention from the data (e.g. data 0–360, box
  -20 to -10); `X` follows the box's convention
- Box edge falls exactly on the seam (e.g. data 0–360, box 350 to 360 or -10 to 0)
- Box spans the seam with edges on cell edges
- Box spans the seam with edges inside cells
- Box spans all 360° of longitude
- At least one dataset of each longitude convention (0–360 and -180–180), if both exist

### Open questions
- Does any dataset in the opendap catalog have a gap in its history? Some
  legacy datasets do. If none of them is in the opendap catalog, the
  middle-of-history incomplete-season cases become synthetic tests.

## 2. Synthetic tests (opendap only)

### Both sources (parametrized over forecast and obs)
- ✅ (forecast so far) Extensive variables are summed over the season
- Intensive variables are averaged over the season, weighted by days in the month
- A NaN in any month of the season makes the seasonal value NaN
- Incomplete seasons at the end of the history are removed (will currently
  fail). Synthetic because a real case needs recent data, and no discontinued
  datasets will be added to the opendap catalog.
- Output has a `missing` attribute
- Data whose `X` is not monotonic increasing raises an error

### Forecast only
- ✅ Ensemble members are averaged
- Only leads `season_start_lead` through `season_start_lead + season_length - 1` are used
- ✅ Where URLs overlap in `S`, the later URL's values are used
- Non-overlapping URLs are concatenated in order
- URLs with different ensemble sizes are combined correctly
- When a pressure level is given and the data has `P`, that level is selected
- When a pressure level is given and the data has no `P`, it is ignored (backwards compatibility)

### Obs only
- Months outside the target season are not used
- When a pressure level is given, that level is selected
- When a pressure level is given and the data has no `P`, an error is raised
- Daily data: skip for now. We might end up deleting CHIRPSv2 daily and I
  don't want to waste time on it.

### Spatial, if at all
No real dataset is known to have these. Keep only if they're worth it.
- Box lies entirely outside a regional grid
- Box partly overlaps a regional grid
- Data with an explicit `X` bounds variable: cell edges come from it, not from the centers

## Later: move specification cases to synthetic data
Once the Ingrid backend is turned off, convert the specification cases to
synthetic tests. Each keeps its request and assertions; the opener returns a
synthetic dataset on the real dataset's grid. This removes the need to
re-record when a refactor changes the DAP requests `opendap.py` makes.

## Cleanup
- `test_forecast_download_target_season_coords` (✅) is superseded by the
  specification cases; delete it once they exist.
- Once the tests above exist, delete the subroutine tests
  `test_forecast_sum`, `test_forecast_mean`, `test_obs_sum`, `test_obs_mean`
  and the `normalize_bounding_box` tests.
