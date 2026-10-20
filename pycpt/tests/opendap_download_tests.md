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

✅ = already written. ❌ = written, opendap currently fails. 🚫 = can't be
written with SPEAR.PRCP (reason given).

## 1. Specification cases (Ingrid and opendap)

In `download/test_download_spec.py`. All forecast cases so far use
SPEAR.PRCP. Its Ingrid URL concatenates hindcasts and forecasts for both
the hindcast and forecast templates, which is the behavior to preserve.
(CanSIPSIC4's Ingrid entry keeps them separate, which is not.) Requests
always span more than one year, because Ingrid uses the forecast-only
template when first year == final year.

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

SPEAR.PRCP (same in both backends):
- Grid: 1°, cell centers on whole degrees, so cell edges on half degrees.
  `X` 0 to 359 (0–360 convention), `Y` -90 to 90. The seam is the cell
  edge at 359.5 (= -0.5), not 360.
- History: initializations from Jan 1991, monthly, leads 0–11 for every
  initialization. Hindcast to Dec 2020, forecast from Dec 2020 (both
  backends split there; the opendap URLs overlap at Dec 2020).
- The forecast is missing 11 initializations: 2025-06, 2025-08, 2025-10 to
  2025-12, 2026-02, 2026-03, 2026-05, 2026-06, 2026-08, 2026-09 (checked
  2026-10-20). Ingrid's `S` grid is regular and has all-NaN values there;
  the opendap server leaves those values out of `S`. Other cases avoid
  2025–26.

### Forecast: temporal selection
`T` is the midpoint `Ti + (Tf - Ti) / 2`, which falls at noon when `Tf - Ti`
is an odd number of days. Ingrid agrees.
- ✅ Only initializations in the requested month are returned
  (`inits-in-month-years-inclusive`)
- ✅ The range of years is inclusive at both ends (same case)
- ✅ Lead and season length give the right `Ti`, `Tf` and `T` (midpoint)
  (`lead-and-length`: Mar init, lead 4, length 2)
- ✅ Jan initialization, MAM target: the right initialization years
  (`jan-init-mam`)
- ✅ Dec initialization, MAM target (season starts the year after
  initialization): the right initialization years (`dec-init-mam`)
- ✅ A season that spans the new year (e.g. DJF) (`djf`, last season ends
  Feb 29)
- ✅ A request spanning a hindcast/forecast split returns every year exactly
  once (`hindcast-forecast-split`: Dec 2019, 2020, 2021 inits; Dec 2020 is
  in both opendap URLs)
- No incomplete-season cases for forecasts. A forecast season comes from a
  single initialization's leads, and every initialization has all its
  leads, so a missing month means a missing initialization: that year is
  absent, not incomplete.
- ✅ opendap-only: missing initializations are left out, not returned as
  all-NaN years (`missing-initializations`: Jun 2024–2026, only 2024
  returned). Ingrid can't take part because it fills them with NaN. Will
  break on re-recording if the missing data is filled in on the server, so
  the behavior is also a synthetic test (section 2, forecast only).
- ✅ opendap-only: if no data falls in the requested range, the result has
  length 0 in `T` (`no-data-in-range`, 1980–1982). Confirmed that Ingrid
  returns a 404 here.

### Obs: temporal selection
🚫 for all of these: SPEAR is a forecast dataset. UCSB.PRCP is the only obs
dataset in the opendap catalog, and it's daily, which section 2 says to skip
for now.
- The right seasons are returned for the range of season start years,
  inclusive at both ends
- `Ti`, `Tf` and `T` (midpoint) are correct
- A season that spans the new year (e.g. DJF)
- Incomplete seasons at the beginning of the history are removed (opendap
  will currently fail). UCSB.PRCP starts 1981-01-01, so e.g. DJF 1980–81
  is incomplete.
- Incomplete seasons in the middle of the history: no obs dataset in the
  opendap catalog has a gap, so this is a synthetic test (section 2, obs
  only).

### Spatial: alignment of the bounding box with the grid
Cells that overlap the box are returned (legacy `RANGEEDGES` behavior).
Ingrid confirms that cells which only touch the box along an edge are not
returned, and that a zero-area box returns the cell containing it.
- No bounding box: the full grid is returned. Deferred to the synthetic
  move: the opendap recording for SPEAR's full grid would be ~8 MB even for
  one month and two years. Expect each cell once (360 longitudes for
  SPEAR). Opendap-only: Ingrid substitutes a 0–360, -90–90 box, so it would
  presumably return 361 longitudes like `all-longitudes-0-360` (not run).
- ✅ Box edges fall on cell edges, including cells that only touch the box
  along an edge (`edges-on-cell-edges`)
- ✅ Box edges fall on cell centers (`edges-on-cell-centers`)
- ✅ Box edges fall strictly inside cells, off center
  (`edges-inside-cells-off-center`)
- ✅ Box edges are aligned in one dimension and not the other
  (`aligned-in-x-not-y`)
- ✅ Box is smaller than one cell and inside it (`smaller-than-one-cell`)
- ✅ Box has zero width or height (west == east or south == north)
  (`zero-width-and-height`, both at once)
- ✅ Box extends past the grid's latitude range (`past-latitude-range`)
- ✅ A grid with cell centers on whole degrees, if a dataset has one (SPEAR:
  every spatial case)
- 🚫 A grid whose cells are not 1° (e.g. 0.25° CHIRPS), if a dataset has one.
  SPEAR is 1°.

### Spatial: longitude seam
- ✅ Box in the other longitude convention from the data (e.g. data 0–360, box
  -20 to -10); `X` follows the box's convention (`other-convention`)
- ✅ The same, with the box's west edge inside a cell east of its center, so
  that cell's center is west of the box
  (`other-convention-edges-inside-cells`)
- ✅ Box edge falls exactly on the seam. SPEAR's seam is at 359.5, so:
  `edge-on-seam-0-360` (349.5 to 359.5) and `edge-on-seam-negative` (-10.5
  to -0.5).
- ✅ The plan's examples, box 350 to 360 or -10 to 0, overlap SPEAR's cell
  centered on 0, so for SPEAR they span the seam: `east-edge-360` (`X`
  350..360) and `east-edge-0` (`X` -10..0).
- ✅ Box spans the seam with edges on cell edges
  (`spans-seam-edges-on-cell-edges`)
- ✅ Box spans the seam with edges inside cells
  (`spans-seam-edges-inside-cells`)
- ✅ Box spans all 360° of longitude (`all-longitudes-0-360`,
  `all-longitudes-negative`). The cell at the box's east edge is also the
  one at its west edge, and it's returned at both ends: 361 points (0..360,
  or -180..180), as Ingrid does. That's acceptable for a box spanning the
  globe; a caller who wants the whole globe leaves the box None.
- Box wider than 360°: not handled (the piece east of the seam would reach
  past the seam again). Decide whether to raise an error; no case yet.
- 🚫 At least one dataset of each longitude convention (0–360 and -180–180), if
  both exist. SPEAR is 0–360. Look for a -180–180 dataset among the other
  twins.

### Open questions
None at the moment.

Answered: does any dataset in the opendap catalog have a gap in its history?
Checked 2026-10-20, one variable per model. UCSB.PRCP is daily with no gaps
(1981-01-01 to 2026-08-31). Among forecasts, only SPEAR's forecast URL has
gaps (missing initializations, see Datasets); CCSM4, CESM1, CFSv2,
CanSIPSIC4, GEOSS2S and SEAS51c have none. For forecasts a gap means a
missing year, not an incomplete season (missing years are left out; see
`missing-initializations`), so the middle-of-history incomplete-season case
is an obs synthetic test.

## 2. Synthetic tests (opendap only)

### Both sources (parametrized over forecast and obs)
- ✅ Extensive variables are summed over the season
- ✅ Intensive variables are averaged over the season, weighted by days in the month
- ✅ A NaN in any month of the season makes the seasonal value NaN
- ✅ Output has a `missing` attribute
- ✅ Data whose `X` is not monotonic increasing raises an error
- ✅ A bounding box whose west edge is greater than its east edge raises an
  error (replaces the `normalize_bounding_box` test of the same)

### Forecast only
- ✅ Ensemble members are averaged
- ✅ Only leads `season_start_lead` through `season_start_lead + season_length - 1` are used
- ✅ Where URLs overlap in `S`, the later URL's values are used
- ✅ Non-overlapping URLs are concatenated in order
- ✅ Initializations missing from `S` (a gap in the history) are left out of the
  result, not returned as all-NaN years. Synthetic as well as the
  `missing-initializations` specification case, because that case depends on
  the server's data staying incomplete.
- ✅ URLs with different ensemble sizes are combined correctly
- ✅ When a pressure level is given and the data has `P`, that level is selected
- ✅ When a pressure level is given and the data has no `P`, it is ignored (backwards compatibility)

### Obs only
- ❌ Incomplete seasons at the end of the history are removed. Synthetic
  because a real case needs recent data, and no discontinued datasets will
  be added to the opendap catalog. (Not for forecasts: every initialization
  has all its leads, so a forecast season is never incomplete.)
- ❌ Incomplete seasons in the middle of the history are removed. Synthetic
  because no obs dataset in the opendap catalog has a gap.
- ✅ Months outside the target season are not used
- ✅ When a pressure level is given, that level is selected
- ✅ When a pressure level is given and the data has no `P`, an error is raised
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

Add the "no bounding box: full grid" case at that point (deferred because
its recording would be large).
