# `rtsVis`

A lightweight `R` package to visualize large raster time series, building on a fast temporal interpolation core.

## Concepts
rtsVis operates on lists of objects:

- Lists of raster stacks 
- Lists of frames (ggplot objects of rasters)


## Get started

``` r
library(rtsVis)

# example raster data
data("basemap_data", package = "moveVis")
r_list <- basemap_data[[1]] # a list of rasters ...
r_times <- basemap_data[[2]] # ... and their timestamps

# target temporal resolution
out_times <- seq.POSIXt(min(r_times), max(r_times), length.out = 50)

# assign rasters by "nearest-neighbour" in time
r_nn <- ts_raster(r_list, r_times, out_times, fade_raster = F)

# apply linear interpolation over time
r_int <- ts_raster(r_list, r_times, out_times, fade_raster = T)

# advanced: use options to control computation
# multi-core for interpolation for very large rasters:
options(rtsVis.n_cores = 7)
r_int <- ts_raster(r_list, r_times, out_times, fade_raster = T)
options(rtsVis.n_cores = 1)

# activate memory releave to disk to avoid memorey overload:
options(rtsVis.frames_to_disk = T) 
# set maxmimum number of rasters to be hold in memory or NA for auto:
options(rtsVis.n_memory_frames = NA)
r_int <- ts_raster(r_list, r_times, out_times, fade_raster = T)
```

## Demo

For more elaborate examples, check out the [Demo](https://github.com/JohMast/rtsVis_demo).

In development, not public yet. Last updated: `2021-02-18 17:30:00 CEST`
