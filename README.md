# `rtsVis`

A lightweight `R` package to visualize large raster time series, building on a fast temporal interpolation core.
rtsVis is linked to the <a href="https://github.com/16EAGLE/moveVis">`moveVis`</a> package and their joint use is recommended.

<img align="right" src="https://github.com/JohMast/rtsVis_demo/blob/main/Images/rtsVis_Logo.png" width="130" height="150" />
## Concepts

rtsVis operates on lists of objects:

- Lists of <a href="https://CRAN.R-project.org/package=raster">`raster`</a> stacks 
- Lists of frames (<a href="https://ggplot2.tidyverse.org/">`ggplot`</a> objects of rasters or charts)

To process those lists in a pipeline, we recommend pipes such as provided by <a href="https://magrittr.tidyverse.org/">`magrittr`</a>.

- Positions of points or polygons, which can be added to the frames or be used to extract values from the time series and create animated charts.


## Functions
### Preparing rasters
* `ts_raster` Assemble/interpolate a raster time series.
* `ts_fill_na` Fill NA values in a raster time series
### Creating Frames
* `ts_flow_frames` 	Create a series of charts of a raster time series.
* `ts_makeframes` 	Create spatial ggplots of a raster time series.
* `ts_add_positions_to_frames` Add points, coordinates, or polygons to a list of spatial plots.

## Example
The creation an animation of NDVI changes in Northern Europe serves as an example of a typical and complete pipeline.

### Step 1: Acquiring the Data
In this step we acquire the data and load it. For this, we use the MODIStsp package. In practice, any data source will do, as long as it comes with sufficient metadata to associate a timestamp to each raster object. Sometimes this must be done manually.

``` r
library(MODIStsp) 
library(raster)
library(tidyverse)
library(rtsVis)

#### Step 1: Acquiring the Data ####
t <- MODIStsp(
  gui = FALSE,
  out_folder = "Beispieldaten/MODIS/NDVI_tsp",out_folder_mod="Beispieldaten/Temp",
  selprod = "Vegetation_Indexes_16Days_1Km (M*D13A2)",
  bandsel = c("NDVI"),
  start_x = 18,end_x = 18,start_y = 3,end_y = 3,
  user = "---Your Username---",
  password = "---Your Password---",
  start_date = "2011.01.01",
  end_date = "2013.12.30",
  verbose = TRUE,
  parallel = TRUE
)

ts <- get(load("PathTo/NDVI/MOD13A2_MYD13A2_NDVI_1_2011_361_2013_RData.RData"))
```

### Step 2: Preprocessing the Data

The goal of this step is to prepare the time series for plotting. Filling NAs is not necessary here.
The median value for every month is calculated.
We use ts_raster to create the raster time series, the first true 'rtsVis' object. Note that 24 frames are created using linear interpolation. The values are, thus, changed, but the final animation will become much smoother.

``` r
#### Step 2: Preprocessing the Data ####

month_median <- stackApply(ts, months(getZ(ts)), fun = median)

in_dates <- as.POSIXct(seq.Date(as.Date("2013-01-01"),
                                as.Date("2013-12-30"),
                                length.out = 12))

out_dates <-seq.POSIXt(from = in_dates[1],
                       to = in_dates[length(in_dates)],
                       length.out = length(in_dates)*2 )

month_median_interpolated <-  ts_raster(r_list = as.list(month_median),
                                        r_times = in_dates,
                                        out_times = out_dates,
                                        fade_raster = T)

```
### Step 3: Creating Basic Frames

In this step, ts_makeframes is used to create a list of frames (ggplot2 objects) from the time series (raster objects).
Using pipes is not necessary, but improves readibility greatly. Here, we also use moveVis functions to easily add map elements. 
Note that ggplot2 functions can be easily applied to the created frames using lapply. 

``` r
dem <- raster("Beispieldaten/Ancillary/SR_LR/SR_LR.tif") 
month_median_interpolated_fr <-  
  ts_makeframes(x_list = month_median_interpolated,
                hillshade = dem,
                r_type = "gradient")

month_median_interpolated_fr_mod <- month_median_interpolated_fr %>% lapply(FUN=function(x){x+
    scale_fill_gradient2("NDVI\n(scaled by 10k)",
                         low = "red",
                         mid="yellow",
                         high = "green4",
                         midpoint=3000,
                         limits=c(-2000,10000),
                         na.value = NA)+
    theme_bw() +
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("NDVI in Northern Europe",
            "Product: MOD13A2 -- Monthly Composite 2011-2013 --   Projection: MODIS Sinusoidal")
  }) %>%
  moveVis::add_northarrow(colour = "black", position = "bottomright") %>% 
  moveVis::add_progress()
``` 
#### Adding positions
We use ts_add_positions_to_frames to add the positions of points to the frames. These positions can be points or polygons and either sf or sp objects.

``` r
# Adding Positions
places_northern_europe <- rgdal::readOGR("Beispieldaten/Ancillary/Northern Europe/Places_Northern_Europe.shp")
month_median_interpolated_fr_mod_pos <- 
  ts_add_positions_to_frames(r_frame_list = month_median_interpolated_fr_mod,
                             positions = places_northern_europe,
                             position_names = places_northern_europe$Name,
                             tcol = "blue4",
                             t_hjust = -30000,
                             tsize = 3,
                             psize=3,
                             pcol="blue4",
                             add_text = T)
```
### Step 4: Creating Flow Frames

To create animated charts, ts_flow_frames can be used. By default, it provides support for line and violin plots, and custom functions are easily implemented.
We use the same positions as before to create a list of line plots, one for each timestep. Around every point position, values in a buffer of 30000m are extracted and averaged.
Note that for this, we pass the raster series, and not the previously created frames.

``` r
month_median_interpolated_lineframes <- 
  ts_flow_frames(r_list = month_median_interpolated,
                 positions = spTransform(places_northern_europe,crs(month_median_interpolated[[1]])),
                 position_names = places_northern_europe$Name,
                 pbuffer = 30000,
                 val_min = -2000,
                 val_max = 10000,
                 legend_position = "right",
                 plot_function = "line_flp",
                 aes_by_pos = TRUE,
                 position_legend = TRUE) %>% 
  lapply(FUN = function(x){
    x+
      ggtitle("NDVI", "In a 30km radius around the places")+
      scale_color_brewer("Places",palette = "Dark2")+
      ylab("Median NDVI")+
      xlab("Month")+
      scale_x_datetime(labels = months)
  })
  
```
### Step 5: Animating the Frames

To join our created plots together and animate them, we again use moveVis functions.

``` r
month_median_joined <- moveVis::join_frames(list(month_median_interpolated_fr_mod_pos,month_median_interpolated_lineframes))
moveVis::animate_frames(month_median_joined,
                        overwrite = TRUE,
                        out_file = "NDVI_Northern_Europe_joined_pause.gif",
                        height=525,
                        width=1200,
                        fps = 12,
                        end_pause = 0.5,
                        res=75)

```
<p align="center"><img src="https://raw.githubusercontent.com/JohMast/rtsVis_demo/main/Images/NDVI_Northern_Europe_joined_pause.gif"></p>

## Demo

For more examples, check out the [Demo](https://github.com/JohMast/rtsVis_demo) repository.

In development, published on CRAN. Last updated: `2021-03-02 17:30:00 CEST`


## Links

To learn more about creating **custom plot types**, access the guide and test material at the associated github [repo](https://github.com/JohMast/rtsVis_demo/).

For creating visualization with movement data, visit [moveVis](http://movevis.org/).

For inspiration, visit the [r-graph-gallery](https://www.r-graph-gallery.com/)!

To learn about data visualization see Claus Wilke's excellent [book](https://clauswilke.com/dataviz/).
