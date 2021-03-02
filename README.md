# `rtsVis`

A lightweight `R` package to visualize large raster time series, building on a fast temporal interpolation core.
<img align="right" src="https://github.com/JohMast/rtsVis_demo/blob/main/Images/rtsVis_Logo.png" width="130" height="150" />
## Concepts
rtsVis operates on lists of objects:

- Lists of raster stacks 
- Lists of frames (ggplot objects of rasters)


## Get started

To create a time-series of the monthly NDVI changes in Northern Europe.

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

ts <- get(load("Beispieldaten/MODIS/NDVI_tsp/VI_16Days_1Km_v6/Time_Series/RData/Mixed/NDVI/MOD13A2_MYD13A2_NDVI_1_2011_361_2013_RData.RData"))

#### Step 2: Preprocessing the Data ####

month_median <- stackApply(ts, months(getZ(ts)), fun = median)
month_avg_agg <- raster::aggregate(month_avg,fact=10)

# alter replace month_avg_agg with month_avg
in_dates <- as.POSIXct(seq.Date(as.Date("2013-01-01"),as.Date("2013-12-30"),length.out = 12))

out_dates <-seq.POSIXt(from = in_dates[1],
                       to = in_dates[length(in_dates)],
                       length.out = length(in_dates)*2 )

month_median_interpolated <-  ts_raster(r_list = as.list(month_median),
                                        r_times = in_dates,
                                        out_times = out_dates,
                                        fade_raster = T)



#### Step 3: Creating Basic Frames ####
dem <- raster("Beispieldaten/Ancillary/SR_LR/SR_LR.tif") 
month_median_interpolated_fr <-  ts_makeframes(x_list = month_median_interpolated,hillshade = dem,r_type = "gradient")

month_median_interpolated_fr_mod <- month_median_interpolated_fr %>% lapply(FUN=function(x){x+
    #scale_fill_distiller("NDVI Northern Europe",palette="BuPu",direction = 0,limits=c(-10000,10000))+
    scale_fill_gradient2("NDVI\n(scaled by 10k)",low = "red",mid="yellow",high = "green4",midpoint=3000,limits=c(-2000,10000),na.value = NA)+
    theme_bw() +
    xlab("Longitude")+
    ylab("Latitude")+
    ggtitle("NDVI in Northern Europe", "Product: MOD13A2 -- Monthly Composite 2011-2013 --   Projection: MODIS Sinusoidal")
}) %>%
  moveVis::add_northarrow(colour = "black", position = "bottomright") %>% 
  moveVis::add_progress()

######With Points###########
places_northern_europe <- rgdal::readOGR("Beispieldaten/Ancillary/Northern Europe/Places_Northern_Europe.shp")
month_median_interpolated_fr_mod_pos <- ts_add_positions_to_frames(r_frame_list = month_median_interpolated_fr_mod,
                                                                   positions = places_northern_europe,
                                                                   position_names = places_northern_europe$Name,
                                                                   tcol = "blue4",t_hjust = -30000,
                                                                   tsize = 3,psize=3,pcol="blue4",
                                                                   add_text = T)
moveVis::animate_frames(month_median_interpolated_fr_mod_pos,overwrite = TRUE,out_file = "NDVI_Germany_Interpolated.gif",height=700,width=800,fps = 12)


#### Step 4: Creating Flow Frames ####

month_median_interpolated_lineframes <- ts_flow_frames(r_list = month_median_interpolated,
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

#### Step 5: Animating the Frames ####
month_median_joined <- moveVis::join_frames(list(month_median_interpolated_fr_mod_pos,month_median_interpolated_lineframes))
moveVis::animate_frames(month_median_joined,overwrite = TRUE,out_file = "NDVI_Northern_Europe_joined_pause.gif",height=525,width=1200,fps = 12,end_pause = 0.5,res=75)
moveVis::animate_frames(month_median_joined,overwrite = TRUE,out_file = "NDVI_Northern_Europe_joined.gif",height=525,width=1200,fps = 10,res=75)

```
<p align="center"><img width="50%" height="50%" src="https://raw.githubusercontent.com/JohMast/rtsVis_demo/main/Images/NDVI_Northern_Europe_joined_pause.gif
"></p>
## Demo

For more examples, check out the [Demo](https://github.com/JohMast/rtsVis_demo).

In development, published on CRAN. Last updated: `2021-03-02 17:30:00 CEST`
