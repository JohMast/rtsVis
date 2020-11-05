#' Example MODIS time series for southern Slovenia.
#'
#' 105 MODIS MCD43A4 Images covering the time from 2020-02-15 to 2020-05-29 ,
#' downsampled by a factor of 4 and subset to bands 1 to 4
#' to serve as lightweight example time series.
#' Acquired from LP DAAC using the getSpatialData package.
#'
#' @format A list of 105 raster objects
#' @source \url{https://lpdaac.usgs.gov/tools/usgs-earthexplorer/}
"MODIS_SI_ds"



#' Example vector data for Slovenia.
#'
#' Three sets of example vector data for Slovenia. Two sp objects and one matrix object. Chosen to match
#' the "MODIS_SI_ds" raster data.
#' @format A list of three sets of example vector data:
#'  \describe{
#'   \item{points_matrix}{A matrix of coordinates corresponding to the coordinates of two points in Slovenia. Manually created. MODIS sinusoidal projection.}
#'   \item{points}{SpatialPointsDataFrame of four points in Slovenian municipalities. Manually created. MODIS sinusoidal projection.}
#'   \item{polygons}{SpatialPolygonsDataFrame of three polygons corresponding to borders of municipalities. Acquired from GADM. MODIS sinusoidal projection.}
#'   ...
#' }
#' @source \url{https://gadm.org/data.html}
"SI_positions"