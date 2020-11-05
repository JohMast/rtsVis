#' Assemble/interpolate a raster time series
#'
#' This function assembles a raster time series by assigning or interpolating input rasters to a target time series.
#'
#' @param r_list a list of raster objects.
#' @param r_times POSIXct, a vector of times corresponding to the elements of \code{r_list}.
#' @param out_times POSIXct, a vector of times for which output rasters will be created.
#' @param fade_raster (Optional) logical. If \code{TRUE} performs a linear interpolation to calculate the values for the output raster. Otherwise uses a nearest temporal neighbor approach. Default is \code{FALSE}. 
#' @param ... additional arguments.
#' @param verbose  (Optional) logical. If \code{TRUE} outputs progress. Default is \code{TRUE}. 
#' @author Jakob Schwalb-Willmann, Johannes Mast
#' @return a list of raster objects.
#' 
#' 
#' @export
#' @examples 
#' \donttest{
#' #Setup
#'  library(rtsVis)
#' x_list <- MODIS_SI_ds
#' x_dates <- do.call(c, lapply(MODIS_SI_ds,attr,"time") )
#' 
#' #Fill NAs
#' x_list_filled <- ts_fill_na(x_list)
#' 
#' #Make a sequence of output dates, double the length of input dates
#' out_dates <-seq.POSIXt(from = x_dates[1],
#'                        to = x_dates[length(x_dates)],length.out = length(x_dates)*2 )
#' 
#' #For each output date, interpolate a raster image from the input files
#' r_list_out <- ts_raster(r_list = x_list_filled,
#'                         r_times = x_dates,
#'                         out_times = out_dates,
#'                         fade_raster = TRUE)
#'} 
ts_raster <- function(r_list, r_times, out_times = NA, fade_raster = F, ..., verbose = T){
  #2do: take rtype as user input and automatically assign rtype (give warning if auto)
  #2do: take rgb_layers as user input and if more than 3 layers, automatically choose the first 3 as rgb (give warning if auto)
  if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
  if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
  if(!inherits(r_times, "POSIXct")) out("Argument 'r_times' must be of type 'POSIXct' if 'r_list' is defined.", type = 3)
  if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
  if(length(out_times) == 1) out_times <- r_times

  if(identical(r_times, out_times)) return(r_list) else{
    out <- .rFrames(r_list, r_times, 
             data.frame(frame = 1:length(out_times), time = sort(out_times)),
             gg.ext = NA, fade_raster, crop_raster = F)
  }
  #add the out dates as attribute "time"
  .ts_set_frametimes(out,out_times)
  
}