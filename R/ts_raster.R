#' Assemble/interpolate a raster time series
#'
#' This function assembles a raster time series by assigning or interpolating input rasters to a target time series.
#'
#' @param r_list list of rasters
#' @param r_times POSIXct, 
#' @param out_times POSIXct, 
#' @param fade_raster logical,
#' @param ... additional arguments.
#' @param verbose
#' 
#' @return
#' 
#' 
#' @export
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