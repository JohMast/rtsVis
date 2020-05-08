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

  if(all(!is.list(r_list), inherits(r_list, "Raster"))) r_list <- list(r_list)
  if(length(unique(sapply(r_list, nlayers))) > 1) out("Number of layers per raster object in list 'r' differ.", type = 3)
  if(!inherits(r_times, "POSIXct")) out("Argument 'r_times' must be of type 'POSIXct' if 'r_list' is defined.", type = 3)
  if(!is.logical(fade_raster)) out("Argument 'fade_raster' has to be either TRUE or FALSE.", type = 3)
  if(length(out_times) == 1) out_times <- r_times

  if(identical(r_times, out_times)) return(r_list) else{
    .rFrames(r_list, r_times, 
             data.frame(frame = 1:length(out_times), time = sort(out_times)),
             gg.ext = NA, fade_raster, crop_raster = F)
  }
}