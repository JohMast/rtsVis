#' ts_makeframes
#'
#' @param x_list A list of rasters.
#' @param r_type one of "discrete","gradient", "RGB"
#' @param minq Lower quantile boundary for the stretch
#' @param maxq Upper quantile boundary for the stretch
#' @param samplesize Number of samples to determine the quantile from
#'
#' @return A list of ggplots
#' @export
#' @importFrom raster compareCRS nlayers
ts_makeframes <- function(x_list,r_type = "RGB",minq = 0.02,maxq = 0.98,samplesize = 1000){
  #2do: dont stretch discrete rtypes
  ts_quantiles <- rtsVis:::ts_get_ts_quantiles(r_list_out,minq = minq,maxq = maxq,samplesize = samplesize)
  r_list_out_stretched <- rtsVis:::ts_stretch_list(x_list = r_list_out)
  r_ggplots <- rtsVis:::.ts_makeframes(x_list = r_list_out_stretched,r_type = r_type)
}
