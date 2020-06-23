#' Create Spatial Plots of a raster time series
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
ts_makeframes <- function(x_list,r_type = NULL,minq = 0.02,maxq = 0.98,samplesize = 1000,blacken_NA=F,l_indices=NULL){
  
  #If r_type not provided, guess from the first raster object in the list
  if(is.null(r_type)){
    r_type <- .ts_guess_raster_type(x_list[[1]])
  }
  
  #If no layer indices were provided 
  if(is.null(l_indices)){
    print("No layer indices were given. Assuming and selecting layers by r_type.")
    if(r_type == "RGB"){
      l_indices <- c(1:3)
    }else if (r_type == "gradient" | r_type == "discrete"){
      l_indices <- 1
    }
  }
  #Choose the bands by layer indices
  x_list <- .ts_subset_ts_util(x_list,l_indices)
  
  #Performing stretch, but dont quantile stretch discrete rtypes
  if(r_type=="discrete"){
   # ts_quantiles <- ts_get_ts_quantiles(x_list,minq = minq,maxq = maxq,samplesize = samplesize)
    r_list_out_stretched <- ts_stretch_list(x_list = x_list,minq = 0,maxq = 1,samplesize = samplesize)
  }else{
    r_list_out_stretched <- ts_stretch_list(x_list = x_list,minq = minq,maxq = maxq,samplesize = samplesize)
  }
  
  #Replace NA with 0 if necessary (this can prevent error: "Farbintensität nan nicht in [0, 1]")
  if(blacken_NA){
    r_list_out_stretched <- .blacken_NA_util(r_list_out_stretched)
  }
  
  #make the plots
  r_ggplots <- .ts_makeframes(x_list = r_list_out_stretched,r_type = r_type)
}

