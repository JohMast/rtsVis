#' Fill NA values in a raster time series
#'
#' @param verbose (Optional) logical. If \code{TRUE} outputs progress. Default is \code{FALSE}. 
#' @param ... additional arguments to be passed on to  \link[raster]{approxNA}. Of particular interest is the \code{rule} argument which defines how first and last cells are dealt with. 
#' @param x_list_fill a list of raster objects.
#' @param maskvalues numberi, a vector of values to be set to NA before the masking.
#'
#' @return A list of rasters with NAs filled.
#' @importFrom raster approxNA
#' @author Johannes Mast
#' @details Loads all layers of a specific bands into a stack and uses \link[raster]{approxNA} to fill the NAs if possible. Note that the procedure requires the entire list of raster layery for each band to be be stacked. It is therefore very memory intensive and likely to fail for very large time series.
#' @export
#' @examples 
#' \donttest{
#' 
#' #Setup
#'  library(rtsVis)
#' x_list <- MODIS_SI_ds[seq(1,length(MODIS_SI_ds),15)]   #A list of raster objects
#' 
#' #Fill NAs
#' x_list_filled <- ts_fill_na(x_list) 
#' }

ts_fill_na <- function(x_list_fill,maskvalues=NULL,verbose=FALSE,...){
  rasternames <- names(x_list_fill)
  n_layers <- nlayers(x_list_fill[[1]])
  for(n_l in 1:n_layers){
    if(verbose){
      print(paste0("Filling Layer:",n_l))
    }
    #make a brick from all the layers
    if(n_layers>1){
      x_lay <- stack( .ts_subset_ts_util(x_list_fill,n_l) )
    }else{
      x_lay <- stack(x_list_fill)
    }
    if(!is.null(maskvalues)){
      print(paste0("Filling Mask values: ", c(maskvalues)))
      x_lay[x_lay %in% maskvalues] <- NA
    }
    
    #fill the nas
    x_lay_filled <- raster::approxNA(x_lay)#,...)
    

    
    #reassign the filled layers to the list elements
    for(n_r in 1:length(x_list_fill)){
      if(n_layers>1){
        x_list_fill[[n_r]][[n_l]] <- x_lay_filled[[n_r]]
      }else{
        x_list_fill <- as.list(x_lay_filled)
      }
    }
  }
  #reassign the original names
  names(x_list_fill) <- rasternames
  return(x_list_fill)
}


