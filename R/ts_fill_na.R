#' Fill NA values in a raster time series
#'
#' @param x_list_fill 
#'
#' @return
#' @export
#'
#' @examples
ts_fill_na <- function(x_list_fill){
  for(n_l in 1:nlayers(x_list_fill[[1]])){
    print(paste0("Filling Layer:",n_l))
    #make a brick from all the layers
    x_lay <- rtsVis:::.ts_subset_ts_util(x_list_fill,n_l) %>% stack()
    #fill the nas
    x_lay_filled <- approxNA(x_lay)
    #reassign the filled layers to the list elements
    for(n_r in 1:length(x_list_fill)){
      x_list_fill[[n_r]][[n_l]] <- x_lay_filled[[n_r]]
    }
  }
  return(x_list_fill)
}


