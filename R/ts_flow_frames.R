#' Create line charts of a raster time series
#' @param r_list 
#' @param positions 
#' @param position_names 
#' @param val_min 
#' @param val_max 
#' @param val_by 
#' @param path_size 
#' @param path_legend 
#' @param path_legend_title 
#' @return
#' @export




ts_flow_frames <- function(r_list,positions=NULL,position_names=NULL,band_names=NULL,band_colors=NULL,val_min=NULL,val_max=NULL,val_by=0.1,path_size=1,position_legend=T,band_legend=T,band_legend_title="Bands",position_legend_title="Positions"){
  ## extract the values of the raster into a long dataframe
  extract_df <- rtsVis:::.ts_extract_from_frames(r_list_extract = r_list,
                                                positions = positions,
                                                position_names = position_names,
                                                band_names = band_names)
  
  ## create value sequence
  if(is.null(val_min)) val_min <- floor_dec(min(sapply(r_list, minValue), na.rm = T),level = 2)
  if(is.null(val_max)) val_max <- ceiling_dec(max(sapply(r_list, maxValue), na.rm = T),level = 2)
  val_digits <- nchar(strsplit(as.character(val_by), "[.]")[[1]][2])
  if(is.na(val_digits)) val_digits <- 0
  #val_by = NULL
  if(is.null(val_by)){
    #If there are no given vals, just make four
    val_seq <- seq(val_min, val_max, length.out = 4)
  }else{
    #Else make the sequence by the given vals
    val_seq <- seq(val_min, val_max, by = val_by)
  }
  
  flow_frames <- rtsVis:::.ts_gg_flow(pos_df = extract_df,
                                      position_legend = position_legend,
                                      band_legend = band_legend,
                                      band_legend_title = band_legend_title,
                                      position_legend_title = position_legend_title,
                                      path_size =  path_size,
                                      val_seq = val_seq,
                                      band_colors=band_colors)
  return(flow_frames)
}
