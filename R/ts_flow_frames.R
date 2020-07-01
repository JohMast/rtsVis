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




ts_flow_frames <- function(r_list,positions=NULL,position_names=NULL,band_names=NULL,band_colors=NULL,val_min=NULL,val_max=NULL,val_by=0.1,path_size=1,position_legend=T,legend_position="right",band_legend=T,band_legend_title="Bands",position_legend_title="Positions",buffer=0,plot_type="line",aes_by_pos=T,FUN=mean){
  #Check: If a violin Plot is requested, no aggregation function can be used, as the full sample is required
  if(plot_type=="violin"){
    FUN=NULL
  }
  
  #Ensure nice colors
  if(is.null(band_colors)){
    band_colors <-  hcl.colors(nrow(positions))
  }
  #Ensure nice df names
  if(is.null(band_names)){
    band_names <- paste0("Band",1:(nlayers(r_list_extract[[x]])))  #make artificial bandnames if necessary
  }

  
  ## extract the values of the raster into a long dataframe
  extract_df <- rtsVis:::.ts_extract_from_frames(r_list_extract = r_list,
                                                positions = positions,
                                                position_names = position_names,
                                                band_names = band_names,
                                                buffer= buffer,
                                                FUN = FUN)
  
  #For this plot, we need the data in long format
  if(plot_type %in% c("violin","line")){
    extract_df <- extract_df%>% tidyr::pivot_longer(cols =  band_names) 
  }
  #Make a df to match colors to band names
  color_matching_table <- cbind(band_names,band_colors)
  extract_df <- dplyr::left_join(extract_df,color_matching_table,by = c("name" = "band_names"),copy=T)
  
  
  
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
  
  
  if(plot_type=="line"){
    flow_frames <- rtsVis:::.ts_gg_line(pos_df = extract_df,
                                        position_legend = position_legend,
                                        band_legend = band_legend,
                                        band_legend_title = band_legend_title,
                                        position_legend_title = position_legend_title,
                                        legend_position = legend_position,
                                        path_size =  path_size,
                                        val_seq = val_seq,
                                        aes_by_pos=aes_by_pos)
  }else if(plot_type=="violin"){
    flow_frames <- rtsVis:::.ts_gg_vio(pos_df = extract_df,
                                        position_legend = position_legend,
                                        band_legend = band_legend,
                                        band_legend_title = band_legend_title,
                                        position_legend_title = position_legend_title,
                                       legend_position = legend_position,
                                        path_size =  path_size,
                                        val_seq = val_seq,
                                       aes_by_pos=aes_by_pos)
  }

  return(flow_frames)
}
