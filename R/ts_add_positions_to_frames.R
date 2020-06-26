#' Add points, coordinates, or polygons to a list of spatial plotss
#'
#' @param r_frame_list 
#' @param positions 
#' @param pcol 
#' @param psize 
#' @param position_legend_title 
#' @param legend_position 
#' @param aes_by_pos vary some aes(linetype for polygons, shape for points)  to make it different for each position? If False, this also disabled the legend, as no notable classes will be plotted. Default is False. 
#' @param position_names 
#'
#' @return
#' @export
ts_add_positions_to_frames <- function(r_frame_list,positions,position_names=NULL,pcol="red",psize=2,position_legend_title = "Position",legend_position="right",aes_by_pos=F){
  
  
  
  if(inherits(positions,"SpatialPolygonsDataFrame")){
    data <-  fortify(positions);data$id <- as.factor(data$id)
    if(is.null(position_names)){
      levels(data$id) <- paste("Polygon" ,(1:nrow(positions)))
    }else{
      levels(data$id) <- as.factor(position_names)
    }
    if(aes_by_pos){
      out <- moveVis::add_gg(r_frame_list, gg = expr(
        list(
          geom_path(aes(x = long, y = lat,group=group,linetype=id), data = data,colour = pcol,size=psize),
          scale_linetype_discrete(name = position_legend_title),
          theme(legend.position = legend_position)
        )
      ), data = data,pcol=pcol,psize=psize,position_legend_title=position_legend_title,legend_position=legend_position)
    }else{
      out <- moveVis::add_gg(r_frame_list, gg = expr(
        list(
          geom_path(aes(x = long, y = lat,group=group), data = data,colour = pcol,size=psize)
        )
      ), data = data,pcol=pcol,psize=psize)
    }
        
    
    
    
    
  }else if(inherits(positions,"SpatialPointsDataFrame")){
    data <-  data.frame(long=positions@coords[,1],lat=positions@coords[,2],group=as.factor(seq(1,nrow(positions))))
    print(data)
    if(is.null(position_names)){
      levels(data$id) <- paste("Point" ,(1:nrow(positions)))
    }else{
      levels(data$id) <- as.factor(position_names)
    }
    if(aes_by_pos){
      out <- moveVis::add_gg(r_frame_list, gg = expr(
        list(
          geom_point(aes(x = long, y = lat,group=group,shape=id), data = data,colour = pcol,size=psize),
          scale_shape_discrete(name = position_legend_title),
          theme(legend.position = legend_position)
        )
      ),data = data,pcol=pcol,psize=psize,position_legend_title=position_legend_title,legend_position=legend_position)     
    }else{
      out <- moveVis::add_gg(r_frame_list, gg = expr(
        list(
          geom_point(aes(x = long, y = lat,group=group), data = data,colour = pcol,size=psize)
        )
      ),data = data,pcol=pcol,psize=psize)
    }

  }else if(inherits(positions,c("matrix","array"))){
    data <- data.frame(long=positions[,1],lat=positions[,2],group=as.factor(seq(1,nrow(positions))))
    if(is.null(position_names)){
      levels(data$group) <- paste("Point" ,(1:nrow(positions)))
    }else{
      levels(data$group) <- as.factor(position_names)
    }
    if(aes_by_pos){
      out  <- moveVis::add_gg(r_frame_list, gg = expr(
        list(
          geom_point(aes(x = long, y = lat,group=group,shape=id), data = data,colour = pcol,size=psize),
          scale_shape_discrete(name = position_legend_title),
          theme(legend.position = legend_position)
        )
      ), data = data,pcol=pcol,psize=psize,position_legend_title=position_legend_title,legend_position=legend_position)
    }else{
      out  <- moveVis::add_gg(r_frame_list, gg = expr(
        list(
          geom_point(aes(x = long, y = lat,group=group), data = data,colour = pcol,size=psize),
          scale_shape_discrete(name = position_legend_title),
          theme(legend.position = legend_position)
        )
      ), data = data,pcol=pcol,psize=psize)
    }
    
  }
}
