#' Suppress messages and warnings
#' @noRd 
quiet <- function(expr){
  #return(expr)
  return(suppressWarnings(suppressMessages(expr)))
}

#' Outputs errors, warnings and messages
#'
#' @param input character
#' @param type numeric, 1 = message/cat, 2 = warning, 3 = error and stop
#' @param msg logical. If \code{TRUE}, \code{message} is used instead of \code{cat}. Default is \code{FALSE}.
#' @param sign character. Defines the prefix string.
#'
#' @keywords internal
#' @noRd

out <- function(input, type = 1, ll = NULL, msg = FALSE, sign = "", verbose = getOption("rtsVis.verbose")){
  if(is.null(ll)) if(isTRUE(verbose)) ll <- 1 else ll <- 2
  if(type == 2 & ll <= 2){warning(paste0(sign,input), call. = FALSE, immediate. = TRUE)}
  else{if(type == 3){stop(input, call. = FALSE)}else{if(ll == 1){
    if(msg == FALSE){ cat(paste0(sign,input),sep="\n")
    } else{message(paste0(sign,input))}}}}
}

#' interpolate NAs
#'
#' @param v vector with NAs to be replaced by interpolated values
#' @param rule see approxfun
#'
#' @keywords internal
#' @noRd
.na.approx <- function(v, rule = 2){
  if(length(which(!is.na(v))) < 2) return(v) else{
    s <- 1:length(v)
    stats::approx(x = s[!is.na(v)], y = v[!is.na(v)], rule = rule, xout = s)$y 
  }
}

#' create interpolated layer by frame position
#' @importFrom raster clusterR overlay brick unstack stack
#' @noRd
.int2frames <- function(r_list, pos, frames, n.rlay, cl){
  
  # get frames outside shoulders not to be interpolated
  r.frames <- rep(list(NULL), length(frames))
  names(r.frames) <- frames
  early <- as.numeric(names(r.frames)) < utils::head(pos, n=1)
  if(any(early)) r.frames[early] <- utils::head(r_list, n=1)
  
  late <- as.numeric(names(r.frames)) > utils::tail(pos, n=1)
  if(any(late)) r.frames[late] <- utils::tail(r_list, n=1)
  
  exist <- match(as.numeric(names(r.frames)), pos)
  if(any(!is.na(exist))){
    r.frames[!is.na(exist)] <- r_list[stats::na.omit(exist)]
  }
  
  # collect remaining frame ids
  i.frames <- as.numeric(names(r.frames)[sapply(r.frames, is.null)])
  
  # between which elements
  i.frames <- lapply(2:length(pos), function(i){
    y <- i.frames > pos[i-1] & i.frames < pos[i]
    if(any(y)) return(i.frames[which(y)])
  })
  i.rasters <- which(!sapply(i.frames, is.null))+1
  i.frames <- i.frames[i.rasters-1]
  
  # interpolation function
  v.fun <- function(v.x, v.y, ...) t(mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, ...) .na.approx(c(xx, v.na, yy))[pos.frames], SIMPLIFY = T))
  #v.fun <- function(v.x, v.y, ...) t(mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, ...) zoo::na.approx(c(xx, v.na, yy), rule = 2)[pos.frames], SIMPLIFY = T))
  #v.fun <- function(v.x, v.y) mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, xx.pos = x.pos, yy.pos = y.pos, xy.frame = frame) zoo::na.approx(c(xx, rep(NA, (yy.pos-xx.pos)-1), yy))[(xy.frame-xx.pos)+1], SIMPLIFY = T)
  #v.fun <- Vectorize(function(x, y, ...) zoo::na.approx(c(x, v.na, y), rule = 2)[pos.frame])
  
  # iterate over shoulder ranges
  for(i in i.rasters){
    
    # rasters
    if(n.rlay > 1){
      x <- unstack(r_list[[i-1]])
      y <- unstack(r_list[[i]])
    } else{
      x <- r_list[i-1] # keep listed using [ instead of [[ to work with lapply
      y <- r_list[i]
    }
    
    # positions
    x.pos <- pos[i-1]
    y.pos <- pos[i]
    v.na <- rep(NA, (y.pos-x.pos)-1)
    pos.frames <- (i.frames[[which(i.rasters == i)]]-x.pos)+1
    if(getOption("rtsVis.n_cores") > 1) parallel::clusterExport(cl, c("v.na", "pos.frames"), envir = environment())
    
    # interpolate layer-wise
    r <- lapply(1:length(x), function(i.layer){
      if(getOption("rtsVis.n_cores") > 1){
        clusterR(stack(x[[i.layer]], y[[i.layer]]), fun = overlay, args = list("fun" = v.fun), cl = cl) # export = c("pos.frames", "v.na"))
      }else overlay(stack(x[[i.layer]], y[[i.layer]]), fun = v.fun)
    })
    
    # disassemble brick time- and layerwise
    if(length(r) > 1){
      for(j in 1:length(i.frames[[which(i.rasters == i)]])){
        r.frames[[match(i.frames[[which(i.rasters == i)]], frames)[j]]] <- brick(lapply(1:n.rlay, function(lay) r[[lay]][[j]]))
      }
    } else{
      r.frames[match(i.frames[[which(i.rasters == i)]], frames)] <- if(inherits(r[[1]], "RasterLayer")) r else unstack(r[[1]])
    }
  }
  return(r.frames)
}


#' assign raster to frames
#' @importFrom raster nlayers crop extent brick writeRaster dataType
#' @noRd
.rFrames <- function(r_list, r_times, m.df, gg.ext, fade_raster = T, crop_raster = T, ...){
  
  if(!is.list(r_list)){
    r_list <- list(r_list)
    n <- 1
  } else n <- length(r_list)
  n.rlay <- nlayers(r_list[[1]])
  
  #if(n.rlay > 1) r_list <- lapply(1:n.rlay, function(i) lapply(r_list, "[[", i)) else r_list <- list(r_list) #FRIDAY
  
  if(isTRUE(crop_raster)){
    r_list <- lapply(r_list, crop, y = extent(gg.ext[1], gg.ext[3], gg.ext[2], gg.ext[4]), snap = "out")
  }
  
  if(n > 1){
    
    ## calcualte time differences to r_times
    x <- lapply(1:max(m.df$frame), function(y) max(unique(m.df[m.df$frame == y,]$time)))
    frame_times <- unlist(x)
    attributes(frame_times) <- attributes(x[[1]])
    diff.df <- as.data.frame(sapply(r_times, function(x) abs(difftime(frame_times, x, units = "secs"))))
    
    ## assign r_list positions per frame times
    pos.df <- data.frame(frame = 1:nrow(diff.df), pos_r = apply(diff.df, MARGIN = 1, which.min))
    
    ## interpolate/extrapolate
    if(isTRUE(fade_raster)){
      pos.df <- pos.df[apply(diff.df[,unique(pos.df[,2])], MARGIN = 2, which.min),]
      
      # start cluster and interpolate over all frames or badge-wise
      if(getOption("rtsVis.n_cores") > 1) cl <- parallel::makeCluster(getOption("rtsVis.n_cores"))
      if(isFALSE(getOption("rtsVis.frames_to_disk"))){
        r_list <- .int2frames(r_list, pos = pos.df$frame, frames = unique(m.df$frame), n.rlay = n.rlay, cl = cl)
      } else{
        
        # create frames badge-wise?
        badges <- unique(c(unlist(sapply(2:length(pos.df$frame), function(i){
          c(seq(if(i == 2) 1 else pos.df$frame[i-1], pos.df$frame[i],
                by = if(is.na(getOption("rtsVis.n_memory_frames"))) length(unique(m.df$frame)) else getOption("rtsVis.n_memory_frames")),
            pos.df$frame[i])
        }, simplify = F)), max(m.df$frame)))
        
        # write to drive instead of memory
        files <- unlist(sapply(2:length(badges), function(i){
          frames <- if(i == 2) (badges[i-1]):badges[i] else (badges[i-1]+1):badges[i]
          r <- .int2frames(r_list, pos = pos.df$frame, frames = frames, n.rlay = n.rlay, cl = cl)
          y <- paste0(getOption("rtsVis.dir_frames"), "/rtsVis_frame_", frames, ".tif")
          catch <- sapply(1:length(r), function(j) writeRaster(r[[j]], filename = y[[j]], datatype = dataType(r_list[[1]]), overwrite = T))
          return(y)
        }, USE.NAMES = F))
        
        # link to files
        r_list <- lapply(files, brick)
      }
      if(getOption("rtsVis.n_cores") > 1) parallel::stopCluster(cl)
    }else{
      r_list <- r_list[pos.df$pos_r]
    }
  }else{r_list <- r_list}
  return(r_list)
}

#' package startup
#' @importFrom pbapply pboptions
#' @noRd 
.onLoad <- function(libname, pkgname){
  if(is.null(getOption("rtsVis.verbose")))  options(rtsVis.verbose = FALSE)
  if(is.null(getOption("rtsVis.n_cores")))  options(rtsVis.n_cores = 1)
  if(is.null(getOption("rtsVis.frames_to_disk")))  options(rtsVis.frames_to_disk = FALSE)
  if(is.null(getOption("rtsVis.n_memory_frames")))  options(rtsVis.n_memory_frames = NA)
  if(is.null(getOption("rtsVis.dir_frames"))){
    options(rtsVis.dir_frames = paste0(tempdir(), "/rtsVis"))
    if(!dir.exists(getOption("rtsVis.dir_frames"))) dir.create(getOption("rtsVis.dir_frames"))
  }
}
