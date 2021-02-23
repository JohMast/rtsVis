#' NULL declaration to suppres R CMD CHECK warning related to tidyverse syntax
#' @keywords internal
#' @noRd
aggregate <- time <- name <- band <- value <- long  <-  lat <-   group <-  hcl.colors <- r_list_extract <-  x <-  minValue <-  maxValue <- quantile <-position_name  <- id <-  NULL


##From moveVis
#' verbose lapply
#'
#' @importFrom pbapply pblapply
#' @noRd 
.lapply <- function(X, FUN, ..., rtsVis.verbose = NULL, rtsVis.n_cores = NULL, rtsVis.export = NULL){
  if(is.null(rtsVis.verbose)) rtsVis.verbose <- getOption("rtsVis.verbose")
  if(is.null(rtsVis.n_cores)) rtsVis.n_cores <- getOption("rtsVis.n_cores")
  
  # with parallelization
  if(rtsVis.n_cores > 1){
    cl <- parallel::makeCluster(rtsVis.n_cores)
    if(!is.null(rtsVis.export)) parallel::clusterExport(cl, rtsVis.export)
    y <- try(parallel::parLapply(cl = cl, X, FUN, ...)) # ensures that cluster is stopped appropriately
    parallel::stopCluster(cl)
    if(inherits(y, "try-error")) out(y, type = 3) else return(y)
    
    # without parallelization
  }else if(isTRUE(rtsVis.verbose)) pblapply(X, FUN, ...) else lapply(X, FUN, ...)
}

##From raster
.haveMinMax <- function(x) {
  if (inherits(x, "RasterLayer") || inherits(x, "RasterBrick")) {
    return(x@data@haveminmax)
  } else if (inherits(x, "RasterStack")) {
    return(all(sapply(x@layers, function(y) y@data@haveminmax)))
  } else {
    return(FALSE)
  }
}

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
  v.fun <- function(v.x, v.y, ...) t(mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, ...) .na.approx(c(xx, v.na, yy))[pos.frames], SIMPLIFY = TRUE))
  #v.fun <- function(v.x, v.y, ...) t(mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, ...) zoo::na.approx(c(xx, v.na, yy), rule = 2)[pos.frames], SIMPLIFY = TRUE))
  #v.fun <- function(v.x, v.y) mapply(xx = v.x, yy = v.y, FUN = function(xx, yy, xx.pos = x.pos, yy.pos = y.pos, xy.frame = frame) zoo::na.approx(c(xx, rep(NA, (yy.pos-xx.pos)-1), yy))[(xy.frame-xx.pos)+1], SIMPLIFY = TRUE)
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


#' plot raster as ggplot
#' @importFrom raster ncell
#' @importFrom ggplot2 ggplot geom_tile geom_raster aes_string scale_fill_identity
#' @noRd 
.gg.bmap <- function(r, r_type, gglayer = FALSE, hillshade_layer=NULL, ...){
  extras <- list(...)
  if(!is.null(extras$maxpixels)) maxpixels <- extras$maxpixels else maxpixels <- 500000
  if(!is.null(extras$alpha)) alpha <- extras$alpha else alpha <- 1
  if(!is.null(extras$maxColorValue)) maxColorValue <- extras$maxColorValue else maxColorValue <- NA
  
  # aggregate raster if too large
  if(maxpixels < ncell(r)) r <- raster::aggregate(r, fact = ceiling(ncell(r)/maxpixels))
  
  # transform into data.frame
  df <- data.frame(raster::as.data.frame(r, xy = TRUE))
  colnames(df) <- c("x", "y", paste0("val", 1:(ncol(df)-2)))
  
  # factor if discrete to show categrocial legend
  df$fill <- df$val1
  if(r_type == "discrete") df$fill <- as.factor(df$fill)
  
  # transform to RGB colours
  if(r_type == "RGB"){
    if(is.na(maxColorValue)) maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = TRUE)
    
    if(maxColorValue < max(c(df$val1, df$val2, df$val3), na.rm = TRUE)){
      out("maxColorValue < maximum raster value. maxColorValue is set to maximum raster value.", type = 2)
      maxColorValue <- max(c(df$val1, df$val2, df$val3), na.rm = TRUE)
    }
    
    # remove NAs
    na.sel <- is.na(df$val1) | is.na(df$val2) | is.na(df$val3)
    if(any(na.sel)) df <- df[!na.sel,]
    
    df$fill <- grDevices::rgb(red = df$val1, green = df$val2, blue = df$val3, maxColorValue = maxColorValue)
  } else{
    
    # remove NAs
    na.sel <- is.na(df$val1)
    if(any(na.sel)) df <- df[!na.sel,]
  }
  # if NA gaps are there, use geom_tile, otherwise make it fast using geom_raster
  if(any(na.sel)){
    gg <- geom_tile(aes_string(x = "x", y = "y", fill = "fill"), data = df, alpha = alpha)
  } else{
    gg <- geom_raster(aes_string(x = "x", y = "y", fill = "fill"), data = df, alpha = alpha)
  }
  

  
  if(!is.null(hillshade_layer)){ #if a hillshade layer was provided
    gg <- ggplot()+gg+hillshade_layer+gg  #plot as normal, then the hillshade over it, then plot again (this ensures that we dont take the hillshades layout)
    if(r_type == "RGB") gg <- gg + scale_fill_identity() 
  }else if(isFALSE(gglayer)){
    gg <- ggplot() + gg
    if(r_type == "RGB") gg <- gg + scale_fill_identity() 
  }
  return(gg)
}



#' assign raster to frames
#' @importFrom raster nlayers crop extent brick writeRaster dataType
#' @noRd
.rFrames <- function(r_list, r_times, m.df, gg.ext, fade_raster = TRUE, crop_raster = TRUE, ...){
  
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
        }, simplify = FALSE)), max(m.df$frame)))
        
        # write to drive instead of memory
        files <- unlist(sapply(2:length(badges), function(i){
          frames <- if(i == 2) (badges[i-1]):badges[i] else (badges[i-1]+1):badges[i]
          r <- .int2frames(r_list, pos = pos.df$frame, frames = frames, n.rlay = n.rlay, cl = cl)
          y <- paste0(getOption("rtsVis.dir_frames"), "/rtsVis_frame_", frames, ".tif")
          catch <- sapply(1:length(r), function(j) writeRaster(r[[j]], filename = y[[j]], datatype = dataType(r_list[[1]]), overwrite = TRUE))
          return(y)
        }, USE.NAMES = FALSE))
        
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

#Could probably be substituted by raster::quantile
#but as raster::stretch uses the method below, we stick with this one
#' ts_get_layer_quantiles
#' @description Takes regular samples from a single rasterlayer and determines the quantiles
#' @param x a raster layer
#' @param minq Lower quantile to be determined
#' @param maxq Upper quantile to be determined
#' @param samplesize Number of samples to take
#' @importFrom raster sampleRegular minValue maxValue
#' @importFrom stats quantile
#' @return a named vector of the minimum and maximum value (corresponding to minq and maxq)
#' @noRd
ts_get_layer_quantiles <- function(x,minq=0.02,maxq=0.98,samplesize=100000){
  #get quantiles for one image
  minq <- max(0,minq)
  maxq <- min(1,maxq)
  stopifnot(minq < maxq)
  
  if ((minq==0 & maxq==1) & .haveMinMax(x)) {
    q <- cbind(minValue(x), maxValue(x)) #old version that works, but requires internal raster function
  } else {
    if (samplesize[1] < ncell(x)) {
      stopifnot(samplesize[1] > 1) 
      y <- sampleRegular(x, samplesize, asRaster=TRUE)
      q <- quantile(y, c(minq, maxq), na.rm=TRUE)
    } else {
      q <- quantile(x, c(minq, maxq), na.rm=TRUE)
    }
  }
}

#could probably be substituted by raster::quantile as in:
#cellStats(x, stat=raster::quantile,ncells=100000,probs=c(0.01,0.99))
#' ts_get_stack_quantiles
#' @description Takes regular samples from a single rasterstack and determines the quantiles for every layer
#' @param x a raster stack
#' @param minq Lower quantile to be determined
#' @param maxq Upper quantile to be determined
#' @param samplesize Number of samples to take
#' @return a matrix of n_layers*2, one min and one max bound for every layer
#' @noRd
ts_get_stack_quantiles <- function(x,minq=0.02,maxq=0.98,samplesize=100000){
  if(nlayers(x)<=1){
    rs_qs <-  ts_get_layer_quantiles(x,minq=minq,maxq=maxq,samplesize=samplesize)
  }else{
    rs_qs <- t(do.call(cbind, lapply(unstack(x),FUN =  ts_get_layer_quantiles,minq=minq,maxq=maxq,samplesize=samplesize)))
  }
  return(rs_qs)
}

#2do: Make this choose ts_get_layer quantiles for single layers (if this is necessary, and ts_get_stack_quantiles breaks for single layers)
#' ts_get_ts_quantiles
#' @param ts a list of raster stacks
#' @param minq Lower quantile to be determined
#' @param maxq Upper quantile to be determined
#' @param samplesize Number of samples to take
#' @return a matrix of n_layers*2, one min and one max bound for every layer, which is the min and the max respectively of all the rasters for that layer index
#' @noRd
ts_get_ts_quantiles <- function(ts,minq=0.02,maxq=0.98,samplesize=100000){
  if(minq==0){minq <- 0.000001} #This prevents weird things from occasionally happening with NAs (unknown cause)
  qs <- sapply(ts,ts_get_stack_quantiles,simplify="array",minq=minq,maxq=maxq,samplesize=samplesize)
  if(nlayers(ts[[1]])<=1){ #for discrete and gradient rasters, we get a 2*n_images matrix, for rgb we get a 2*n_images*n_layers array, which needs to be handles a bit differently
    maxqs <- max(qs[2])
    minqs <- min(qs[1])
  }else{
    maxqs <- apply(qs[,2,], MARGIN = 1,max)
    minqs <- apply(qs[,1,], MARGIN = 1,min)
  }
  return(as.data.frame(cbind(minqs,maxqs)))
}

#' ts_stretch
#' @description Stretch and clips a raster(stack) from within a certain source range determined by min quantile and max quantile to a target range ymin to ymax
#' @param x a raster to be stretched
#' @param minqs Lower quantile 
#' @param maxqs Higher quantile 
#' @param ymin target min value
#' @param ymax target max value
#' @importFrom RStoolbox rescaleImage
#' @importFrom raster clamp
#' @return A raster with values between ymin and ymax
#' @noRd
ts_stretch <- function(x,minqs,maxqs,ymin=0,ymax=0){
  raster::clamp(
    RStoolbox::rescaleImage(x,
                            xmin = minqs,
                            xmax = maxqs,
                            ymin = ymin,
                            ymax = ymax,
                            forceMinMax = TRUE),
    ymin,ymax)
}


#' ts_stretch_list
#' @description Stretch and clips a list of raster(stacks) from within a certain source range determined by min quantile and max quantile to a target range ymin to ymax. The quantiles are determined for each layer individually but across all rasters of the inputlist
#' @param x_list a list of rasters to be stretched
#' @param minqs Lower quantile 
#' @param maxqs Higher quantile 
#' @param ymin target min value
#' @param ymax target max value
#' @return A list of raster(stacks) with values between ymin and ymax
#' @noRd
ts_stretch_list <- function(x_list,minq=0.01,maxq=0.99,ymin=0,ymax=0, samplesize = 10000){
  ts_quantiles <- ts_get_ts_quantiles(ts = x_list,minq = minq,maxq = maxq,samplesize = samplesize)
  out <- lapply(x_list,ts_stretch,minqs = ts_quantiles$minqs,maxqs = ts_quantiles$maxqs,ymin = 0,ymax = 1)
  .ts_set_frametimes(out,.ts_get_frametimes(x_list))
}

#' .ts_makeframes
#' @param x_list a list of rasters
#' @param r_type one of "discrete","gradient", "RGB"
#' @return a list of ggplots, carrying over the "time" attribute of x_list set
#' @noRd
.ts_makeframes <- function(x_list,r_type="RGB",gglayer=FALSE,alpha=1,hillshade_layer=NULL){
  out <- lapply(x_list, .gg.bmap,r_type=r_type,gglayer=gglayer,alpha=alpha,hillshade_layer=hillshade_layer)
  .ts_set_frametimes(out,.ts_get_frametimes(x_list))
}

#' #' .ts_update_NA_util
#' #'
#' #' @param x a raster object
#' #' @param new_na Value to replace the old one as NA
#' #' @return The modified raster object, with the new NA value set
#' #' @importFrom raster NAvalue
#' #' @noRd
#' .ts_update_NA_util <- function(x,new_na){
#'   NAvalue(x) <- new_na
#'   return(x)
#' }
#' 

#' .blacken_NA_util
#' @param x A list of raster objects 
#' @return a raster object with NAs replaced by 0
#' @noRd
.blacken_NA_util <- function(x_list){
  out <- lapply(x_list,FUN = function(y){
    y[is.na(y[])] <- 0 
    return(y)
  })
  #carry over the frame times
  .ts_set_frametimes(out,.ts_get_frametimes(x_list))
}


#' ts_guess_raster_type
#' @description Attempts to determine the raster type of the input raster, to be used for other functions in the rtsVis package.
#' @param x A raster object
#' @return
#' @importFrom raster sampleRandom
#' @noRd
.ts_guess_raster_type <- function(x){
  print("Guessing raster type.")
  if(nlayers(x) >= 3 ){
    r_type <-"RGB"
    print("Detected 3+ layers, choosing raster type 'RGB'")
  }else if(nlayers(x) != 3 && length(unique(sampleRandom(x,100)))>=50){
    r_type <-"gradient"
    print("Detected more than 50/100 unique values, choosing raster type 'gradient'")
  }else if(nlayers(x) != 3 && length(unique(sampleRandom(x,100)))<50){
    r_type <-"discrete"
    print("Detected fewer than 50/100 unique values, choosing raster type 'discrete'")
  }else{
    stop("Could not determine raster type.")
  }
  return(r_type)
}

#' .ts_subset_ts_util
#' @noRd 
#' @param x_list a list of raster objects
#' @param l_indices a vector of indices to select from each object
#' @return a list of raster objects, each subset by l_indices
#' @noRd
.ts_subset_ts_util <- function(x_list,l_indices=1){
  x_list_out <- lapply(x_list, function(x){
    x[[l_indices]]
  }
  )
  x_list_out <- .ts_set_frametimes(x_list_out,.ts_get_frametimes(x_list)) #reapply the dates
  return(x_list_out)
}


#' .ts_extract_from_frames
#' @noRd 
#' @param r_list_extract A list of rasters to extract values from. Need to have frame times set so they can be extracted by .ts_get_frametimes
#' @param positions Positions from where to extract values. Can be a Two-column matrix, a spatialpoints, or spatialpolygons. If none are provided, values are extracted for the entire raster using raster::cellstats
#' @param position_names (Optional) A vector of length positions, giving the names of the position objects
#' @param FUN A function to apply to summarize the values per position object. Default is mean.
#' @return A dataframe. Columns for the summarized values per layer, position centroid lat & lon, position names, and timestamp and frame indices (integer). Number of rows equals the number of positions in positions multiplied by the number of rasters in r__list_extract
#' @importFrom tidyr pivot_longer
#' @importFrom raster extract 
#' @importFrom sf st_centroid st_coordinates st_geometry st_geometry_type st_buffer st_crs st_contains st_as_sfc st_bbox
#' @importFrom raster buffer intersect
#' @importFrom assertthat assert_that
#' @import sp
#' @noRd
.ts_extract_from_frames <- function(r_list_extract,positions=NULL,position_names=NULL,band_names=NULL,FUN=mean,pbuffer=NULL){
  
  nlay <- nlayers(r_list_extract[[1]])#get the number of layers from a template
  
  frametimes <- .ts_get_frametimes(r_list_extract)
  
  assert_that(length(r_list_extract)==length(frametimes))

  if(!is.null(positions)){
      assert_that(st_crs(r_list_extract[[1]])==st_crs(positions))
    if(inherits(positions, "sf")){
      all(st_contains(st_as_sfc(st_bbox(r_list_extract[[1]])),positions,sparse = FALSE))
    }else if (nrow(positions)==1){  #if there is only one object (point or line) there may be no raster::intersect
      all(st_contains(st_as_sfc(st_bbox(r_list_extract[[1]])),sf::st_as_sf(positions[1,]),sparse = FALSE)) #use the sf version for just the first feature then
    }else{
      assert_that(!is.null(raster::intersect(r_list_extract[[1]],positions)))
      
    }
      if(!is.null(pbuffer)){
        if(inherits(positions, "sf")){
          positions <- st_buffer(positions,dist=pbuffer)
        }else if(inherits(positions,c("matrix","array"))){
          print("Buffering not supported for raw coordinates. Consider converting the coordinates into an sf object.")
        }else{
          positions <- raster::buffer(positions,width=pbuffer,dissolve=FALSE)
        }
    }
  }
  #make names if no names
  if(is.null(names(r_list_extract))){
    names(r_list_extract) <- as.character(1:length(r_list_extract))
  }
  # Check for duplicated Names
  while(anyDuplicated(names(r_list_extract))){
    warning("Duplicated Names found in input raster list")
    print("Creating unique Names")
    names(r_list_extract)[duplicated(names(r_list_extract))] <- paste0(names(r_list_extract)[duplicated(names(r_list_extract))],"_2")
  }
  

  extr_df <-  
    do.call(rbind,lapply(names(r_list_extract),
                         function(x) {
                           if(!is.null(positions)){
                             if(inherits(positions, "sf")){
                               if(all(st_geometry_type(positions) %in% c("MULTIPOLYGON", "POLYGON") )){
                                 if(!is.null(position_names)){
                                   o_name <- position_names
                                 }else{
                                   o_name <-paste("Polygon" ,(1:nrow(positions)))
                                 }
                                 extr_df <- raster::extract(r_list_extract[[x]], positions, df = FALSE,fun=FUN,na.rm=TRUE)
                                 #if we did use a fun to aggregate, the previous step returned a dataframe instead of a list of dataframes
                                 #if so, things get more complicated
                                 # we need make it a list of 1 for consitency
                                 # (Alternatively use df=TRUE to get a df with a sequential ID which we could then recode somehow)
                                 if(!is.list(extr_df)){
                                   if(nlay>1){extr_df <- split(extr_df,1:nrow(extr_df))  #this now is a list of1 containing a vector, otherwise a list of n_objects containing a matrix
                                   }else{
                                     extr_df <- as.list(extr_df)
                                   }
                                 }
                                 #add the object name to the respective list element
                                 for(i in 1:length(extr_df)){
                                   extr_df[[i]] <- data.frame(matrix(extr_df[[i]],ncol = nlay,byrow = FALSE))
                                   extr_df[[i]]$position_name <- o_name[[i]]
                                   extr_df[[i]]$centr_lon <-   st_coordinates(st_centroid(st_geometry(positions)))[,1][i] #sf variant of the above
                                   extr_df[[i]]$centr_lat <-   st_coordinates(st_centroid(st_geometry(positions)))[,2][i] #sf variant of the above
                                 }
                                 #bind the list elements together
                                 extr_df <- do.call("rbind", extr_df)
                                 #ensure that its a data frame
                                 extr_df <- as.data.frame(extr_df)
                               }else if(all(st_geometry_type(positions)=="POINT")){
                                 if(!is.null(position_names)){
                                   o_name <- position_names
                                 }else{
                                   o_name <- paste("Point", 1:nrow(positions))
                                 }
                                 
                                 #Extract the Values, !!suppressing warnings which are currently caused by discarded datums due to Proj4->proj6 switch!!
                                 extr_df <-suppressWarnings(
                                   raster::extract(r_list_extract[[x]], positions, df = FALSE,fun=FUN,na.rm=TRUE)
                                 )
                                 
                                 #if we did use a fun to aggregate, the previous step returned a dataframe instead of a list of dataframes
                                 #if so, things get more complicated
                                 # we need make it a list of 1 for consitency
                                 # (Alternatively use df=TRUE to get a df with a sequential ID which we could then recode somehow)
                                 if(!is.list(extr_df)){
                                      if(nlay>1){extr_df <- split(extr_df,1:nrow(extr_df))  #this now is a list of1 containing a vector, otherwise a list of n_objects containing a matrix
                                      }else{
                                        extr_df <- as.list(extr_df)
                                      }
                                      
                                   }
                                 #add the object name to the respective list element
                                 for(i in 1:length(extr_df)){
                                   extr_df[[i]] <- data.frame(matrix(extr_df[[i]],ncol = nlay,byrow = FALSE))
                                   extr_df[[i]]$position_name <- o_name[i]
                                   extr_df[[i]]$centr_lon <-   st_coordinates(positions)[,1][i] #sf variant of the below
                                   extr_df[[i]]$centr_lat <-   st_coordinates(positions)[,2][i] #sf variant of the below
                                 }
                                 #bind the list elements together
                                 extr_df <- do.call("rbind", extr_df)
                                 #ensure that its a data frame
                                 extr_df <- as.data.frame(extr_df)
                               }
                               
                               
                               
                             }else if(inherits(positions,"SpatialPointsDataFrame")){
                               if(!is.null(position_names)){
                                 o_name <- position_names
                               }else{
                                 o_name <- paste("Point", 1:nrow(positions))
                               }
                               #Extract the Values, !!suppressing warnings which are currently caused by discarded datums due to Proj4->proj6 switch!!
                               extr_df <-suppressWarnings(
                                 raster::extract(r_list_extract[[x]], positions, df = FALSE,fun=FUN,na.rm=TRUE)
                               )
                               #if we did use a fun to aggregate, the previous step returned a dataframe instead of a list of dataframes
                               #if so, things get more complicated
                               # we need make it a list of 1 for consitency
                               # (Alternatively use df=TRUE to get a df with a sequential ID which we could then recode somehow)
                               if(!is.list(extr_df)){
                                 if(nlay>1){extr_df <- split(extr_df,1:nrow(extr_df))  #this now is a list of1 containing a vector, otherwise a list of n_objects containing a matrix
                                 }else{
                                   extr_df <- as.list(extr_df)
                                 }
                                 
                               }
                               #add the object name to the respective list element
                               for(i in 1:length(extr_df)){
                                 extr_df[[i]] <- data.frame(matrix(extr_df[[i]],ncol = nlay,byrow = FALSE))
                                 extr_df[[i]]$position_name <- o_name[[i]]
                                 extr_df[[i]]$centr_lon <- coordinates(positions)[, 1][i]
                                 extr_df[[i]]$centr_lat <- coordinates(positions)[, 2][i]
                               }
                               #bind the list elements together
                               extr_df <- do.call("rbind", extr_df)
                               #ensure that its a data frame
                               extr_df <- as.data.frame(extr_df)
                               
                             }else if(inherits(positions,"SpatialPolygonsDataFrame")){
                               if(!is.null(position_names)){
                                 o_name <- position_names
                               }else{
                                 o_name <-paste("Polygon" ,(1:nrow(positions)))
                               }
                               extr_df <- raster::extract(r_list_extract[[x]], positions, df = FALSE,fun=FUN,na.rm=TRUE)
                               #if we did use a fun to aggregate, the previous step returned a dataframe instead of a list of dataframes
                               #if so, things get more complicated
                               # we need make it a list of 1 for consitency
                               # (Alternatively use df=TRUE to get a df with a sequential ID which we could then recode somehow)
                               if(!is.list(extr_df)){
                                 if(nlay>1){extr_df <- split(extr_df,1:nrow(extr_df))  #this now is a list of1 containing a vector, otherwise a list of n_objects containing a matrix
                                 }else{
                                   extr_df <- as.list(extr_df)
                                 }
                                 
                               }
                               #add the object name to the respective list element
                               for(i in 1:length(extr_df)){
                                 extr_df[[i]] <- data.frame(matrix(extr_df[[i]],ncol = nlay,byrow = FALSE))
                                 extr_df[[i]]$position_name <- o_name[[i]]
                                 extr_df[[i]]$centr_lon <-   coordinates(positions)[,1][i] #sf variant of the above
                                 extr_df[[i]]$centr_lat <-   coordinates(positions)[,2][i] #sf variant of the above
                               }
                               #bind the list elements together
                               extr_df <- do.call("rbind", extr_df)
                               #ensure that its a data frame
                               extr_df <- as.data.frame(extr_df)
                               
                             }else if(inherits(positions,c("matrix","array"))){
                               assert_that(ncol(positions)==2)
                               if(!is.null(position_names)){
                                 o_name <- position_names
                               }else{
                                 o_name <- paste("Point", 1:nrow(positions))
                               }
                               extr_df <- raster::extract(r_list_extract[[x]], positions, df = FALSE,fun=FUN,na.rm=TRUE)
                               #if we did use a fun to aggregate, the previous step returned a dataframe instead of a list of dataframes
                               #if so, things get more complicated
                               # we need make it a list of 1 for consitency
                               # (Alternatively use df=TRUE to get a df with a sequential ID which we could then recode somehow)
                               if(!is.list(extr_df)){
                                 if(nlay>1){extr_df <- split(extr_df,1:nrow(extr_df))  #this now is a list of1 containing a vector, otherwise a list of n_objects containing a matrix
                                 }else{
                                   extr_df <- as.list(extr_df)
                                 }
                                 
                               }
                               #add the object name to the respective list element
                               for(i in 1:length(extr_df)){
                                 extr_df[[i]] <- data.frame(matrix(extr_df[[i]],ncol = nlay,byrow = FALSE))
                                 extr_df[[i]]$position_name <- o_name[[i]]
                                 extr_df[[i]]$centr_lon <- coordinates(positions)[, 1][i]
                                 extr_df[[i]]$centr_lat <- coordinates(positions)[, 2][i]
                               }
                               #bind the list elements together
                               extr_df <- do.call("rbind", extr_df)
                               #ensure that its a data frame
                               extr_df <- as.data.frame(extr_df)
                               
                             }
                           }else if(is.null(positions)){
                             extr_df <- as.data.frame(matrix( raster::cellStats(r_list_extract[[x]],FUN),nrow = 1,byrow = TRUE)) #unpiped
                             extr_df$lon <- mean(extent(r_list_extract[[x]])[1:2])
                             extr_df$lat <- mean(extent(r_list_extract[[x]])[3:4])
                             extr_df$position_name <- "AOI"
                             extr_df$centr_lon <- 0  #2do: add the centroid coords or sth equivalent
                             extr_df$centr_lat <- 0  #2do: add the centroid coords or sth equivalent
                           }
                           names(extr_df)[1:nlay] <- band_names
                           #extr_df$time <- frametimes[as.integer(x)]
                           extr_df$time <- frametimes[names(r_list_extract)==x]
                           return(extr_df)
                         }))
  extr_df$frame <- as.numeric(as.factor(extr_df$time))
  out <- extr_df 
  
  return(out)
}




#' line stats plot function
#' Adapted from moveVis and only lightly changed (to not require a move object and instead a rtsVis extracted dataframe
#' @param i the index of the current frame
#' @param edf a dataframe of all extracted values across all frames
#' @param pl position_legend (Optional) logical. If \code{TRUE}: Add a legend for the positions. Only recommended if \code{aes_by_pos} is also  \code{TRUE}.
#' @param lp legend_position  (Optional) character, position of the legend. Use \code{"none"} to disable all legends. Default is \code{"right"}.
#' @param bl band_legend (Optional) logical. If \code{TRUE}: Add a legend for the bands. Default is \code{TRUE}.
#' @param blt band_legend_title  (Optional) character, title of the band legend. Default is \code{"Positions"}.
#' @param plt position_legend_title  (Optional) character, position of the legend. Use \code{"none"} to disable all legends. Default is \code{"right"}.
#' @param ps plot_size (Optional) numeric, size for the ggplot objects. Default is \code{1}.
#' @param vs val_seq Value Sequence for the y axis.
#' @param abp aes_by_pos  (Optional) logical. If \code{TRUE}: vary the linetype aesthetic to be different for each position? If  \code{FALSE}, this also disables the \code{position_legend}, as no notable classes will be plotted. Default is \code{TRUE}.
#' @noRd
.ts_gg_line <- function(i, edf , pl,lp, bl, blt,plt, ps, vs,abp){
  
  #The data up to the current frame (this will be plotted)
  x = edf[edf$frame <= i,]
  #All data (this sets the frame)
  y=edf
  
  ## generate base plot, either with position mapped to linetype or without
  if(!isTRUE(abp)){
    p <- ggplot(x, aes(x = time, y = value,group = interaction(position_name,band),colour=band))
  }else{
    p <- ggplot(x, aes(x = time, y = value,group = interaction(position_name,band),linetype=position_name,colour=band))+
      scale_linetype_discrete(name=plt)
    
  }
  ## style it
  p <- p +
    geom_path( size = ps, show.legend = TRUE)+  
    coord_cartesian(xlim = c(min(y$time, na.rm = TRUE), max(y$time, na.rm = TRUE)), ylim = c(min(vs, na.rm = TRUE), max(vs, na.rm = TRUE))) +
    theme_bw() + 
    theme(aspect.ratio = 1) +
    scale_y_continuous(expand = c(0,0), breaks = vs)+
    theme(legend.position = lp)
  
  #add the colors
  p <- p +
    scale_colour_manual(values = x$band_colors,breaks = x$band, name=blt)
  
  
  ## add legend
  if(!isTRUE(pl)){
    p <- p + guides(linetype = FALSE)
  }  
  if(!isTRUE(bl)){
    p <- p + guides(colour = FALSE)
  }  
  return(p)
}



#Aux function for rounding significant digits, credit to 
#https://stackoverflow.com/a/39611375
floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)



#' .ts_gg_vio
#'
#' @description Create a violin plot
#' @param i the index of the current frame
#' @param edf a dataframe of all extracted values across all frames
#' @param pl position_legend (Optional) logical. If \code{TRUE}: Add a legend for the positions. Only recommended if \code{aes_by_pos} is also  \code{TRUE}.
#' @param lp legend_position  (Optional) character, position of the legend. Use \code{"none"} to disable all legends. Default is \code{"right"}.
#' @param bl band_legend (Optional) logical. If \code{TRUE}: Add a legend for the bands. Default is \code{TRUE}.
#' @param blt band_legend_title  (Optional) character, title of the band legend. Default is \code{"Positions"}.
#' @param plt position_legend_title  (Optional) character, position of the legend. Use \code{"none"} to disable all legends. Default is \code{"right"}.
#' @param ps plot_size (Optional) numeric, size for the ggplot objects. Default is \code{1}.
#' @param vs val_seq Value Sequence for the y axis.
#' @param abp aes_by_pos  (Optional) logical. If \code{TRUE}: vary the linetype aesthetic to be different for each position? If  \code{FALSE}, this also disables the \code{position_legend}, as no notable classes will be plotted. Default is \code{TRUE}.
#' @return
#' @importFrom dplyr group_size group_by
#' @noRd
.ts_gg_vio <- function(i,edf,pl,lp, bl, blt,plt, ps, vs,abp){
  
  x <- edf[edf$frame == i,]
  min(group_size(group_by(x,position_name,band))) -> n_value_min
  
  #x %>% group_by(position_name,band)%>% group_size() %>% min() -> n_value_min
  if(n_value_min<10){
    warning("Some frames contain less than 10 values for some positions. Are you sure you want to plot distribution? ")
  }
  
  ## generate base plot, either with position mapped to linetype or without
  if(!isTRUE(abp)){
    p <- ggplot(x, aes(x = 1, y = value,group = interaction(position_name,band),colour=band))
  }else{
    p <- ggplot(x, aes(x = 1, y = value,group = interaction(position_name,band),linetype=position_name,colour=band))+
      scale_linetype_discrete(name=plt)
    
  }
  
  #Style the plot
  p <-p +
    geom_violin( size = ps, show.legend = TRUE)+  
    coord_cartesian(xlim = c(0,2), ylim = c(min(vs, na.rm = TRUE), max(vs, na.rm = TRUE))) +
    theme_bw() + 
    theme(aspect.ratio = 1) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    scale_y_continuous(expand = c(0,max(x$value)/10), breaks = vs)+
    facet_grid(position_name ~ band, scales='free')+
    theme(legend.position = lp)
  
  #add the colors
  p <- p +
    scale_colour_manual(values = x$band_colors,breaks = x$band, name=blt)
  
  
  ## add legend
  if(!isTRUE(pl)){
    p <- p + guides(linetype = FALSE)
  }  
  if(!isTRUE(bl)){
    p <- p + guides(colour = FALSE)
  }  
  return(p)
}


#' line stats plot function
#' Version of the ts_gg_line where mappings for colors and positions are reversed
#' @param i the index of the current frame
#' @param edf a dataframe of all extracted values across all frames
#' @param pl position_legend (Optional) logical. If \code{TRUE}: Add a legend for the positions. Only recommended if \code{aes_by_pos} is also  \code{TRUE}.
#' @param lp legend_position  (Optional) character, position of the legend. Use \code{"none"} to disable all legends. Default is \code{"right"}.
#' @param bl band_legend (Optional) logical. If \code{TRUE}: Add a legend for the bands. Default is \code{TRUE}.
#' @param blt band_legend_title  (Optional) character, title of the band legend. Default is \code{"Positions"}.
#' @param plt position_legend_title  (Optional) character, position of the legend. Use \code{"none"} to disable all legends. Default is \code{"right"}.
#' @param ps plot_size (Optional) numeric, size for the ggplot objects. Default is \code{1}.
#' @param position_colors (Optional) character. Colors for the positions. By default, uses rainbow colors.
#' @param vs val_seq Value Sequence for the y axis.
#' @param abp aes_by_pos  (Optional) logical. If \code{TRUE}: vary the linetype aesthetic to be different for each position? If  \code{FALSE}, this also disables the \code{position_legend}, as no notable classes will be plotted. Default is \code{TRUE}.
#' @noRd
#' @importFrom grDevices rainbow
.ts_gg_line_flp <- function(i, edf , pl,lp, bl, blt,plt, ps, vs,abp,position_colors=NULL){

  #The data up to the current frame (this will be plotted)
  x = edf[edf$frame <= i,]
  #All data (this sets the frame)
  y=edf
  
  if(is.null(position_colors)){position_colors <- rainbow(length(unique(edf$position_name)))}
  
  ## generate base plot, either with position mapped to linetype or without
  if(!isTRUE(abp)){
    p <- ggplot(x, aes(x = time, y = value,group = interaction(position_name,band),linetype=position_name))
  }else{
    p <- ggplot(x, aes(x = time, y = value,group = interaction(position_name,band),linetype=band,colour=position_name))+
      scale_linetype_discrete(name=blt)
    
  }
  ## style it
  p <- p +
    geom_path( size = ps, show.legend = TRUE)+  
    coord_cartesian(xlim = c(min(y$time, na.rm = TRUE), max(y$time, na.rm = TRUE)), ylim = c(min(vs, na.rm = TRUE), max(vs, na.rm = TRUE))) +
    theme_bw() + 
    theme(aspect.ratio = 1) +
    scale_y_continuous(expand = c(0,0), breaks = vs)+
    theme(legend.position = lp)
  
  #add the colors
  p <- p +
    scale_colour_manual(values = position_colors,
                        #breaks = unique(position_name),
                        name=plt)


  ## add legend
  if(!isTRUE(bl)){
    p <- p + guides(linetype = FALSE)
  }
  if(!isTRUE(pl)){
    p <- p + guides(colour = FALSE)
  }
  return(p)
}








#' Add \code{ggplot2} function to frames
#' @import ggplot2 
#' @noRd 
.ts_add_gg <- function(frames, gg, data = NULL, ..., verbose = TRUE){
  
  ## check data and replicate if necessary
  if(inherits(data, "list")){
    if(length(data) != length(frames)) out("Argument 'data' is a list und thus must be of same length as 'frames'.", type = 3)
  } else{
    data <- rep(list(data), length(frames))
  }
  
  ## gg is not a list, make it one
  if(inherits(gg, "list")){
    if(length(gg) != length(frames)) out("Argument 'gg' is a list und thus must be of same length as 'frames'.", type = 3)
  } else{
    if(length(gg) != length(frames)) gg <- rep(list(gg), length(frames))
  }
  if(!is.call(gg[[1]])) out("Argument 'gg' must be an expression or a list of expressions (see ?moveVis::add_gg and ?ggplot2::expr).", type = 3)
  
  mapply(.frame = frames, .gg = gg, data = data, function(.frame, .gg, data, arg = list(...)){
    if(length(arg) > 0) for(i in 1:length(arg)) assign(names(arg)[[i]], arg[[i]])
    return(.frame + eval(.gg)) #parse(text = paste0(y, collapse = " + ")))
  }, USE.NAMES = FALSE, SIMPLIFY = FALSE)
}
