#' Create spatial ggplots of a raster time series
#'
#' @param x_list a list of raster objects.
#' @param r_type (Optional) character, one of \code{"discrete"}, \code{"gradient"} or \code{"RGB"}. By default, attempts to discern the \code{r_type} from the content of \code{r_list}.
#' @param minq (Optional) numeric, lower quantile boundary for the stretch.  Default is \code{0.02}.
#' @param maxq (Optional) numeric, upper quantile boundary for the stretch. Default is \code{0.98}.
#' @param samplesize (Optional) numeric, number of samples per layer to determine the quantile from. See \link[raster]{sampleRegular} for details on the sampling. Default is \code{1000}.
#' @param blacken_NA  (Optional) logical. If \code{TRUE}: set \code{NA} to \code{0}. Default is \code{FALSE}. 
#' @param l_indices (Optional) numeric, a vector of layer indices specifying which layers are to be plotted. Should contain 3 values for an RGB image or a single value for a discrete or gradient image. By default, chooses the first three layers if \code{r_type} is \code{"RGB"} and the first layer if \code{r_type} is \code{"discrete"} or \code{"gradient"}.
#'
#' @return A list of ggplots
#' @details A linear percent stretch will be applied to each band to improve contrast.
#' #\deqn{(X - Low_{in}) * \frac{((High_{out}-Low_{out})}{(High_{in}-Low_{in}))}  + Low_{out}}
#' # The stretch parameters 
#' # \eqn{High_{out}},\eqn{Low_{out}},\eqn{High_{in}},\eqn{Low_{in}}
#' #  are calculated separately for each band based on the \code{minq} and \code{maxq} which are first applied to \code{samplesize} regular samples (see \link[raster]{sampleRegular}) of each individual layer.
#'  From these, across all layers belonging to a certain band, the minimum and maximum values are taken as the stretching parameters for the linear stretch, which is performed using  \link[RStoolbox]{rescaleImage}.  Discrete \code{r_type}s will not be stretched.
#'   To further enhance the plots, consider using functionalities implemented in \pkg{moveVis},
#' (see \url{http://movevis.org/} ). For example, a northarrow may be added to all frames using \link[moveVis]{add_northarrow}.
#' @export
#' @author Johannes Mast
#' @importFrom raster compareCRS nlayers 
#' @examples 
#' \donttest{
#' #Setup
#' x_list <- MODIS_SI_ds
#' x_dates <- do.call(c, lapply(MODIS_SI_ds,attr,"time") )
#' 
#' #Fill NAs
#' x_list_filled <- ts_fill_na(x_list)
#' 
#' #Make a sequence of output dates, double the length of input dates
#' out_dates <-seq.POSIXt(from = x_dates[1],
#'                        to = x_dates[length(x_dates)],length.out = length(x_dates)*2 )
#' 
#' #For each output date, interpolate a raster image from the input files
#' r_list_out <- ts_raster(r_list = x_list_filled,
#'                         r_times = x_dates,
#'                         out_times = out_dates,
#'                         fade_raster = TRUE)
#' #Create the frames 
#' # as from the desired layers
#'r_frames <- ts_makeframes(x_list = r_list_out,
#'                           l_indices = c(1,4,3))
#' }
ts_makeframes <- function(x_list,r_type = NULL,minq = 0.02,maxq = 0.98,samplesize = 1000,blacken_NA=FALSE,l_indices=NULL){
  
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

