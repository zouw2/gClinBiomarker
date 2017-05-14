#' Wrapper for fitting model and adjusting to predicted values and
#' passing newly fitted model to ggplot geom_stat_ribbon
#' 
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#' 
#' @param data data to use for model fitting and plotting
#' @param mapping ggplot aesthetic mapping to use for plotting
#' @param formula formula to use for adjusting values
#' @param model model function to use for fitting; defaults to lm
#' @param model.args additional model parameters to pass to model
#' @param facet.fun function to use for ggplot faceting in ggplot2::facet_grid
#' @param ... additional arguments passed to geom_stat_ribbon
#' 
#' @return a ggplot object with y values adjusted based on model function if provided
#' 
#' @examples
#' library(tidyverse) # for pipe and tibble
#' 
#' # default representation
#' PlotLong(nasa %>% as_tibble, aes(x=month, y=temperature))
#' 
#' # a more appropriate representation for large n
#' PlotLong(nasa %>% as_tibble, aes(x=month, y=temperature), 
#'          fun.data = 'quantile_contour')
#' 
#' # include linear adjustment accounting for amount of ozone
#' PlotLong(nasa %>% as_tibble, aes(x=month, y=temperature), 
#'          formula = temperature ~ ozone, fun.data = 'quantile_contour', 
#'          show.counts = T)
#' 
#' @export
#' 
PlotLong <- function(data, mapping, formula = NULL, model = lm, 
                     model.args = NULL, facet.fun = NULL, ...) {
  
  # overwrite mapping with new fitted variable ("<y>.fitted")
  if (!is.null(formula)) mapping$y <- as.name(paste(deparse(mapping$y), "fitted", sep="."))
          
  data %>%
    (function(.data) { 
      if (!is.null(formula)) augment_predict(.data, model, formula, model.args)
      else .data
    }) %>%
    
    ggplot() + mapping + 
      do.call(geom_stat_ribbons, list(...)) +
      (if (!is.null(facet.fun)) facet_grid(facet.fun)
       else facet_null() ) +
      (if (!is.null(formula)) ylab(paste("Adjusted", deparse(formula[[2]])))
       else NULL)
  
}



#' Adds prediction outputs to data based on model and formula
#' 
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#' 
#' @param .data data to use for model fitting
#' @param model model function to use (e.g `lm`)
#' @param fomula formula to use for model fitting (e.g `mpg ~ gear + wt`)
#' @param model.args list of additional arguments to use in model fitting
#' 
#' @return a data frame with new columns based on model fit
#' 
#' @export
#' 
augment_predict <- function(.data, model, formula, model.args = NULL, overwrite = FALSE) {
  .data %>% 
    do(augment(do.call(model, c(list(formula=formula, data=.), model.args)), .)) %>%
    rename_(.dots = setNames(names(.), gsub("^\\.", paste0(deparse(formula[[2]]), "."), names(.))))
}



#' ggplot helper for plotting ribbon, taking a stats string or function set
#' as an argument
#' 
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#' 
#' @param mapping a ggplot aesthetic mapping
#' @param data data to pass to ggplot2::stat_summary functions
#' 
#' @return a list of ggplot calls to chain into a ggplot
#' 
#' @examples
#' library(tidyverse) # using lubridate, dplyr
#' 
#' # data prep
#' nasa %>% as_tibble %>% 
#'   mutate(date = ymd("0000/01/01") + months(month) + years(year)) %>%
#'   mutate(hemisphere = ifelse(lat>0,"Northern", "Southern")) %>%
#'   mutate(temperature = temperature - 273) %>%
#'  
#'  # plotting
#'  ggplot() + 
#'    aes(x=date, y=temperature) + 
#'    geom_stat_ribbons() + 
#'    facet_grid(hemisphere ~ .)
#'   
#' @export
#' 
geom_stat_ribbons <- function(mapping = NULL, data = NULL, show.counts = FALSE, show.counts.min = 0,
                              fun.data = c('tukey_hinges', 'tukey_whiskers'), ...) {
  
  .dots <- list(...)
  fun.data <- parse.fun.data(fun.data)
  
  as.list(c(
    
    # loop through stats to plot as ribbons
    lapply(fun.data, function(f) { 
      do.call(stat_summary, 
        modifyList(.dots, 
          list(geom = 'ribbon', fun.data = f, colour = NA,
               alpha = 1 / (2 ** (length(f) + 1)) * dplyr:::`%||%`(.dots$alpha, 1.0)) ))
    }),
    
    # plot line along stat y
    do.call(stat_summary,
      modifyList(.dots, list(fun.data = fun.data[[1]], geom = "line") )),
  
    # add labels of group counts
    if (show.counts) {
      do.call(stat_summary,
        modifyList(.dots, 
          list(fun.data = fun.data[[1]], 
               geom = ifelse(require(ggrepel, quietly = T), "label_repel" , "label"),
               nudge_y = 0.1, label.size = 0, alpha = 0.7) ))
    } else NULL
    
  ))
}



#' helper function to interpret strings and functions passed to geom_stat_ribbons
#' 
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#' 
#' @param format either a function returning a named list of stat transformations or 
#'               a string matching to one of 'mean_se', 'median_iqr', 'tukey_hinges', 
#'               'tukey_whiskers' or 'quantile_contour', or a list of strings from 
#'               this group. The y calculation of the first function in the list 
#'               returned is used for placing labels.
#' 
#' @return a list of functions to apply to pass to fun.data in stat_summary
#' 
parse.fun.data <- function(format) { 
  if (is.character(format)) {
    unlist(lapply(format, function(f) switch(f,
      
      mean_sd = function(d) c(
        y = mean(d),
        ymin = mean(d) - sd(d),
        ymax = mean(d) + sd(d),
        label = length(d)),                                       
      
      mean_se = function(d) c(
        y = mean(d), 
        ymin = mean(d) - sd(d)/sqrt(length(d)), 
        ymax = mean(d) + sd(d)/sqrt(length(d)),
        label = length(d)),
      
      median_iqr = function(d) c(
        y = median(d), 
        ymin = median(d) - IQR(d), 
        ymax = median(d) + IQR(d),
        label = length(d)),
      
      tukey_hinges = function(d) c(
        y = median(d),
        ymin = as.numeric(quantile(d, 0.25)),
        ymax = as.numeric(quantile(d, 0.75)),
        label = length(d)),
      
      tukey_whiskers = function(d) c(
        y = median(d),
        ymin = max(min(d), as.numeric(quantile(d, 0.25)) - 1.58 * IQR(d) / sqrt(length(d))),
        ymax = min(max(d), as.numeric(quantile(d, 0.75)) + 1.58 * IQR(d) / sqrt(length(d))),
        label = length(d)),
      
      quantile_contour = lapply(seq(0.1, 0.4, 0.1), function(q) { 
        function(d) c(
          y = median(d),
          ymin = as.numeric(quantile(d, q)),
          ymax = as.numeric(quantile(d, 1 - q)),
          label = length(d)
        ) })
      
    )))
  } else if (is.function(format)) list(format) 
  else if (is.list(format) && is.function(format[[1]])) format
  else stop("fun.data must be either a string, function or list of functions")
}

