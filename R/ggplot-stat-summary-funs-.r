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
stat.summary.funs <- function(format, args) { 
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
      
      deciles = stat.summary.iles(seq(0.0, 0.4, 0.1)),
      
      quartiles = stat.summary.iles(c(0, 0.25)),
      
      quantiles = do.call(stat.summary.iles, args)
  
    )))
  } else if (is.function(format)) { 
    if (is.null(args)) 
      list(format) 
    else 
      list(do.call(format, args))
  } else if (is.list(format) && is.function(format[[1]])) format
  else stop("fun.data must be either a string, function or list of functions")
}

stat.summary.iles <- function(s) {
  lapply(s, function(q) { 
    function(d) c(
      y = median(d),
      ymin = as.numeric(quantile(d, q)),
      ymax = as.numeric(quantile(d, 1 - q)),
      label = length(d)
    ) })
}