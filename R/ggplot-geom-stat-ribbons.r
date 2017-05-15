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
  fun.data <- stat.summmary.funs(fun.data)
  
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
