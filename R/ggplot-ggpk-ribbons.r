#' ggplot wrapper for plotting ribbon, taking a stats string or function set
#' as an argument
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#'
#' @note When specifying arguments for wrapped ggplot geometries, id
#' the parameter with geometry specifiers: "ribbon" for ribbon geometry
#' "line" for central line geometry, or "label" for count labels. e.g.
#' \code{ggpk_stat_ribbon(ribbon.alpha = 0.8, line.alpha = 0.6)}
#'
#' @param mapping a ggplot aesthetic mapping
#' @param data data to pass to ggplot2::ggplot2::stat_summary functions
#' @param show.counts True, False, "label" or "table" to display either as
#' labels at each datapoint or as a table at the top of the plot
#'
#' @return a ggpacket object that can be added to any ggplot to
#' draw a collection of layers
#'
#' @examples
#' library(tidyverse) # for dplyr, ggplot
#' library(lubridate)
#'
#' nasa %>% as_tibble %>%
#'   mutate(date = ymd("0000/01/01") + months(month) + years(year)) %>%
#'   mutate(hemisphere = ifelse(lat>0,"Northern", "Southern")) %>%
#'   mutate(temperature = temperature - 273) %>%
#'
#'   # plotting
#'   ggplot() +
#'     aes(x=date, y=temperature) +
#'     ggpk_ribbons(fun.data = 'deciles') +
#'     facet_grid(hemisphere ~ .) +
#'     labs(title="Temperature by Hemisphere")
#'
#' @export
#'
ggpk_ribbons <- function(
  mapping = NULL, data = NULL, show.counts = FALSE, fun.data = 'mean_se',
  fun.args = list(), id = 'ggpk_ribbons', ...) {

  fun.data <- stat_summary_funs(fun.data, fun.args)
  dots <- substitute(...())

  # add some conditional arguments to handle stat behavior
  if (!('identity' %in%  dots[c(paste0(id, '.stat'), 'label.stat')]))
     dots$label.fun.data <- last(fun.data)
  if (!('identity' %in%  dots[c(paste0(id, '.stat'), 'point.stat')]))
     dots$point.fun.data <- last(fun.data)
  if (!('identity' %in%  dots[c(paste0(id, '.stat'), 'line.stat')]))
     dots$line.fun.data <- last(fun.data)

  ## pack ## ribbon
  # use standard ribbon geom if ribbon.stat is 'identity'
  (if ('identity' %in%  dots[c(paste0(id, '.stat'), 'ribbon.stat')])
    ggpack(ggplot2::geom_ribbon, id = c(id, 'ribbon'),
           stat = 'summary', dots =  dots, color = NA,
           alpha = 0.85 / 3 * ( dots$ribbon.alpha %||% 1))

    # reduce through list of ribbon geoms and collect sum
    else
      Reduce(function(l, r) { l +
          ggpack(ggplot2::geom_ribbon, id = c(id, 'ribbon'),
            stat = 'summary', dots =  dots, fun.data = r, color = NA,
            alpha = 0.85 / (length(fun.data) + 2) * ( dots$ribbon.alpha %||% 1))
      }, fun.data, init = NULL) ) +

    ## pack ## point
    # plot point along stat y
    ggpack(ggplot2::geom_point, id = c(id, 'point'),
           stat = 'summary', dots =  dots) +

    ## pack ## line
    # plot line along stat y
    ggpack(ggplot2::geom_line, id = c(id, 'line'),
           stat = 'summary', dots =  dots) +

    ## pack ## label
    # add labels of group counts
    if (isTRUE(show.counts) || show.counts == 'label') {
      if (require(ggrepel, quietly = TRUE))
        ggpack(ggrepel::geom_label_repel, id = c(id, 'label'),
          stat = 'summary', dots =  dots, direction = "y",
          nudge_y = 0.1, label.size = 0, fill = 'white', alpha = 0.85)

      else
        ggpack(ggplot2::geom_label, id = c(id, 'label'),
          stat = 'summary', dots =  dots, label.size = 0,
          fill = 'white',alpha = 0.85)

    } else if (show.counts == 'table') {
      ggpack(geom_text_table, id = c(id, 'label'),
          stat = 'summary', dots =  dots, show.legend = FALSE)
    } else NULL

}



