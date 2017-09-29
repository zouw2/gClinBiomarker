#' ggplot wrapper for plotting ribbon, taking a stats string or function set
#' as an argument
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#'
#' @note When specifying arguments for wrapped ggplot geometries, prefix
#' the parameter with geometry specifiers: "ribbons" for ribbon geometry
#' "line" for central line geometry, or "label" for count labels. e.g.
#' \code{ggpk_stat_line_errorbar(ribbons.alpha = 0.8, line.alpha = 0.6)}
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
#'     ggpk_stat_line_errorbar(fun.data = 'deciles') +
#'     facet_grid(hemisphere ~ .) +
#'     labs(title="Temperature by Hemisphere")
#'
#' @export
#'
ggpk_line_errorbar <- function(mapping = NULL, data = NULL, show.counts = FALSE,
                               fun.data = 'mean_se', fun.args = list(),
                               position = ggplot2::position_dodge(width = ggplot2::rel(0.5)),
                               ...) {

  fun.data <- stat_summary_funs(fun.data, fun.args)

  # prep dots arguments with default values, overwritten with ellipses args
  defaults <- list(
    ggpk_line_errorbar.stat = 'summary',
    errorbar.alpha = 0.5,
    errorbar.size = ggplot2::rel(2),
    errorbar.linetype = 1,
    errorbar.position = position,
    point.position = position,
    line.position = position)
  .dots <- modifyList(defaults, substitute(...()))

  # add some conditional arguments to handle stat behavior
  if (!('identity' %in% .dots[c('ggpk_line_errorbar.stat', 'point.stat')]))
    .dots$point.fun.data <- last(fun.data)

  if (!('identity' %in% .dots[c('ggpk_line_errorbar.stat', 'line.stat')]))
    .dots$line.fun.data <- last(fun.data)

  if (!('identity' %in% .dots[c('ggpk_line_errorbar.stat', 'label.stat')]))
    .dots$label.fun.data <- last(fun.data)


  ## pack ## ribbons
  # use standard errorbar geom if errorbar.stat is 'identity'
  (if ('identity' %in% .dots[c('ggpk_line_errorbar.stat', 'errorbar.stat')])
    ggpack(ggplot2::geom_errorbar, c('ggpk_line_errorbar', 'errorbar'), .dots,
           width = 0)

    # reduce through list of errorbar geoms and collect sum
    else
      Reduce(`+`, mapply(function(f, i) {
        ggpack(ggplot2::geom_errorbar, c('ggpk_line_errorbar', 'errorbar'), .dots,
               width = 0,
               fun.data = f,
               size = .dots$errorbar.size %||% ggplot2::rel(2) *
                 (1 - (i - 1) / length(fun.data)))
      }, f = fun.data, i = 1:length(fun.data))) ) +

    ## pack ## point
    # plot point along stat y
    ggpack(ggplot2::geom_point, c('ggpk_line_errorbar', 'point'), .dots) +

    ## pack ## line
    # plot line along stat y
    ggpack(ggplot2::geom_line, c('ggpk_line_errorbar', 'line'), .dots) +

    ## pack ## label
    # add labels of group counts
    if (isTRUE(show.counts) || show.counts == 'label') {
      if (require(ggrepel, quietly = TRUE)) {
        ggpack(ggrepel::geom_label_repel, c('ggpk_line_errorbar', 'label'), .dots,
               direction = "y",
               nudge_y = 0.1,
               label.size = 0,
               fill = 'white',
               alpha = 0.85)
      } else
        ggpack(ggplot2::geom_label, c('ggpk_line_errorbar', 'label'), .dots,
               label.size = 0,
               fill = 'white',
               alpha = 0.85)
    } else if (show.counts == 'table') {
      ggpack(geom_text_table, c('ggpk_line_errorbar', 'label'), .dots, show.legend = FALSE)
    } else NULL

}
