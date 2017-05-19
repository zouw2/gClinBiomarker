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
#' @param data data to pass to ggplot2::stat_summary functions
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
ggpk_stat_line_errorbar <- function(mapping = NULL, data = NULL, show.counts = FALSE,
                                    fun.data = 'mean_se', fun.args = list(), ...) {

  defaults <- list(errorbar.position = position_dodge(width = 0.9))

  .dots <- modifyList(defaults, list(...))
  fun.data <- stat_summary_funs(fun.data, fun.args)

  ## wrap ## ribbons
  # reduce through list of ribbon geoms and collect sum
  Reduce(`+`, mapply(function(f, i) {
    ggpack(stat_summary, 'errorbar', .dots,
           geom = 'linerange',
           fun.data = f,
           size = .dots$errorbar.size %||% rel(2) * (1 - (i - 1) / length(fun.data)),
           position = position_dodge(rel(0.33)),
           alpha = 0.5)
  }, f = fun.data, i = 1:length(fun.data)) ) +

  ## wrap ## line
  # plot line along stat y
  ggpack(stat_summary, 'line', .dots,
      geom = 'line',
      fun.data = fun.data[[1]]) +

  ## wrap ## label
  # add labels of group counts
  if (isTRUE(show.counts) || show.counts == 'label') {
    ggpack(stat_summary, 'label', .dots,
      fun.data = fun.data[[1]],
      direction = "y",
      nudge_y = 0.1,
      label.size = 0,
      geom = ifelse(require(ggrepel, quietly = T), "label_repel" , "label"),
      fill = 'white',
      alpha = 0.85)
  } else if (show.counts == 'table') {
    ggpack(stat_summary, 'label', .dots,
      fun.data = function(d) c(y=Inf, label = length(d)),
      geom = 'text_repel',
      direction = "y",
      segment.colour = NA,
      show.legend = FALSE)
  } else NULL

}
