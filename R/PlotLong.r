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
#' @param ... additional arguments passed to underlying ggplot components
#' by prefixing value with ggplot call name. ("ribbons", "line", "text", "facet", "xlab",
#' "ylab", "labs" or "theme" - e.g. `ribbons.color = 'red'`)
#'
#' @return a ggplot object with y values adjusted based on model function if provided
#'
#' @examples
#' library(tidyverse) # for pipe and tibble
#' library(broom)     # for augment
#'
#' # default representation
#' PlotLong(nasa %>% as_tibble, aes(x=month, y=temperature))
#'
#' # a more appropriate representation for large n
#' PlotLong(nasa %>% as_tibble, aes(x=month, y=temperature),
#'          fun.data = 'quartiles')
#'
#' # include linear adjustment accounting for amount of ozone
#' PlotLong(nasa %>% as_tibble, aes(x=month, y=temperature),
#'          formula = temperature ~ ozone, fun.data = 'deciles',
#'          show.counts = T)
#'
#' # adjusting by independent models for the northern and southern hemispheres
#' PlotLong(nasa %>% as_tibble %>% mutate(hemi=ifelse(lat>0, "North", "South")),
#'          aes(x=month, y=temperature), formula = temperature ~ ozone,
#'          model.per = ~ hemi, facet.fun = ~ hemi, fun.data = 'deciles',
#'          xlab = "Month", ylab = "Temperature Adjusted for Ozone",
#'          labs.title = "Temperature by Hemisphere",
#'          labs.caption = "*idependent models fit per hemisphere")
#'
#' @export
#'
PlotLong <- function(data, mapping, formula = NULL, model = lm, model.args = NULL,
                     model.per = NULL, facet.fun = NULL,
                     plot.style = 'ribbons', ...) {

  if (!is.null(formula))  {
    # add predicted values based on formula provided
    data <- augment_predict(data, model, formula, model.args, model.per)
    # overwrite mapping with new fitted variable ("<y>.fitted") from augment_predict
    mapping$y <- as.name(paste(deparse(mapping$y), "fitted", sep="."))
  }

  # plot using geom_stat_ribbons, passing extra arguments to geom
  data %>% ggplot() + mapping +
    (if      (plot.style == 'ribbons') ggpk_stat_ribbon(...)
     else if (plot.style == 'errorbars') ggpk_stat_line_errorbar(...)) +
    (if (is.null(facet.fun)) facet_null()
     else ggpack(facet_grid, 'facet', list(...), facets = facet.fun)) +
    (if (is.null(formula)) NULL
     else ylab(paste("Adjusted", deparse(formula[[2]])))) +
    ggpack(xlab,  'xlab',  list(...), null.empty = T) +
    ggpack(ylab,  'ylab',  list(...), null.empty = T) +
    ggpack(labs,  'labs',  list(...), null.empty = T) +
    ggpack(theme, 'theme', list(...), null.empty = T)

}
