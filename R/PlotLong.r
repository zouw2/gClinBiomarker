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
#' by prefixing value with ggplot call name. ("ribbons", "line", "text",
#' "facet", "xlab", "ylab", "labs" or "theme" - e.g. `ribbons.color = 'red'`)
#'
#' @return a ggplot object
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
#' # including a table of value counts and subsetting value data to specific
#' # months
#' PlotLong(nasa %>%
#'            as_tibble %>%
#'            mutate(hemisphere=ifelse(lat > 0, "North", "South")),
#'          aes(x=month, y=temperature, group = hemisphere,
#'              color = hemisphere, fill = hemisphere),
#'          formula = temperature ~ ozone,
#'          model.per = ~ hemisphere, fun.data = 'deciles',
#'          show.counts = 'table',
#'          label.data = . %>% filter(month %in% c(1, 6, 12)),
#'          label.hjust = 'inward',
#'          xlab = "Month", ylab = "Temperature Adjusted for Ozone",
#'          labs.title = "Temperature by Hemisphere",
#'          labs.caption = "*idependent models fit per hemisphere")
#'
#' @export
#'
PlotLong <- function(data, mapping, model = lm, model.per = NULL,
                     model.formula = NULL, facet.fun = NULL,
                     plot.style = 'ribbons', ...) {

  if (!is.null(model.formula))  {
    # add predicted values based on formula provided
    data <- data %>%
      augment_predict(
        model,
        model.per = model.per,
        model.formula = model.formula,
        ...)

    # overwrite mapping with residuals after model adjustment ("<y>.resid")
    # from augment_predict
    mapping$y <- as.name(paste(deparse(mapping$y), "resid", sep="."))
  }

  # collapse linetype to group since ggpk_line_errorbar will set it to constant
  # for the errorbar geom
  mapping <- ggpack_flatten_aesthetics_to_group(mapping, 'linetype')

  # plot using geom_stat_ribbons, passing extra arguments to geom
  data %>% ggplot() + mapping +

    # plot with specified
    (if      (plot.style == 'ribbons')   ggpk_ribbons(...)
     else if (plot.style == 'errorbars') ggpk_line_errorbar(...)) +

    # handle optional facetting
    (if (is.null(facet.fun)) facet_null()
     else ggpack(facet_grid, 'facet', list(...), facets = facet.fun)) +

    # adjust label to accommodate model fitting if a model was used
    (if (is.null(model.formula)) NULL
     else ylab(paste("Adjusted", deparse(model.formula[[2]])))) +

    # catch plot labels
    ggpack.decorators(...)

}
