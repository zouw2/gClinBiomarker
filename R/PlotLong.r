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
#' @param ... additional arguments are handled in one of two ways. First,
#' arguments which can be used as any of ggplot's default aesthetics will be
#' pulled from ... args. Second, remaining arguments are passed to underlying
#' ggplot components by prefixing value with ggplot call name. ("ribbons",
#' "line", "text", "facet", "xlab", "ylab", "labs" or "theme" - e.g.
#' `ribbons.color = 'red'`)
#'
#' @return a ggplot object
#'
#' @examples
#' # load data
#' nasa.data <- as.data.frame(dplyr::nasa)
#'
#' # default representation
#' PlotLong(nasa.data, x = month, y = temperature)
#'
#' # a more appropriate representation for large n
#' PlotLong(nasa.data, x = month, y = temperature, fun.data = 'quartiles')
#'
#' # include linear adjustment accounting for amount of ozone
#' PlotLong(nasa.data, x = month, y = temperature,
#'          formula = temperature ~ ozone, fun.data = 'deciles',
#'          show.counts = T)
#'
#' # adjusting by independent models for the northern and southern hemispheres
#' library(dplyr) # needed for %>%
#' PlotLong(nasa.data %>% mutate(hemi=ifelse(lat>0, "North", "South")),
#'          x = month, y = temperature, formula = temperature ~ ozone,
#'          model.per = ~ hemi, facet.fun = ~ hemi, fun.data = 'deciles',
#'          xlab = "Month", ylab = "Temperature Adjusted for Ozone",
#'          labs.title = "Temperature by Hemisphere",
#'          labs.caption = "*idependent models fit per hemisphere")
#'
#' # including a table of value counts and subsetting value data to specific
#' # months
#' library(dplyr) # needed for %>%
#' PlotLong(nasa.data %>% mutate(hemisphere=ifelse(lat > 0, "North", "South")),
#'          x = month, y = temperature, group = hemisphere,
#'          color = hemisphere, fill = hemisphere,
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
#' @import ggplot2 dplyr
PlotLong <- function(data, mapping = NULL, model = lm, model.per = NULL,
                     model.formula = NULL, facet.fun = NULL,
                     plot.style = 'ribbons', ...) {

  if (is.null(mapping)) {
    args <- split_aes_from_dots(...)
    mapping <- args$aes; .dots <- args$not_aes
  } else .dots <- list(...)

  if (!is.null(model.formula))  {
    # ensure model variables reflect plotted variables
    if (deparse(mapping$y) != model.formula[[2]])
      stop('Independent model.formula variable must be the same as y aesthetic')

    # add model fit output to data
    data <- do.call(augment_predict,
        c(list(data, model, model.per, model.formula=model.formula), .dots))

    # change aesthetic y to be fitted values
    mapping$y <- as.name(sprintf("%s.fitted", deparse(mapping$y)))
  }

  # collapse linetype to group to allow for ggpack overrides (errorbar color)
  mapping <- flatten_aesthetics_to_group(mapping, 'linetype')

  if (all(c('ymin', 'ymax', 'y') %in% names(mapping)))
    .dots <- modifyList(.dots, list(
      ggpk_ribbons.stat = 'identity',
      ggpk_line_errorbar.stat = 'identity'))

  # plot using geom_stat_ribbons, passing extra arguments to geom
  data %>% ggplot2:::ggplot() + mapping +

    # plot with specified styling
    (if      (plot.style == 'ribbons')   do.call(ggpk_ribbons, .dots)
     else if (plot.style == 'errorbars') do.call(ggpk_line_errorbar, .dots)) +

    # handle optional facetting
    (if (is.null(facet.fun)) ggplot2::facet_null()
     else ggpack(ggplot2::facet_grid, 'facet', .dots, facets = facet.fun)) +

    # adjust label to accommodate model fitting if a model was used
    (if (is.null(model.formula)) NULL
     else ylab(paste("Adjusted", deparse(model.formula[[2]])))) +

    # catch plot labels
    do.call(ggpack.decorators, .dots)

}
