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
#' @export
#' 
PlotLong <- function(data, mapping, formula = NULL, model = lm, 
                     model.args = NULL, facet.fun = NULL, ...) {
  
    if (!is.null(formula))  { 
      # add predicted values based on formula provided
      data <- augment_predict(data, model, formula, model.args)
      # overwrite mapping with new fitted variable ("<y>.fitted") from augment_predict
      mapping$y <- as.name(paste(deparse(mapping$y), "fitted", sep="."))
    }
    
    # plot using geom_stat_ribbons, passing extra arguments to geom
    data %>% ggplot() + mapping + 
      do.call(ggwrap_stat_ribbons, list(...)) +
      (if (!is.null(facet.fun)) facet_grid(facet.fun)
       else facet_null() ) +
      (if (!is.null(formula)) ylab(paste("Adjusted", deparse(formula[[2]])))
       else NULL) +
      ggwrap(xlab, 'xlab', list(...), null.empty = TRUE) +
      ggwrap(ylab, 'ylab', list(...), null.empty = TRUE)
  
}


PlotLong(nasa %>% as_tibble, aes(x=month, y=temperature),
         formula = temperature ~ ozone, fun.data = 'deciles',
         show.counts = T, xlab="test")

