#' Adds prediction outputs to data based on model and formula
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#'
#' @param .data data to use for model fitting
#' @param model model function to use (e.g `lm`)
#' @param fomula formula to use for model fitting (e.g `mpg ~ gear + wt`)
#' @param model.args list of additional arguments to use in model fitting
#' @param model.per a formula representing all terms to group data by to
#' fit individual models (e.g. `~ am + vs`)
#'
#' @return a data frame with new columns based on model fit
#'
#' @importFrom broom augment
#' @importFrom dplyr rename rename_ group_by group_by_
#'
#' @export
#'
augment_predict <- function(.data, model, model.per = NULL, ...) {
  .dots <- filter_args('model', list(...))
  fy <- deparse(.dots$formula[[2]]) # get formula independent variable

  .data %>%
    dplyr::group_by_(.dots = all.vars(model.per)) %>%
    do(broom::augment(do.call(model, c(list(data=.), .dots)), .)) %>%
    # rename model outputs (".fitted") to instead begin with var ("y.fitted")
    dplyr::rename_(.dots = setNames(names(.), gsub("^\\.", paste0(fy, "."), names(.))))
}



#' Filter only specific columns, leaving the rest of the dataframe intact
#'
#' Useful for filtering down to a single datapoint in long data formats, for
#' instance when selecting baseline values when visitation date is a specific
#' code.
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#'
#' @param .tbl a dataframe or tibble to be sent to mutate
#' @param .cols a dplyr::vars() column selection similar to `dplyr::select()` or
#'              numeric indices of columns to select
#' @param ... filtering functions to filter column data
#'
#' @return a dataframe with the specified columns filtered by the specified conditions
#'
#' @export
#'
na_if_at <- function(.tbl, .cols, ...) {
  cols <- dplyr:::select_colwise_names(.tbl, .cols)
  f <- lazyeval::lazy_dots(...)
  f <- Reduce(function(l,r) lazyeval::interp(lazyeval::lazy(llz & rlz), llz=l, rlz=r), f)
  f <- funs_(lazyeval::interp(lazyeval::lazy(ifelse(c, ., NA)), c=f))
  vars <- dplyr:::colwise_(.tbl, f, cols)
  mutate_(.tbl, .dots = vars)
}
