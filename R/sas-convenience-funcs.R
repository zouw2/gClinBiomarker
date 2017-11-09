#' Compute LSMEANS to mirror SAS functionality
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}, Christina Rabe
#'   \email{rabe.christina@gene.com}
#'
#' @param model.obj A model object on which to calculate lsmeans
#' @param specs specs to be passed to lsmeans()
#' @param mode mode to be passed to lsmeans()
#' @param as.data.frame whether to return a lsm.list object or a data.frame
#' @param quietly whether to hide message informing of changes to default
#'   functionality
#' @param ... additional arguments to be passed to lsmeans
#'
#' @importFrom lsmeans lsmeans
#' @importFrom broom tidy
#' @import dplyr
#'
#' @examples
#' library(dplyr) # for %>% operator
#'
#' # create model on which to calculate lsmeans
#' gls(mpg ~ hp + carb + wt,
#'     data = mtcars %>%
#'              mutate(carb = as.factor(carb)), # make sure contrast is factor
#'     na.action = na.exclude) %>%
#'
#' # call lsmeans, specifying factors over which means should be calculated
#' sas.lsmeans(~ carb)
#'
#' @export
sas.lsmeans <- function(model.obj, specs, data = NULL, mode = 'kenward-roger',
  quietly = FALSE, verbose = FALSE, ...) {

  if (!quietly) message(paste(
    "To mirror SAS functionality:",
    " 1. mode of 'kenward-roger' will be used unless otherwise specified.",
    " 2. data used for initial model fit...",
    "   a. is filtered for only complete cases in the response variable",
    "   b. will have any numeric variables in specs converted to categorical",
    " 3. grouping variables for lsmeans converted to factor.",
    "      (creating a new factor column where necessary)",
    " 4. continuous means calculated using rows where response is not NA.",
    " 5. categorical level weights from all rows of dataset\n",
    sep = "\n"))

  model.call <- as.list(model.obj$call)
  model      <- append_split_terms(as.formula(get_model_terms(model.obj)))
  model.env  <- attr(model$terms, ".Environment")

  # prevent model that doesn't store model construction without explicit data
  if (is.null(data) && identical(model.env, environment()))
    stop(paste(
      "\nModel environment was not found within model object. ",
      "To resolve, pass source data to sas.lsmeans as well.", sep = "\n"))

  specs         <- clean_lsmeans_specs(specs) # ~ x | y => pairwise ~ x | y
  specs.pred    <- get_lsmeans_specs_predictors(specs) # c('x', 'y')
  specs.pred.nf <- Map(class, Filter(Negate(is.factor), data[specs.pred]))

  # filter dataset down to complete cases, coerce spec vars to factor
  if (is.null(data)) data <- eval(substitute(model.call$data), envir=model.env)
  cleaned_data <- data %>%
    dplyr::filter_at(vars(unlist(model$vars)), all_vars(!is.na(.))) %>%
    dplyr::mutate_at(.vars = as.character(names(specs.pred.nf)), .funs = as.factor)

  if (!quietly & nrow(data) != nrow(cleaned_data)) message(sprintf(
    "%d records with incomplete data removed in model fit. (%1.2f%%)\n",
     nrow(data) - nrow(cleaned_data),
    (nrow(data) - nrow(cleaned_data)) / nrow(data) * 100))

  if (verbose) message(sprintf(
    '%s model is being refit with arguments: \n%s\n',
    as.character(model.call[[1]]),
    print_to_string(model.call[-1], condense = TRUE)))

  model.call$data <- cleaned_data
  sas.model.obj <- do.call(as.character(model.call[[1]]), model.call[-1])

  # split non-grouping model covariates into numeric and non-numeric lists
  non.spec.vars <- setdiff(all.vars(model$terms), c(specs.pred, model$resp))
  covs.f <- names(Filter(Negate(is.numeric), data[non.spec.vars]))
  covs.n <- names(Filter(       is.numeric , data[non.spec.vars]))

  lsmeans_args <- modifyList(
    list(object = sas.model.obj, specs = specs, mode = mode, data = cleaned_data,
         at = cleaned_data %>% sas_lsmeans_at(covs.n)),
    list(...)) %>% append_weights_as_freq(covs.f, data = data, verbose = verbose)

  if (verbose) {
    message('Running lsmeans::lsmeans() with parameters: ')
    message(print_to_string(list(object = summary(lsmeans_args$object))))
    message(print_to_string(lsmeans_args[setdiff(names(lsmeans_args), 'object')]))
  }

  lsmlist <- do.call(lsmeans::lsmeans, lsmeans_args)
  lsmlist$lsmeans@grid %<>%
    coerce_vars(specs.pred.nf, suffix = '.lsmeans.factor', verbose = verbose) %>%
    copy_variable_attributes(data)
  lsmlist$unary.vars.gdf <- summarize_unary_vars(data, specs.pred)
  lsmlist
}

sas_lsmeans_at <- function(data, means.at) {
  data %>%
    dplyr::summarise_at(as.character(means.at), dplyr::funs(mean)) %>%
    as.list
}

coerce_vars <- function(data, class_list, preserve = TRUE, suffix = '.old', verbose = FALSE) {
  if (length(class_list) == 0) return(data)

  if (verbose) message(
    sprintf('Coercing variables according to: %s\n\n',
    print_to_string(class_list)))

  data %>%
    dplyr::mutate_(.dots =
      if (!preserve) list()
      else setNames(names(class_list), paste0(names(class_list), suffix))) %>%
    dplyr::mutate_(.dots =
      Map(function(n, c) sprintf("as.%s(levels(%s)[%s])", c, n, n),
      names(class_list), class_list))
}

copy_variable_attributes <- function(obj, attr_src_obj) {
  for (n in intersect(names(obj), names(attr_src_obj)))
    attributes(obj[[n]]) <- attributes(attr_src_obj[[n]])
  obj
}

summarize_unary_vars <- function(data, ..., .dots = c()) {
  groups <- c(unlist(list(...)), .dots)
  data %>%
    dplyr::group_by_(.dots = groups) %>%
    dplyr::select(c(
      groups,
      dplyr::summarise_all(., funs(n_distinct)) %>%
        dplyr::select_if(funs(all(. == 1))) %>%
        names
    )) %>% dplyr::slice(1)
}

append_weights_as_freq <- function(lsmeans_args, ..., data = NULL, verbose = FALSE) {
  if ('weights' %in% names(lsmeans_args)) return(lsmeans_args)
  factor_vars <- unlist(list(...))

  levels_args <- modifyList(lsmeans_args, list(weights = 'show.levels'))
  levels <- sink_to_temp(do.call(lsmeans, levels_args))

  if (is.null(names(levels)) || length(factor_vars) < 1)
    return(modifyList(lsmeans_args, list(weights = 'proportional')))

  if (verbose) message(sprintf('Calculating weights over levels: %s',
    paste(names(levels), collapse = ', ')))

  level_weights <- dplyr::left_join(
      levels,
      broom::tidy(table((data %||% lsmeans_args$data)[names(levels)], dnn=names(levels))),
      by = names(levels))

  if (verbose) message(print_to_string(level_weights), '\n')

  lsmeans_args$weights <- level_weights %>% dplyr::pull(Freq)
  lsmeans_args
}

get_model_terms <- function(model.obj) {
  # return formula if model took an argument named with one of the following
  possible.names <- c('model', 'terms', 'formula')

  # search in the model object itself (WILL contain model env)
  mdl.ind <- first(na.omit(c(NA, match(possible.names, names(model.obj[-1])))))
  if(!is.na(mdl.ind))
    return(model.obj[[mdl.ind+1]])

  # pull from model call (will NOT contain model env)
  model.call  <- as.list(model.obj$call)
  if (length(model.call) <= 1)
    stop('Model object does not contain any arguments. Cannot discern model formula')
  mdl.ind <- first(na.omit(c(NA, match(possible.names, names(model.call[-1])))))
  if (!is.na(mdl.ind))
    return(model.call[[mdl.ind+1]])

  # try to cast the first argument to formula (will NOT contain model env)
  tryCatch({ as.formula(model.call[[2]]); model.call[[2]] },
    error = function(e) {
      warning("Attempted to coerce first argument to formula but failed.")
      e
    })
}

append_split_terms <- function(t) {
  m <- list(terms = terms(t))
  if (attr(m$terms, 'response')) {
    m$resp <- all.vars(m$terms[[2]])
    m$covs <- all.vars(m$terms[[3]])
  } else m$covs <- all.vars(m$terms[[2]])
  m$vars <- as.list(c(m$resp, m$covs))
  m
}

clean_lsmeans_specs <- function(specs) {
  if (inherits(specs, 'formula') && !attr(terms(specs), 'response'))
    as.formula(paste('pairwise ~', as.character(specs)[[2]]))
  else specs
}

get_lsmeans_specs_predictors <- function(specs) {
  if (inherits(specs, "formula"))
    all.vars(delete.response(terms(specs)))
  else if (class(specs) == 'character') specs
  else if (class(specs) == 'list')
    stop('Passing list as specs is currently unsupported in sas.lsmeans. Please file an issue if this is functionality you require.')
  else stop('Unable to parse variables from provided specs')
}

#' @method as.data.frame lsm.list
#' @export
as.data.frame.lsm.list <- function(lsm) {
  if ('unary.vars.gdf' %in% names(lsm)) {
    dplyr::left_join(
      as.data.frame(lsm$lsmeans),
      lsm$unary.vars.gdf %>% dplyr::ungroup(),
      by = attr(lsm$unary.vars.gdf, 'vars'))
  } else
    as.data.frame(lsm$lsmeans)
}

#' @method as.data.frame lsmobj
#' @export
as.data.frame.lsmobj <- function(lsmo) {
  as.data.frame(summary(lsmo)) %>%
    left_join(lsmo@grid %>% select(c(names(lsmo@levels), '.wgt.')),
              by = names(lsmo@levels))
}

#' @method tidy lsm.list
#' @export
tidy.lsm.list <- function(lsm) { broom::tidy(lsm@lsmeans) }
