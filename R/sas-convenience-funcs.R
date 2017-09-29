#' Compute LSMEANS to mirror SAS functionality
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}, Christina Rabe \email{rabe.christina@gene.com}
#'
#' @param model.obj A model object on which to calculate lsmeans
#' @param specs specs to be passed to lsmeans()
#' @param mode mode to be passed to lsmeans()
#' @param as.data.frame whether to return a lsm.list object or a data.frame
#' @param quietly whether to hide message informing of changes to default functionality
#' @param ... additional arguments to be passed to lsmeans
#'
#' @importFrom lsmeans lsmeans
#' @importFrom broom tidy
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
sas.lsmeans <- function(model.obj, specs, mode = 'kenward-roger',
                        quietly = FALSE, verbose = FALSE, ...) {

    if (!quietly) message(paste(
        "To mirror SAS functionality:",
        " - mode of 'kenward-roger' will be used unless otherwise specified.",
        " - model.obj will be fit with only complete response variables.",
        " - response variables for lsmeans converted to factor.",
        "     (creating a new factor column where necessary)",
        " - continuous means calculated using rows where respose is not NA.",
        " - categorical level weights from all rows from model dataset\n",
        sep = "\n"))

    # get model.obj call
    model.call  <- as.list(model.obj$call)

    # try to get model formula from model object, defaulting to first arg
    # if a 'model' or 'formula' argument are not found
    if ('model' %in% names(model.call))
        model.form <- as.formula(model.call$model)
    else if ('formula' %in% names(model.call))
        model.form <- as.formula(model.call$formula)
    else if (length(model.call) > 1)
        model.form <- as.formula(model.call[[2]])
    else stop('Model formula parameter not found in model object')
    model.form.trms <- terms(model.form)

    # collect variables defined from the specs input
    if (inherits(specs, "formula")) {
      specs.dep.vars <- all.vars(delete.response(terms(specs)))
      # edit specs to preface with 'pairwise' if not provided to lsmeans
      if (attr(terms(specs), 'response'))
        specs <- as.formula(paste('pairwise', labels(terms(specs)), sep=" ~ "))
    } else if (class(specs) == 'character')
      specs.dep.vars <- specs
    else if (class(specs) == 'list')
      specs.dep.vars <- unlist(specs, use.names = F)
    else stop('Unable to parse variables from provided specs')

    # ensure model has a response variable
    if (length(model.form) < 3)
      stop('model must be a formula of the form "resp ~ pred"')

    data <- model.call$data # original, complete dataset
    resp <- all.vars(model.form[[2]])
    covs <- all.vars(model.form[[3]])
    model.form.vars <- as.character(attr(model.form.trms, "variables")[-1])
    model.form.type <- eval(substitute(Map(class, data[model.form.vars])))
    non.spec.type <- model.form.type[!(names(model.form.type) %in% c(specs.dep.vars, resp))]

    # split non-grouping model covariates into numeric and non-numeric lists
    covs.f <- Filter(function(t) t != 'numeric', non.spec.type)
    covs.n <- Filter(function(t) t == 'numeric', non.spec.type)

    # get numeric columns in lsmeans specs (for internal conversion to factor)
    specs.dep.n <- eval(substitute(names(Filter(is.numeric, data[specs.dep.vars]))))

    # overwrite model data with subset of only complete response variables
    model.call$data <- substitute(data[which(complete.cases(data[,resp])),] %>%
      mutate_at(.vars = specs.dep.n, .funs = as.factor))

    # give a bit of info about how data was subset
    orig.data.len <- nrow(eval(substitute(data)))
    new.data.len <- nrow(eval(model.call$data))
    if (!quietly & orig.data.len != new.data.len) message(paste0(
      orig.data.len - new.data.len,
      " records with incomplete data for the specified variables removed. (",
      round((orig.data.len - new.data.len) / orig.data.len, digits=2),
      "%)\n"))

    # refit model with new, complete cases data
    sas.model.obj <- do.call(as.character(model.call[[1]]), model.call[-1])

    # prep lsmeans levels for weighting lsmeans call (sink cat outputs to null)
    sink(tempfile())
    on.exit(suppressWarnings(sink()))
    lsmeans.levels <- lsmeans(sas.model.obj, specs, weights = "show.levels")
    sink()

    # prep sas-style arguments with which to call lsmeans
    .dots <- modifyList(
        list(sas.model.obj, specs = specs, mode = mode,
             # mean continuous variables calculated with proportional weights and filtered dataset
             at = eval(data) %>%
                    do( .[which(!is.na(.[resp])),] ) %>%
                    select(names(covs.n)) %>%
                    summarise_all(funs(mean(.))) %>%
                    as.list,
             # weights for categorical terms using marginal frequencies
             weights =
               if (is.null(names(lsmeans.levels))) 'proportional'
               else left_join(
                      lsmeans.levels,
                      broom::tidy(table(eval(data)[names(covs.f)])),
                      by = names(lsmeans.levels)) %>%
                    pull(Freq) ),
        list(...))

    if (!quietly & verbose) {
        message('Running lsmeans::lsmeans() with parameters: ')
        message(show(.dots))
    }

    lsmlist <- do.call(lsmeans::lsmeans, .dots)
    lsmlist$lsmeans@grid <- lsmlist$lsmeans@grid %>%
      mutate_(.dots = Map(function(.)
        paste(., 'lsmeans.formula', sep = '.', specs.dep.n))) %>%
      mutate_at(.vars = specs.dep.n, .funs = as.numeric)

    lsmlist
}

#' @method as.data.frame lsm.list
#' @export
as.data.frame.lsm.list <- function(lsm) { as.data.frame(lsm$lsmeans) }

#' @method as.data.frame lsmobj
#' @export
as.data.frame.lsmobj <- function(lsmo) {
  as.data.frame(summary(lsmo)) %>%
    left_join(lsmo@grid %>% select(c(names(lsmo@levels), '.wgt.')),
              by = names(lsmo@levels))
}

#' @method tidy lsm.list
#' @export
tidy.lsm.list <- function(lsm) { broom::tidy(lsm$lsmeans) }
