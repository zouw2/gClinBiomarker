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
                        as.data.frame = FALSE,
                        quietly = FALSE, verbose = FALSE, ...) {

    if (!quietly) message(paste(
        "To mirror SAS functionality:",
        " - mode of 'kenward-roger' will be used unless otherwise specified.",
        " - model.obj will be fit with only complete response variables.",
        " - weights set by count per level based on full dataset.\n",
        sep = "\n"))

    # get model.obj formula response variable
    model.call  <- as.list(model.obj$call)

    # try to get model formula from model object, defaulting to first arg
    # if a 'model' or 'formula' argument are not found
    if ('model' %in% names(model.call))
        model.form.trms <- terms(as.formula(model.call$model))
    else if ('formula' %in% names(model.call))
        model.form.trms <- terms(as.formula(model.call$formula))
    else
        model.form.trms <- terms(as.formula(model.call[[2]]))

    model.form.vars <- as.character(attr(model.form.trms, "variables")[-1])
    model.form.resp <- attr(model.form.trms, "response")
    resp <- as.character(model.form.vars[model.form.resp])
    data <- model.call$data

    # overwrite model data with subset of only complete response variables
    model.call$data <- substitute(data[which(complete.cases(data[,resp])),])

    # refit model with new, complete cases data
    sas.model.obj <- do.call(as.character(model.call[[1]]), model.call[-1])

    # prep lsmeans levels for weighting lsmeans call (sink cat outputs to null)
    sink(tempfile())
    on.exit(suppressWarnings(sink()))
    lsmeans.levels <- names(lsmeans(
        sas.model.obj,
        specs,
        weights = "show.levels"))
    sink()

    # edit specs to preface with 'pairwise' if not provided to lsmeans
    if (inherits(specs, "formula") & is.null(terms(specs)$response)) {
        specs.chr <- as.character(attr(terms(specs), 'variables')[-1])
        specs <- as.formula(paste('pairwise', specs.chr, sep=" ~ "))
    }

    # prep sas-style arguments with which to call lsmeans
    .dots <- modifyList(
        list(sas.model.obj,
             specs = specs,
             mode = mode,
             weights = if (is.null(lsmeans.levels)) 'proportional'
                       else table(eval(data)[lsmeans.levels])),
        list(...))

    if (!quietly & verbose) {
        message('Running lsmeans::lsmeans with parameters: ')
        message(show(.dots))
    }

    # fit and output
    lsmeans.fit <- do.call(lsmeans, .dots)
    if (!as.data.frame) lsmeans.fit
    else broom::tidy(lsmeans.fit$lsmeans)
}

#' @export
tidy.lsm.list <- function(lsm) { broom::tidy(lsm$lsmeans) }
