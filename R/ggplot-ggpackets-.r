#' A simple S4 class to contain wrapped ggplots
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gnee.com}
#'
#' @note eventually this class could be used to handle pass
#' through of geom-specific parameters. For now it just collects
#' a list with cleaner syntax for adding ggproto objects.
#'
ggpacket <- setClass(
  "ggpacket",
  slots = c(ggcalls = "list"),
  prototype = list(ggcalls = list())
)

#' Initialize new object by adding ggproto to list with name label
setMethod("initialize", "ggpacket", function(.Object, ggproto_obj = NULL, label = NULL) {
  if (is.null(ggproto_obj)) return(.Object)
  .Object <- .Object + setNames(list(ggproto_obj), label)
  .Object
})

#' Overload show method to print ggpacket
setMethod("show", "ggpacket", function(object) {
  cat("ggpacket\nA container for multiple ggplot ggproto objects\n\n")
  if (length(object@ggcalls) == 0)
    cat("empty\n\n")
  else
    mapply(function(n, name, ggp) {
        cat(paste0("[[", n, "]] ", name, "\n"))
        print(ggp)
        cat("\n")
      },
      n = 1:length(object@ggcalls),
      ggp = object@ggcalls,
      name = names(object@ggcalls) %||% ""
    )
})

#' Primitive methods for adding ggpackets to various ggplot objects
setMethod("+", c("ggpacket", "ANY"), function(e1, e2) {
  if ("ggpacket" %in% class(e2)) e1@ggcalls <- append(e1@ggcalls, e2@ggcalls)
  else e1@ggcalls <- append(e1@ggcalls, e2)
  e1
})
setMethod("+", c("NULL", "ggpacket"), function(e1, e2) e2)
suppressMessages(setMethod("+", c("gg", "ggpacket"), function(e1, e2) e1 + e2@ggcalls))



#' A helper function to wrap ggplot calls, allowing for passing of a
#' shared list of arguments prefixed by a string. For example,
#' a call to a custom ggplot template may allow for arguments
#' 'line.color' and 'bar.color' to specify the line and bar colors
#' seperately. These arguments are parsed appropriately and
#' passed to the appropriate sub-function.
#'
#' @author Doug Kelkhoff \email{kelkhoff.douglas@gene.com}
#'
#' @note This function should eventually be split out into a separate
#' package for wrapping ggplot functions
#'
#' @param _geom the function to call
#' @param args_prefix the prefix string to subset arguments by
#' @param passed_args arguments to subset
#' @param ... additional arguments to use when calling the provided
#' function
#' @param null.empty return NULL if no arguments are received
#'
#' @return a call to the specified function with arguments subset for
#' only those which match the specified prefix
#'
ggpack <- function(`_call`, args_prefix = NULL, passed_args = NULL, ..., null.empty = FALSE) {
  passthru_args <- modifyList(ggpack_filter_args(args_prefix, passed_args), list(...))
  if (null.empty && length(passthru_args) == 0) return(NULL)

  # account for mismatched aesthetics when mapping being passed through
  # (code largely made to mirror ggplot2 layer.r's compute_aesthetics())
  if ('mapping' %in% names(passthru_args)) {
    callfname <- deparse(as.list(match.call())$"_call")
    if (grepl("geom_", callfname))             geom <- ggplot2:::find_subclass("Geom", gsub("^[^_]*_", "", callfname), parent.frame())
    else if ("geom" %in% names(passthru_args)) geom <- ggplot2:::find_subclass("Geom", passthru_args$geom, parent.frame())
    passthru_args$mapping <- ggpack_filter_aesthetics(geom, passthru_args$mapping)
  }

  if (all(class(`_call`) == "function")) ggpacket(do.call(`_call`, passthru_args), args_prefix)
  else ggpacket(`_call`, args_prefix)
}


#' Helper function for ggpack to filter arguments based on a prefix
ggpack_filter_args <- function(prefix, args) {
  if (is.null(prefix) || is.null(args)) return(args %||% list())
  unnamed_args <- args[[prefix]] %||% list()
  named_args <- args[grep(paste0('^', prefix, '.'), names(args))]
  named_args <- setNames(named_args, gsub(paste0('^', prefix, '.(.*)$'), '\\1', names(named_args)))
  as.list(c(named_args, unnamed_args))
}


#' Helper function to filter aesthetic mappings based on geometry
ggpack_filter_aesthetics <- function(geom, mapping) {
  allowed_aes <- c('x', 'y', 'group', geom$required_aes, names(geom$default_aes))
  mapping_aes_names <- names(ggplot2:::rename_aes(mapping))
  disallowed_aes <- setdiff(mapping_aes_names, allowed_aes)
  do.call(ggpack_remove_aesthetics, c(list(mapping), disallowed_aes))
}


#' Helper function to filter out aesthetics from mappings
#'
#' @param mapping aesthetic mapping to use for filtering
#' @param ... mapping labels to filter out
#'
#' @return an aesthetic mapping with filtered mappings removed
#' and group mapping set as interaction terms of all non axial
#' terms.
#'
#' @export
ggpack_remove_aesthetics <- function(mapping, ...) {
  .dots = list(...); if (length(.dots) == 0) return(mapping)
  mapped_vars <- mapping[!(names(mapping) %in% c('x', 'y'))]
  mapped_vals <- unique(unlist(mapped_vars, use.names=FALSE))
  mapping$group <- as.call(c(list(as.symbol("interaction")), mapped_vals))
  mapping[!(names(mapping) %in% .dots)]
}


#' Wrapper for common decorators to package
#'
#' @export
ggpack.decorators <- function(...) {
  ggpack(geom_blank, "blank", list(...)) +
  ggpack(xlab,  'xlab',  list(...), null.empty = TRUE) +
  ggpack(ylab,  'ylab',  list(...), null.empty = TRUE) +
  ggpack(labs,  'labs',  list(...), null.empty = TRUE) +
  ggpack(theme, 'theme', list(...), null.empty = TRUE)
}







