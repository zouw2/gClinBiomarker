#' Set graphical parameters
#' 
#' \code{PlotParam} can be used to set graphical parameters for a pdf file 
#' or the default screen device.
#' 
#' @author Alexey Pronin, Ning Leng
#' 
#' @importFrom grDevices pdf dev.cur dev.off 
#' @importFrom graphics par 
#' 
#' @param pdf.name A name of a pdf file.
#' @param pdf.param A list of parameters that define pdf graphics device. See \code{\link{pdf}}.
#' @param par.param A list of parameters that define graphcial parameters. See \code{\link{par}}.
#' 
#' @note The \code{PlotParam} function is designed to restore graphical parameters \code{par} 
#' to the default values after execution.     
#' 
#' @examples
#' \dontrun{
#' plotKMsub <- function(..., 
#'                       pdf.name = NULL, 
#'                       pdf.param = NULL, 
#'                       par.param = NULL) {
#'    # Do something here.
#'    PlotParam(pdf.name, pdf.param, par.param)  
#'    # Do plotting here. 
#'    PlotParam() # Close the pdf graphical device if it was open and 
#'                # set graphical parameters to the default values. 
#'    # Do something here.
#' }
#' 
#' # Define both pdf graphics device and graphcial parameters.
#' plotKMsub(..., pdf.name = "myplot.pdf", 
#'                pdf.param = list(width=10, height=5), 
#'                par.param = list(mfrow=c(2, 2)))
#' 
#' # Define graphcial parameters. A pdf file is not created.
#' # The graph is pdoduced in the default screen device.
#' plotKMsub(..., par.param = list(mar=c(7,7,7,7)))   
#'             
#' }
#' @export

PlotParam <- function(pdf.name, pdf.param, par.param) {
    vec <- c(missing(pdf.name), missing(pdf.param), missing(par.param))
    if (all(vec)) {
        # Load old par on exit and shut down the graphical device.
        if (names(dev.cur()) != "RStudioGD") {
            par(old.par)
            invisible(dev.off())
        } else {
            # Load old par on exit. Does not shutdown the screen device. 
            par(old.par)
        }
    } else { 
        # Create/Modify pdf.param.
        if (!is.null(pdf.name)) {
            if (is.null(pdf.param)) {
                pdf.param <- list()
            }
            pdf.param$file <- pdf.name
            do.call(pdf, pdf.param)
        }
        # Save par to global variable.
        old.par <- NULL
        old.par <<- par(no.readonly=TRUE) 

        # Create/Modify par.param.
        if (is.null(par.param)) {
            par.param <- list()
        }
        do.call(par, par.param)
    }
}