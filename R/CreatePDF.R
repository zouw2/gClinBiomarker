#' Create pdf file
#' 
#' @keywords internal
#' @importFrom grDevices pdf dev.cur dev.off 
#' @examples
#' \dontrun{
#' MainFunction <- function(..., pdfname = NULL, pdfparam = NULL) {
#'    # Do something here
#'    CreatePDF(pdfname, pdfparam)  
#'    # Do plotting here 
#'    CreatePDF(NULL) # Close the graphical device. 
#'    # Do something here
#' }
#'
#' MainFunction(..., pdfname = "myplot.pdf", pdfparam = list(width=10, height=5))
#' }
#' @export

CreatePDF <- function(pdfname, pdfparam) {
    if (dev.cur() > 1) {
        invisible(dev.off())
    } else {
        if (!is.null(pdfname)) {
            if (is.null(pdfparam)) {
                pdfparam <- list()
            }
            pdfparam$file <- pdfname
            do.call(pdf, pdfparam)
        }
    }
}
