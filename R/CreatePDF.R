#' @keywords internal
#' @importFrom grDevices pdf dev.cur dev.off 

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
