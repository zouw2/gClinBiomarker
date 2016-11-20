#' Read a VAD
#'
#' Read a VAD and create specs for it. 
#'
#' @author Alexey Pronin
#'
#' @param file A SAS dataset.
#' @param data.write.to A csv file where the data will be written to. Default is NULL.
#' @param data.specs.write.to A csv file where the data specs will be written to. Default is NULL.
#'
#' @return A list with two data frames: data and data.specs.
#' @note The ReadVAD() function reads in a VAD and creates data specs.
#' @examples
#' \dontrun{
#' ReadVAD("ars.sas7bdat", data.write.to = "data.csv", data.specs.write.to = "data.specs.csv")
#' ReadVAD("ars.sas7bdat")
#' }
#'
#' @export

ReadVAD <- function(file, data.write.to = NULL, data.specs.write.to = NULL) {
    
    # Check the input file exists.
    if (file.exists(file)) {
        data <- as.data.frame(haven::read_sas(file))
    } else {
        stop("The VAD does not exist!")
    }
    
    # Parse SAS file and create specs.
    Label <- sapply(data, attr, "label")
    Type <- sapply(data, mode)    
    Variable <- names(Type)
    data.specs <- data.frame(Variable, Type, Label, row.names= NULL)
    
    if (!is.null(data.write.to)) {
        utils::write.csv(data, data.write.to, row.names = FALSE)
    }
    
    if (!is.null(data.specs.write.to)) {
        utils::write.csv(data.specs, data.specs.write.to, row.names = FALSE)
    }
    
    return(list(data = data, data.specs = data.specs))
}