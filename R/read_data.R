#' Read data
#'
#' Read two files and perform concordance check. First file contains data.
#' Second file provides specs for data.
#'
#' @author Alexey Pronin
#'
#' @param input A csv file with data.
#' @param input.specs A csv file with specs for \code{input}.
#' @param BEP Indicates Biomarker Evaluable Population.
#'
#' @return A list with two data frames and biomarker evaluable population: data, data.specs, and BEP.
#' @note The ReadData() function reads in the data file and spec file from csv files. The function also perform
#' concordance check between the data and its spec. Data column names and spec entry names should match.
#' No duplicate names are allowed.
#' @examples
#' \dontrun{
#' ReadData("input.csv", "input_specs.csv", "BEP")
#' }
#'
#' @export

ReadData <- function(input, input.specs=NULL, BEP=NULL) {
    
    # Check the input file exists.
    if (file.exists(input)) {
        input <- read.csv(input, stringsAsFactors=FALSE)
    } else {
        stop("The input file does not exist!")
    }
    
    # Check for duplicate columns in the input file.
    dup.input <- abs(length(colnames(input)) - length(unique(colnames(input))))
    if (dup.input != 0) {
        stop("There are duplicate name columns in the input file!")
    }
    
    # If BEP is not NULL
    if (!is.null(BEP)) {
        # Check for the variable BEP in the input file.
        if (!(BEP %in% colnames(input))) {
            stop("There is no variable BEP (Biomarker Evaluable Population). It's required!")
        }
        # Check BEP takes 0 or 1 only.
        if (!(all(input$BEP %in% c(0, 1)))) {
            stop("The variavle BEP must be 0 or 1!")
        }
    }

    # If input.specs is not NULL.
    if (!is.null(input.specs)) {
        # Check the spec file exists.
        if (file.exists(input.specs)) {
        input.specs <- read.csv(input.specs, stringsAsFactors=FALSE)
        } else {
            stop("The specs file does not exist!")
        }
        # Check data types in specs are allowed.
        types.allowed <- c("character", "numeric", "categorical", "time", "event")
        if (!(all(input.specs$Type %in% types.allowed))) {
            stop(paste("Allowed data types are:",
                       "character, numeric, categorical, time, and event.",
                       "Please check the types in the specs file!"))
        }
        # Check for duplicate columns in the spec file.
        dup.input.specs <- abs(length(input.specs$Variable) - length(unique(input.specs$Variable)))
        if (dup.input.specs != 0) {
            stop("There are duplicate rows in the specs file!")
        }
        # Check colnames(input) match input.specs$Variable.
        col.dif <- length(setdiff(colnames(input), input.specs$Variable))
        if (col.dif != 0) {
            stop("The column names between input and input.specs files do not match!")
        }
        # Check class of variables.
        for (i in 1:nrow(input.specs)) {
            # Convert categorical to factor.
            if (input.specs$Type[i] == "categorical") {
                input[, i] <- as.factor(input[, input.specs$Variable[i]])
            }
            # Check the class of numeric variables is numeric.
            if (input.specs$Type[i] %in% c("numeric", "time", "event")) {
                if (is.numeric(input[, input.specs$Variable[i]]) == FALSE) {
                    stop("The variable ", input.specs$Variable[i],
                         " should be numeric but identified as ",
                         class(input.specs$Variable[i]), "!")
                }
            }
        }
    }

    return(list(data=input, data.specs=input.specs, BEP=BEP))
}
