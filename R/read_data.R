#' Read data
#'
#' Read two files. First file contains data.
#' Second file provides specs for data.
#'
#' @author Alexey Pronin
#'
#' @param input A file with data.
#' @param input.specs A file with specs for \code{input}.
#'
#' @return Two data frames: df and df.specs
#'
#' @examples
#' \dontrun{
#' ReadData("input.csv", "input_specs.csv")
#' }
#'
#' @export

ReadData <- function(input, input.specs) {

    # Check the specs file exists.
    if (file.exists(input.specs)) {
        df.specs <- read.csv(input.specs, colClasses="character")
    } else {
        stop("The specs file does not exist!")
    }

    # Check data types in specs are allowed.
    types.allowed <- c("character", "numeric", "categorical", "time", "event")
    if (!(all(df.specs$Type %in% types.allowed))) {
        stop(paste("Allowed data types are:",
                   "character, numeric, categorical, time, and event.",
                   "Please check the types in the specs file!"))
    }

    # Check the input file exists.
    if (file.exists(input)) {
        df <- read.csv(input, colClasses="character")
    } else {
        stop("The input file does not exist!")
    }

    # Check for the variable BEP in the input file.
    if (!("BEP" %in% colnames(df))) {
        stop("There is no variable BEP (Biomarker Evaluable Population). It's required!")
    }

    # Check colnames(input) match input.specs$Variable.
    col.dif <- length(setdiff(colnames(df), df.specs$Variable))
    if (col.dif != 0) {
        stop("The column names between input and input.specs files do not match.")
    }

    # Check for dubplicate columns in the input file.
    dub.input <- abs(length(colnames(df)) - length(unique(colnames(df))))
    if (dub.input != 0) {
        stop("There are dublicate name columns in the input file!")
    }

    # Check for dubplicate columns in the specs file.
    dub.input.specs <- abs(length(df.specs$Variable) - length(unique(df.specs$Variable)))
    if (dub.input.specs != 0) {
        stop("There are dublicate rows in the specs file!")
    }

    # Convert input data types defined by the specs file.
    for(i in 1:nrow(df.specs)) {
        if (df.specs$Type[i] %in% c("categorical", "event")) {
            df[, i] <- as.factor(df[, df.specs$Variable[i]])
        } else if (df.specs$Type[i] %in% c("numeric", "time")) {
            df[, i] <- as.numeric(df[, df.specs$Variable[i]])
        }
    }

    return(list(df, df.specs))
}

#df = ReadData("input.csv", "input.specs.csv")
