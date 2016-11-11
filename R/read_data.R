#' Read data
#'
#' Read two files and perform concordance check. First file contains data.
#' Second file provides specs for data.
#'
#' @author Alexey Pronin
#'
#' @param input A csv file with data.
#' @param input.specs A csv file with specs for \code{input}.
#'
#' @return Two data frames in a list: df and df.specs
#' @note The ReadData() function reads in the data file and spec file from csv files. The function also perform
#' concordance check between the data and its spec. Data column names and spec entry names should match.
#' No duplicate names are allowed.
#' @examples
#' \dontrun{
#' ReadData("input.csv", "input_specs.csv")
#' }
#'
#' @export

ReadData <- function(input, input.specs) {

    # Check the specs file exists.
    if (file.exists(input.specs)) {
        df.specs <- read.csv(input.specs, stringsAsFactors=FALSE)
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
        df <- read.csv(input, stringsAsFactors=FALSE)
    } else {
        stop("The input file does not exist!")
    }

    # Check for the variable BEP in the input file.
    if (!("BEP" %in% colnames(df))) {
        stop("There is no variable BEP (Biomarker Evaluable Population). It's required!")
    }

    # Check BEP takes 0 or 1 only.
    if (!(all(df$BEP %in% c(0, 1)))) {
        stop("The variavle BEP must be 0 or 1!")
    }

    # Check colnames(input) match input.specs$Variable.
    col.dif <- length(setdiff(colnames(df), df.specs$Variable))
    if (col.dif != 0) {
        stop("The column names between input and input.specs files do not match!")
    }

    # Check for duplicate columns in the input file.
    dup.input <- abs(length(colnames(df)) - length(unique(colnames(df))))
    if (dup.input != 0) {
        stop("There are duplicate name columns in the input file!")
    }

    # Check for duplicate columns in the specs file.
    dup.input.specs <- abs(length(df.specs$Variable) - length(unique(df.specs$Variable)))
    if (dup.input.specs != 0) {
        stop("There are duplicate rows in the specs file!")
    }

    for (i in 1:nrow(df.specs)) {
        # Convert categorical to factor.
        if (df.specs$Type[i] == "categorical") {
            df[, i] <- as.factor(df[, df.specs$Variable[i]])
        }

        # Check the class of numeric variables is numeric.
        if (df.specs$Type[i] %in% c("numeric", "time", "event")) {
            if (is.numeric(df[, df.specs$Variable[i]]) == FALSE) {
                stop("The variable ", df.specs$Variable[i],
                     " should be numeric but identified as ",
                     class(df.specs$Variable[i]), "!")
            }
        }
    }

    return(list(df, df.specs))
    #test
}

#df = ReadData("input.csv", "input.specs.csv")

