#' Plot Density
#' 
#' Generate density plot for a continuous covariate.
#' 
#' @author Alexey Pronin \email{pronin.alexey@gene.com}, Ning Leng \email{leng.ning@gene.com}, and previous team members (see DESCRIPTION)
#' 
#' @param data Input data frame. Rows are patients and columns are variables (e.g. demographics variables, time to event variables, 
#' biomarker variables, treatment indicator, etc.). One patient per row. 
#' @param biomarker.var Name of the variable (e.g. a biomarker variable). Should be in colnames of \code{data}.
#' @param log2 If TRUE, computes binary (i.e. base 2) logarithm. Default is FALSE.
#' @param col The color of the line segments. Default is blue.
#' @param text.font Legend text font size. Default is 3.
#' @param main The main title. Default is \code{paste("Distribution of", biomarker.var)}.
#' @param add.num The constant to add to all values. Helps to avoid applying log transformation on 0 or negative values. Default is 0.
#' @param xlab X axis label. Default is \code{paste(biomarker.var, ifelse(log2 == TRUE, "(log2 scale)", ""))}.
#' @param pdf.name Name of output pdf file. If it's NULL (default), the plots will be displayed but not saved as pdf.
#' @param pdf.param A list of parameters that define pdf graphics device. See \code{\link{pdf}}. Default is \code{list(width=7, height=7)}. 
#' @param par.param A list of parameters that define graphcial parameters. See \code{\link{par}}. Default is \code{list(mar=c(4,4,3,2))}.
#' 
#' @return Histogram and density will be shown. Summary statistics will be also shown on the plots.
#' 
#' @examples
#' data(input)
#' PlotDens(data=input, biomarker.var="OS")
#' PlotDens(data=input, biomarker.var="PFS", col="red", pdf.name="hist.pdf")
#' PlotDens(data=input, biomarker.var="PFS", log2=TRUE, pdf.name="hist_log2.pdf")
#' 
#' @export

PlotDens <- function(data,
                     biomarker.var,
                     log2=FALSE,
                     col="blue",
                     text.font=3,
                     main=paste("Distribution of", biomarker.var),
                     add.num=0,
                     xlab=paste(biomarker.var, ifelse(log2 == TRUE, "(log2 scale)", "")),
                     pdf.name=NULL,
                     pdf.param=list(width=7, height=7),
                     par.param=list(mar=c(4,4,3,2))) {

    if (!(is.data.frame(data))) {
        stop("An input is not a data frame!")
    }
    
    if (!(biomarker.var %in% colnames(data))) {
        stop("A covariate is not in the data frame!")
    }
    
    BM <- data[, biomarker.var] + add.num
    
    if (log2 == TRUE) {
        if (any(data[, biomarker.var] <= 0, na.rm=T)) {
            stop("Biomarker contains values less than or equal 0!
	              No log transformation possible! You may set add.num to add a constant to all values.")
        }
        BM <- log2(BM)
    }
    
    sd.bm <- round(sd(BM, na.rm=TRUE), 2)
    s.bm <- summary(BM)
    leg.text <- paste(c("Mean", "SD", "Median", "Range"),
                      c(round(s.bm, 2)["Mean"], 
                        sd.bm, 
                        round(s.bm, 2)["Median"],
                        paste("(", round(s.bm["Min."], 2), " - ", round(s.bm["Max."], 2), ")", sep="")),
                      sep=": ")
    
    PlotParam(pdf.name, pdf.param, par.param)  

    y2 <- max(density(BM, na.rm=T)$y, na.rm=TRUE)
    hist(BM, prob=T, main=main, xlab=xlab, col="grey", ylim=c(0, y2))
    lines(density(BM, na.rm=T), col=col, lwd=2)
    legend("topright", leg.text, bty="n", text.font=text.font)
    mtext(side=3, line=0, paste("N=", sum(!is.na(BM))))
    box()
    
    PlotParam()
}