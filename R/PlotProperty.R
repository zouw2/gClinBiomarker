#' Plot Distribution
#'
#' Plot biomarker or clinical variables properties.
#'
#' @author Alexey Pronin \email{pronin.alexey@gene.com}, Ning Leng \email{leng.ning@gene.com}, and previous team members (see DESCRIPTION)
#'
#' @param data input data frame. Rows are patients and columns are variables (e.g. demographics variables, time to event variables,
#' biomarker variables, treatment indicator, etc.). One patient per row.
#' @param biomarker.var name of the biomarker variable. Should be in colnames of \code{data}.
#' @param biomarker.class can be either "numeric" or "categorical".
#' @param var name of a clinical variable. It can be a list of variables. Should be in colnames of \code{data}.
#' @param var.class can be either "numeric" or "categorical". It can be a list of classes. Default is NULL.
#' @param log2 if TRUE, computes binary (i.e. base 2) logarithm. It can be a list if there are several numeric variables. The \code{log2} transofrmation can be applied to numeric variables only. Default is FALSE.
#' @param colour the color of the line segments or dots. Default is "blue".
#' @param add.num the constant to add to all values. Helps to avoid applying log transformation on 0 or negative values. Will be ignored if covariate is categorical. Default is 0.
#' @param text.font legend text font size. Default is 3.
#' @param main the main title. Default is \code{"Distribution of"}.
#' @param xlab x axis label. Default is "".
#' @param add.lab an additional text to y axis label. Default is "".
#' @param border a vector of colors for the outlines of the boxplots. Default is NULL.
#' @param add.cor add the correlation coefficient to the boxplot. Default is FALSE.
#' @param cor.method which correlation coefficient to compute. One of "pearson", "kendall", or "spearman" (default) can be abbreviated.
#' @param lowess.line performs the computations for the \code{LOWESS} smoother which uses locally-weighted polynomial regression. See \code{\link{lowess}}.
#' @param lowess.line.col the smoother color. Default is "deepskyblue".
#' @param f the smoother span. This gives the proportion of points in the plot which influence the smooth at each value. Larger values give more smoothness. Default is 0.3.
#' @param pdf.name name of output pdf file. If it's NULL, the plots will be displayed but not saved as pdf. Default is paste("BMDist_", as.character(Sys.Date()), ".pdf", sep="")
#' @param pdf.param a list of parameters that define pdf graphics device. See \code{\link{pdf}}. Default is \code{list(width=6, height=4.5)}.
#' @param par.param a list of parameters that define graphcial parameters. See \code{\link{par}}. Default is \code{list(mar=c(4,4,3,2))}.
#'
#' @return If only a biomarker variable is given, it will crete a density plot for a numeric variable or bar plot for a categorical variable.
#' If only a list of clinical variables is provided, it will create a density plot for each numeric variable and a bar plot for each categorical variable.
#' If both a biomarker variable and a list of clinical variables are given, it will create: a scatter plot for a numeric pair of variables;
#' a bar plot for each categorical variable; a boxplot for a pair of numeric and categorical variables.
#'
#' @examples
#' data(input)
#' PlotProperty(data=input, biomarker.var="KRAS.exprs", biomarker.class="numeric", log2=TRUE, pdf.name=NULL)
#' PlotProperty(data=input, biomarker.var="KRAS.exprs", biomarker.class="numeric", var="OS", var.class="numeric", log2=list(TRUE, FALSE), pdf.name=NULL)
#' PlotProperty(data=input, biomarker.var="KRAS.mutant", biomarker.class="categorical", var=list("Arm","OS"), var.class=list("categorical", "numeric"), pdf.name=NULL, par.param = list(mfrow=c(1, 3)))
#'
#' @export

PlotProperty <- function(data,
                         biomarker.var,
                         biomarker.class,
                         var=NULL,
                         var.class=NULL,
                         log2=FALSE,
                         colour="blue",
                         add.num=0,
                         text.font=3,
                         main="Distribution of",
                         xlab="",
                         add.lab="",
                         border=NULL,
                         add.cor=FALSE,
                         cor.method="spearman",
                         lowess.line=FALSE,
                         lowess.line.col="deepskyblue",
                         f=0.3,
                         pdf.name=paste("BMDist_", as.character(Sys.Date()), ".pdf", sep=""),
                         pdf.param=list(width=6, height=4.5),
                         par.param=list(mar=c(4,4,3,2))) {

    # Check the data
    if (!(is.data.frame(data))) {
        stop("An input is not a data frame!")
    }

    if ((is.null(biomarker.var) & is.null(var)))  {
        stop("At least one biomarker variable or clinical variable should be provided!")
    }

    if (length(biomarker.var) > 1 ) {
        stop("Only one biomarker variable should be given!")
    }

    if (length(biomarker.class) > 1 ) {
        stop("Only one class of biomarker variable should be given!")
    }

    sel <- which(!biomarker.var %in% colnames(data))
    if (length(sel) > 0) {
        stop(paste("A covariate", biomarker.var[sel], "is not in the data frame! "))
    }

    sel <- which(!var %in% colnames(data))
    if (length(sel) > 0) {
        stop(paste("A covariate", var[sel], "is not in the data frame! "))
    }

    if (length(biomarker.class) != 0) {
        if (!all(biomarker.class %in% c("numeric", "categorical"))) {
            stop("The class of the biomarker variables can be only 'numeric' or 'categorical'. Please check spelling!")
        }
    }

    if (length(var.class) != 0) {
        if (!all(var.class %in% c("numeric", "categorical"))) {
            stop("The class of the clinical variables can be only 'numeric' or 'categorical'. Please check spelling!")
        }
    }

    PlotParam(pdf.name, pdf.param, par.param)

    # Case 1: Biomarker variable property or Clinical variables property
    if ((!is.null(biomarker.var) & is.null(var)) | (is.null(biomarker.var) & !is.null(var))) {
        if (!is.null(biomarker.var) & is.null(var)) {
            vars <- biomarker.var
            classes <- biomarker.class
        } else if (is.null(biomarker.var) & !is.null(var)) {
            vars <- var
            classes <- var.class
        }

        # Create a plot for each variable
        for (i in 1:length(vars)) {
            # if continues, then density plot
            if (classes[[i]] == "numeric") {
                V <- data[, vars[[i]]] + add.num

                if (log2[[i]] == TRUE) {
                    if (any(data[, vars[[i]]] <= 0, na.rm=T)) {
                        stop(paste(vars[[i]], " contains values less than or equal 0!
                       No log transformation possible! You may set add.num to add a constant to all values."))
                    }
                    V <- log2(V)
                }

                sd.v <- round(sd(V, na.rm=TRUE), 2)
                s.v <- summary(V)
                leg.text <- paste(c("Mean", "SD", "Median", "Range"),
                                  c(round(s.v, 2)["Mean"],
                                    sd.v,
                                    round(s.v, 2)["Median"],
                                    paste("(", round(s.v["Min."], 2), " - ", round(s.v["Max."], 2), ")", sep="")),
                                  sep=": ")

                y2 <- max(density(V, na.rm=T)$y, na.rm=TRUE)

                old.xlab <- xlab

                if (xlab == "") {
                    xlab <- paste(vars[[i]], ifelse(log2[[i]] == TRUE, "(log2 scale)", ""))
                }

                hist(V, prob=T, main=paste(main, vars[[i]], sep=" "), xlab=xlab, col="grey", ylim=c(0, y2))

                xlab <- old.xlab

                lines(density(V, na.rm=T), lwd=2, col=colour)
                legend("topright", leg.text, bty="n", text.font=text.font)
                mtext(side=3, line=0, paste("N=", sum(!is.na(V))))
                box()
            # if categorical, then barplot
            } else {
                    tab <- table(data[, vars[[i]]])
                    freqs <- paste("(", round(100*tab/sum(tab), 2), "%)", sep="")
                    barplot(tab, names.arg=paste(names(tab), freqs), main=paste(main, vars[[i]], sep=" "))
            }
        }


    # Case 2: Biomarker variable vs Clinical variables
    } else {
        # if biomarker variable is numeric
        if (biomarker.class == "numeric") {

            BV <- data[, biomarker.var] + add.num
            j <- 1

            if (log2[[j]] == TRUE) {
                if (any(data[, biomarker.var] <= 0, na.rm=T)) {
                    stop(paste(biomarker.var, " contains values less than or equal 0!
                       No log transformation possible! You may set add.num to add a constant to all values."))
                }
                BV <- log2(BV)
            }

            for (i in 1:length(var)) {
                # if clinical variable is numeric, then scatteplot
                if (var.class[[i]] == "numeric") {
                    j <- j+1
                    V <- data[, var[[i]]] + add.num

                    if (log2[[j]] == TRUE) {
                        if (any(data[, var[[i]]] <= 0, na.rm=T)) {
                            stop(paste(var[[i]], " contains values less than or equal 0!
                                       No log transformation possible! You may set add.num to add a constant to all values."))
                        }
                        V <- log2(V)
                    }

                    x <- V
                    y <- BV
                    plot(x, y, ylab=paste(biomarker.var, add.lab, ifelse(log2[[1]] == TRUE, "(log2 scale)", "")),
                         xlab=paste(var[[i]], ifelse(log2[[j]] == TRUE, "(log2 scale)", "")),
                         main=paste(biomarker.var, "by", var[[i]]), col=colour)
                    grid(nx=NULL, ny=NULL)

                    if(lowess.line) {
                        lines(lowess(x, y, f=f), lwd=2, col=lowess.line.col)
                    }

                # if clinical variable is categorical, then boxplot
                } else if (var.class[[i]] == "categorical") {
                    x <- factor(data[, var[[i]]])
                    xx <- jitter(as.numeric(x))
                    yy <- BV
                    nlev <- nlevels(x)
                    ylim <- range(yy, na.rm=T)

                    if (is.null(colour)){
                        colour <- colorRampPalette(c("deepskyblue", "tomato"))(nlev)
                        colour <- colour[as.numeric(x)]
                    }

                    if (na.exclude(colour)[1] == FALSE) {
                        colour <- NULL
                    }

                    bx <- boxplot(as.formula(paste("yy ~ factor(", var[[i]], ")")), data=data,
                                  main=paste(biomarker.var, "by", var[[i]]),
                                  border=border, ylim=ylim, outline=F, axes=F,
                                  ylab=paste(biomarker.var, add.lab, ifelse(log2[[1]] == TRUE, "(log2 scale)", "")))
                    points(xx, yy, col=colour)

                    if (add.cor) {
                        mycor <- cor(xx, yy, method=cor.method, use="pairwise.complete")
                        legend("bottomright", paste("spear cor =", round(mycor, 2), sep=""), text.font=3)
                    }

                    axis(2)
                    box()

                    if (par("srt") != 0) {
                        sp <- ylim[2]-ylim[1]
                        axis(1, labels=F, at=1:length(bx$n), lwd=0)
                        text(1:length(bx$n), par("usr")[3] - 0.03*sp, labels=bx$names)
                    }

                    if (par("srt") == 0) {
                        axis(1, labels=bx$names, at=1:length(bx$names), lwd=0, font=2)
                    }

                    axis(3, line=-1, lwd=0, at=1:length(bx$n), paste("N=", bx$n, sep=""), cex=0.8)
                    grid(nx=NULL, ny=NULL)
                }
            }

        # if biomarker variable is categorical
        } else {
            for (i in 1:length(var)) {
                j <- 1
                # if clinical variable is numeric, then boxplot
                if (var.class[[i]] == "numeric") {

                    V <- data[, var[[i]]] + add.num

                    if (log2[[j]] == TRUE) {
                        if (any(data[, var[[i]]] <= 0, na.rm=T)) {
                            stop(paste(var[[i]], " contains values less than or equal 0!
                       No log transformation possible! You may set add.num to add a constant to all values."))
                        }
                        V <- log2(V)
                    }

                    x <- factor(data[, biomarker.var])
                    xx <- jitter(as.numeric(x))
                    yy <- V
                    nlev <- nlevels(x)
                    ylim <- range(yy, na.rm=T)

                    if (is.null(colour)){
                        colour <- colorRampPalette(c("deepskyblue", "tomato"))(nlev)
                        colour <- col[as.numeric(x)]
                    }

                    if (na.exclude(colour)[1] == FALSE) {
                        colour <- NULL
                    }

                    bx <- boxplot(as.formula(paste("yy ~ factor(", biomarker.var, ")")), data=data,
                                  main=paste(var[[i]], "by", biomarker.var),
                                  border=border, ylim=ylim, outline=F, axes=F,
                                  ylab=paste(var[[i]], add.lab, ifelse(log2[[j]] == TRUE, "(log2 scale)", "")))
                    points(xx, yy, col=colour)

                    if (add.cor) {
                        mycor <- cor(xx, yy, method=cor.method, use="pairwise.complete")
                        legend("bottomright", paste("spear cor =", round(mycor, 2), sep=""), text.font=3)
                    }

                    axis(2)
                    box()

                    if (par("srt") != 0) {
                        sp <- ylim[2]-ylim[1]
                        axis(1, labels=F, at=1:length(bx$n), lwd=0)
                        text(1:length(bx$n), par("usr")[3] - 0.03*sp, labels=bx$names)
                    }

                    if (par("srt") == 0) {
                        axis(1, labels=bx$names, at=1:length(bx$names), lwd=0, font=2)
                    }

                    axis(3, line=-1, lwd=0, at=1:length(bx$n), paste("N=", bx$n, sep=""), cex=0.8)
                    grid(nx=NULL, ny=NULL)
                    j <- j+1

                # if clinical variable is categorical, then barplot
                } else if (var.class[[i]] == "categorical") {
                    tab <- table(data[, biomarker.var])
                    freqs <- paste("(", round(100*tab/sum(tab), 2), "%)", sep="")
                    barplot(tab, names.arg=paste(names(tab), freqs), main=paste(main, biomarker.var, sep=" "))

                    tab <- table(data[, var[[i]]])
                    freqs <- paste("(", round(100*tab/sum(tab), 2), "%)", sep="")
                    barplot(tab, names.arg=paste(names(tab), freqs), main=paste(main, var[[i]], sep=" "))
                }
            }
        }
    }

    PlotParam()
}
