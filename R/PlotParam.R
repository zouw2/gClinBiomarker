#' Set graphical parameters
#' 
#' \code{PlotParam} can be used to set graphical parameters for a pdf file 
#' or the default screen device.
#' 
#' @author Alexey Pronin \email{pronin.alexey@gene.com}, Ning Leng \email{leng.ning@gene.com}
#' 
#' @param pdf.name A name of a pdf file.
#' @param pdf.param A list of parameters that define pdf graphics device. See \code{\link{pdf}}.
#' @param par.param A list of parameters that define graphcial parameters. See \code{\link{par}}.
#' 
#' @note The \code{PlotParam} function is designed to restore graphical parameters \code{par} 
#' to the default values after execution.  
#' 
#' @importFrom grDevices pdf dev.cur dev.off 
#' @importFrom graphics par 
#' @importFrom knitr all_labels      
#' 
#' @examples
#' \dontrun{
#' CompareKM <- function(data, 
#'                       tte, 
#'                       cens, 
#'                       trt=NULL, 
#'                       bep, 
#'                       bep.indicator=1,
#'                       bep.name="Biomarker Evaluable", 
#'                       itt.name="ITT",
#'                       col.itt="palegreen4", 
#'                       col.bep="lightpink2", 
#'                       col.ci="lightcyan",
#'                       shaded.ci=TRUE,
#'                       xlim=NULL, 
#'                       xat=NULL, 
#'                       ylab=paste(tte, "Survival Probability"), 
#'                       xlab="Time", 
#'                       main="",
#'                       pdf.name = NULL, 
#'                       pdf.param=list(height=5), 
#'                       par.param=list(mar=c(4, 4, 3, 2)), ...) {
#'    
#'    if (is.null(xlim)) { 
#'        xlim <- c(0, max(data[, tte], na.rm=TRUE)) + c(0, 0.1*max(data[, tte], na.rm=TRUE)) 
#'    }
#'    
#'    lev <- NULL
#'    
#'    if (!is.null(trt)) {
#'        lev <- levels(factor(data[, trt]))
#'        nlev <- length(lev)
#'    }
#'    if (is.null(par.param$mfrow)) { 
#'        par.param$mfrow <- c(1, nlev) # 1 by nlev subplots
#'    }
#'    if (is.null(pdf.param$height)) {
#'        pdf.param$height <- 5
#'    }
#'    if (is.null(pdf.param$width)) {
#'        pdf.param$width <- nlev * pdf.param$height
#'    }
#'    
#'    # Set graphical parameters
#'    PlotParam(pdf.name, pdf.param, par.param)
#'    
#'    for (i in 1:nlev) {
#'        if (!is.null(trt)) {
#'            tmp <- data[which(data[, trt] == lev[i]), ]
#'        }
#'        if (is.null(trt)) {
#'            tmp <- data
#'        }
#'        
#'        sf <- survfit(as.formula(paste("Surv(", tte, ",", cens, ")~1")), 
#'                      data=tmp, 
#'                      conf.type="log")
#'        plot(sf, col=col.itt, ylab=ylab, xlab=xlab, xlim=xlim, axes=FALSE, mark.time=FALSE, ...)
#'        axis(2)
#'        
#'        if (!is.null(xat)) {
#'            axis(1, at=xat, labels=xat)
#'        }
#'        if (is.null(xat)) {
#'            axis(1)
#'        }
#'        mtext(side=3, paste(main, "\n", lev[i]), col="black", line=1.2, cex=1.5, font=2)
#'        grid()
#'        box()
#'        
#'        if (shaded.ci == TRUE) {
#'            idx <- which(!is.na(sf[["upper"]]) & !is.na(sf[["lower"]]))
#'            
#'            x <- c(0, sf[["time"]][idx]) 
#'            xp <- x
#'            up <- sf[["upper"]][idx]
#'            lo <- sf[["lower"]][idx]
#'            idx2 <- which(is.na(sf[["upper"]]) & is.na(sf[["lower"]]))
#'            
#'            if (length(idx2) > 0) {
#'                idx2 <- idx2[1]
#'                xp[length(xp)+1] <- sf[["time"]][idx2]
#'                up[length(up)+1] <- up[length(up)]
#'                lo[length(lo)+1] <- lo[length(lo)]
#'            }
#'            
#'            yp <- c(1, up, rev(lo), 1)
#'            xp <- c(xp[1], rep(xp[2:length(xp)], each=2), xp[length(xp)])
#'            xp <- c(xp, rev(xp))
#'            yp <- rep(yp, each=2)
#'            polygon(xp, yp, density=NULL, col=col.ci, lwd=0.5, border=NA)
#'        }
#'        
#'        lines(sf, mark.time=FALSE, col=col.itt, lwd=3)
#'        
#'        sfflag <- survfit(as.formula(paste("Surv(", tte, ",", cens, ")~1")), 
#'                          data=tmp[which(tmp[, bep]==bep.indicator), ], 
#'                          conf.type="log")
#'        lines(sfflag, mark.time=FALSE, col=col.bep, lwd=3)
#'        
#'        legend("topright", lty=1, lwd=3, col=c(col.itt, col.bep), legend=c(itt.name, bep.name))
#'        
#'    }
#'    
#'    # Close the pdf graphical device if it was open
#'    # and set graphical parameters to the default values.
#'    PlotParam()
#'    
#'} # end of CompareKM()
#' 
#' 
#' data(input)
#' library(survival)
#' 
#' CompareKM(data=input, tte="PFS", cen="PFS.CNSR", trt="Arm", bep="BEP", bep.name="BEP")
#' CompareKM(data=input, tte="PFS", cen="PFS.CNSR", trt="Arm", bep="BEP", bep.name="BEP", 
#'           pdf.name="my_graph.pdf")
#' CompareKM(data=input, tte="PFS", cen="PFS.CNSR", trt="Arm", bep="BEP", bep.name="BEP",
#'           pdf.name="my_graph2.pdf", par.param=list(mar=c(7,7,7,7)))
#' }
#' 
#' @export

PlotParam <- function(pdf.nam, pdf.param, par.para) {
    vec <- c(missing(pdf.name), missing(pdf.param), missing(par.param))
    if (all(vec)) {
        # Load old par on exit and shut down the graphical device.
        if (names(dev.cur()) != "RStudioGD" & length(all_labels()) == 0) {
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
        invisible(do.call(par, par.param))
    }
}