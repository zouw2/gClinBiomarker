#' Description 1
#' 
#' Description 2
#' 
#' @author Alexey Pronin \email{pronin.alexey@gene.com}, Ning Leng \email{leng.ning@gene.com}, and previous team members (see DESCRIPTION)
#'
#' @param outcome.var vector specifying the outcome variable.
#' @param reference.var ???.
#' @param ref.popname ???. Default is "All".
#' @param treatment.var name of the treatment variable. Default is NULL.
#' @param biomarker.var name of the biomarker variable. Default is NULL.
#' @param biomarker.varname ???. Default is NULL.
#' @param biomarker.code ???. Default is NULL.
#' @param main main title of the barplot. Default is "Association of biomarker expression with response rate".
#' @param sub footnote under the bar plot. Default is NULL.
#' @param return.table ???. Default is TRUE.
#' @param dichotomized ???. Default is TRUE.
#' @param pdf.name name of output pdf file. If it's NULL, the plots will be displayed but not saved as pdf. Default is "rsp.barplot.pdf".
#' @param pdf.param list of parameters that define pdf graphics device. See \code{\link{pdf}}. Default is \code{list(width=6, height=4.5)}. 
#' @param par.param list of parameters that define graphcial parameters. See \code{\link{par}}. Default is \code{list(mar=c(4,4,3,2))}.
#' 
#' @return ???
#' 
#' @examples
#' data(input)
#' 
#' @export

RspBarPlot <- function(outcome.var, 
                       reference.var, 
                       ref.popname="All", 
                       treatment.var=NULL, 
                       biomarker.var=NULL,
                       biomarker.varname=NULL,
                       biomarker.code=NULL,
                       main="Association of biomarker expression with response rate",
                       sub=NULL,
                       return.table=TRUE,
                       dichotomized=TRUE,
                       pdf.name="rsp.barplot.pdf",
                       pdf.param=list(width=6, height=4.5),
                       par.param=list(mar=c(4,4,3,2))) {
    
    if (!is.null(biomarker.var)) {
        if (is.null(biomarker.code) & data.class(biomarker.var) == "factor") {
            biomarker.code <- levels(biomarker.var)
        } else if (!is.null(biomarker.code) & data.class(biomarker.var) == "factor") {
            if (length(biomarker.code) == length(levels(biomarker.var))) {
                biomarker.var <- factor(biomarker.var, labels=biomarker.code)
            } else {
                stop("length of biomarker.code needs to be the same of levels of biomarker...\n")
            }
        }
    }
    
    myoutcome <- as.character(outcome.var)
    myoutcome[myoutcome%in%c("")] <- "Missing"
    myoutcome.levels.tmp <- names(table((myoutcome)))
    myoutcome.levels <- c()
    
    if (dichotomized) {
        if (!all(myoutcome %in% c("0", "1", "Missing"))) {
            stop("The dichotomized outcome variable contains values other than 0, 1, and missing).")
        }
        myoutcome[myoutcome%in%c("1")] <- "Rsp"
        myoutcome[myoutcome%in%c("0")] <- "Non-Rsp"
        if (any(myoutcome == "Missing")) {
            myoutcome.levels <- c("Rsp", "Missing", "Non-Rsp")
        } else {
            myoutcome.levels <- c("Rsp", "Non-Rsp")  
        }
    } else {
        if ("CR" %in% myoutcome.levels.tmp) {
            myoutcome.levels <- c(myoutcome.levels, 'CR')
        }
        
        if ("PR" %in% myoutcome.levels.tmp) {
            myoutcome.levels <- c(myoutcome.levels, 'PR')
        }
        
        if ("SD" %in% myoutcome.levels.tmp) {
            myoutcome.levels <- c(myoutcome.levels, 'SD')
        }
        
        if ("NE" %in% myoutcome.levels.tmp) {
            myoutcome.levels <- c(myoutcome.levels, 'NE')
        }
        
        if ("NON CR/PD" %in% myoutcome.levels.tmp) {
            myoutcome.levels <- c(myoutcome.levels, 'NON CR/PD')
        }
        
        if ("PD" %in% myoutcome.levels.tmp) {
            myoutcome.levels <- c(myoutcome.levels, 'PD')
        }
    }
    
    n.levels = length(myoutcome.levels)
    myoutcome <- factor(myoutcome, levels=myoutcome.levels)
    
    if(!is.null(treatment.var))
        trt.levs <- unique(treatment.var)
    
    count <- 1
    n1 <- table(myoutcome[!is.na(reference.var)]) 
    N1 <- sum(n1)
    plottab <- matrix(n1/N1, byrow=TRUE, nrow=1) 
    numtab <- matrix(n1, byrow=TRUE, nrow=1)
    rownames(plottab) <- rownames(numtab) <- ref.popname
    
    if (!is.null(treatment.var)) {
        for(tt in trt.levs) {
            subgroup <- treatment.var == tt & !is.na(reference.var)
            n2 <- table(myoutcome[subgroup]) 
            N2 <- sum(n2)
            plottab <- rbind(plottab, n2/N2)
            numtab <- rbind(numtab, n2)
            n <- nrow(plottab)
            rownames(plottab)[n] <- rownames(numtab)[n] <- paste(ref.popname, tt, sep="\n")
            count <- c(count, count[length(count)])
        }  
    }
    
    if (!is.null(biomarker.var)) {
        count <- c(count, count[length(count)]+1)
        subgroup <- !is.na(biomarker.var)
        n2 <- table(myoutcome[subgroup])
        N2 <- sum(n2,na.rm=TRUE)
        plottab <- rbind(plottab, n2/N2)
        numtab <- rbind(numtab, n2)
        n <- nrow(plottab)
        rownames(plottab)[n] <- rownames(numtab)[n] <- biomarker.varname
        
        if (!is.null(treatment.var)) {
            for(tt in trt.levs) {
                subgroup <- treatment.var == tt & !is.na(biomarker.var)
                n2 <- table(myoutcome[subgroup])
                N2 <- sum(n2, na.rm=TRUE)
                plottab <- rbind(plottab, n2/N2)
                numtab <- rbind(numtab, n2)
                n <- nrow(plottab)
                rownames(plottab)[n] <- rownames(numtab)[n] <- paste(biomarker.varname,tt,sep="\n")
                count <- c(count, count[length(count)])
            }
        }
        
        if (!is.null(biomarker.code)) {
            for(ii in biomarker.code) {
                count <- c(count, count[length(count)]+1)
                subgroup <- biomarker.var == ii
                n2 <- table(myoutcome[subgroup])
                N2 <- sum(n2)
                plottab <- rbind(plottab, n2/N2)
                numtab <- rbind(numtab, n2)
                n <- nrow(plottab)
                rownames(plottab)[n] <- rownames(numtab)[n] <- ii
                if (!is.null(treatment.var)) {
                    for(tt in trt.levs) {
                        subgroup <- treatment.var == tt & biomarker.var == ii
                        n2 <- table(myoutcome[subgroup])
                        N2 <- sum(n2, na.rm=TRUE)
                        plottab <- rbind(plottab, n2/N2)
                        numtab <- rbind(numtab, n2)
                        n <- nrow(plottab)
                        rownames(plottab)[n] <- rownames(numtab)[n] <- paste(ii,tt,sep="\n")
                        count <- c(count, count[length(count)])
                    }
                }
            }
        }
    }
    
    cname.plottab = colnames(plottab)
    plottab <- plottab[nrow(plottab):1,]*100
    
    if (!is.matrix(plottab)) {
        plottab <- as.matrix(plottab)
        colnames(plottab) <- cname.plottab
    }
    
    cname.numtab = colnames(numtab)
    numtab <- numtab[nrow(numtab):1,]
    
    if (!is.matrix(numtab)) {
        numtab <- as.matrix(numtab)
        colnames(numtab) <- cname.numtab
    }
    
    space <- cumsum(table(count))+1 
    space <- space[-length(space)]
    myspace <- rep(0.2, nrow(plottab)) 
    myspace[space] <- 0.8
    
    # Parameters for the pdf file if it's created.
    if (is.null(pdf.param$height)) {
        pdf.param$height <- ifelse(nrow(plottab) > 6, 8, 6) # 1 by nlev subplots
    }
    
    if (is.null(pdf.param$width)) {
        pdf.param$width <- 8 
    }
    
    # Define par parameters
    if(is.null(par.param$mar)) {
        par.param$mar <- c(5,5,4,6)
    }
    
    if(is.null(par.param$font.axis)) {
        par.param$font.axis <- 3
    }
    
    PlotParam(pdf.name, pdf.param, par.param)  
    
    cols <- colorRampPalette(c("deepskyblue", "tomato"))(n.levels)
    bb <- barplot(t(plottab), horiz=T, col=cols, space=myspace,
                  names=rownames(plottab),las=2, axes=FALSE, main=main, xlab="(%)",cex.axis=0.7)
    axis(1, las=1)
    
    if (dichotomized) {
        axis(4, at=bb, paste(paste("N=",rowSums(numtab,na.rm=TRUE),sep=""), 
                             paste("; Rsp=",round(plottab[,"Rsp"],0),"%",sep=""), sep=""),
             las=2, cex.axis=0.8, line=-0.5, tick=F)  
    } else {
        axis(4, at=bb, paste("N=",rowSums(numtab,na.rm=TRUE),sep=""), las=2, cex.axis=0.8, line=-0.5, tick=F)
    }
    
    box()
    
    for (kk in 1:n.levels) {
        mtext(myoutcome.levels[kk], col=cols[kk], side=3, out=FALSE, at=100*kk/n.levels - 50/n.levels, adj=0)
    }
    
    abline(h=bb[space]/2+bb[space-1]/2)
    
    if (!is.null(sub)) {
        mtext(sub, side=1, out=FALSE, cex=0.8, line=4, adj=0)
    }
    
    PlotParam()  
    
    if (return.table) {
        tmp <- matrix(paste(numtab, "(",round(plottab,2),")"), byrow=FALSE, nrow=nrow(numtab))
        tmp <- cbind(rownames(numtab), tmp, rowSums(numtab,na.rm=TRUE))
        colnames(tmp) <- c("Group", colnames(plottab), "Total")
        return(tmp)
    }
}
