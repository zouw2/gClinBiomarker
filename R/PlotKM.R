#' Generate KM curve(s) for full population or subgroups defined by single factor
#' 
#' @author Ning Leng \email{leng.ning@gene.com}, Alexey Pronin \email{pronin.alexey@gene.com}, and previous team members (see DESCRIPTION)
#' 
#' @param bep name of the column which indicates subpopulation (e.g. biomarker evaluable population)
#' If parameter bep is not defined, the KM curve(s) will be draw using all samples.
#' If bep is defined, the KM curve(s) will be draw using only samples in BEP.
#' @param subgroups name of the column which indicates the subgroups (e.g. treatment group).
#' this column is expected to be categorical. If this column is in character class and subgroups.levels is not specified, 
#' it will be converted to a factor by factor() function. If subgroups.levels is defined, the column will be converted to
#' a factor following the level order in subgroups.levels. 
#' In the legend, the subgroups will be ordered based on the order of factor levels. 
#' @param subgroups.levels levels in the subgroups
#' @param subgroups.labels preferred labels for the subgroups. The order in subgroups.labels should match subgroups.levels
#' @param plot.nrisk whether show number of patients at risk at the below the graph. If it is specified as TRUE, number of patients
#' at risk will be summarized by subgroup.
#' @param nrisk.interval interval to summarize number of patients at risk . Default is to summarize every 2 (months)
#' @param cex.nrisk font size for the number of patients at risk.
#' @param plot.grid whether show horizontal grids
#' @param grids horizontal grids
#' @param plot.legend whether show legend
#' @param plot.median whether show median TTE of each subgroup. (won't show if median TTE is NA)
#' @param median.cex font size of marked median. This parameter will be ignored if plot.median=FALSE
#' @param xlim,ylab,xlab,main,col,lty,lwd,sub,ylim see \code{\link{plot}}
#' @param  ... additional parameters for \code{\link{plot}} 
#' 
#' @note This function generates KM curve(s) for full population (when parameter subgroups is not defined) 
#' or subgroups (when parameter subgroups is defined).
#'
#' @importFrom graphics plot axis mtext grid box polygon lines legend
#' @importFrom survival survfit  
#'
#' @inheritParams CompareKM
#' @inheritParams SumSingle
#' 
#' @example data(input)
#' sample.data <- input
#' PlotKM(data=sample.data, tte="OS",cen="OS.CNSR", main="OS ITT by treatment", subgroups="Arm")
#' @export

PlotKM <- function(data, tte, cens, subgroups=NULL, subgroups.levels=NULL, subgroups.labels=NULL,
		   bep=NULL, bep.indicator=1,
                    plot.nrisk=TRUE, nrisk.interval=2, cex.nrisk=.8,
                    plot.grid=TRUE, grids=seq(0,1,0.1), plot.legend=TRUE,
                    col=NULL, lty=1, lwd=3,
                    xlab="Months To Event Or Censoring", ylim=c(0,1), xlim=NULL,  ylab="Survival Probability",
                    main="",sub="", plot.median=FALSE,median.cex=.8,digits=2,
		    pdf.name=NULL, pdf.param=list(height=5), par.param=list(mar=c(12,9,3,2))){
  

	stopifnot(class(data) == "data.frame")
	if(!is.null(bep))if(! bep %in% colnames(data))stop("bep should have matched column names in the input data!")
	if(!is.null(subgroups))if(! subgroups %in% colnames(data))stop("bep should have matched column names in the input data!")
	if(!is.null(bep))data <- data[which(data[,bep]==bep.indicator),]
	if(is.null(subgroups)) subgroups <- "tmp.subgroup"
	data$tmp.subgroup <- ""
	if(is.null(subgroups.levels))data[,subgroups] <- factor(data[,subgroups])
	if(!is.null(subgroups.levels)){
		data[,subgroups] <- factor(data[,subgroups], levels=subgroups.levels)
		if(!is.null(subgroups.labels)) levels(data[,subgroups]) <- subgroups.labels
}

	if(is.null(par.param$mar))par.param$mar <- c(12,9,3,2)
  
	
	strat.vec <- data[,subgroups]
	nlev <- nlevels(strat.vec)
	if(is.null(col))col <-  1:nlev
  	if(is.null(subgroups.labels))subgroups.labels <- levels(strat.vec)
    	
	fit <- survfit(as.formula(paste("Surv(",tte,",",cens,") ~ ", subgroups)), data=data)    
	

	# xlim
   	if(is.null(xlim)){
    	xlim2 <- max(data[,tte],na.rm=TRUE)*1.05
    	if(plot.nrisk)
      		xlim1 <- -0.5
    	else
      		xlim1 <- 0
    	xlim <- c(xlim1, xlim2)
  }

  
	  if(plot.nrisk) {
		fit2 <- fit
   		time.pt <- seq(0,xlim[2],nrisk.interval)
    		ix = 0
    		n.risk <- c()
		if(nlev==1) fit2$strata <- length(fit$n.risk)
    		for (kk in 1:(length(fit2$strata)))
  		  {
      		fit.n.risk = fit2$n.risk[(ix+1) : (ix+fit2$strata[kk])]
     		fit.time = fit2$time[(ix+1) : (ix+fit2$strata[kk])]
      		tmp = findInterval(time.pt, fit.time)
      		n.risk <- rbind(n.risk, ifelse(tmp<length(fit.time), fit.n.risk[tmp+1], 0))
      		ix = ix + fit2$strata[kk]
    		}
    	dimnames(n.risk)[[2]] = time.pt
  	}
	  
	if(plot.nrisk){
    		if(par.param$mar[1] < 4+nlev) par.param$mar[1] <- 4+ nlev 
  	}
  

 	PlotParam(pdf.name, pdf.param, par.param) 
	
	plot(fit,col=col,lwd=lwd,xlab="", ylab=ylab,lty=lty,
       		main=main, sub=sub, axes=FALSE, ylim=ylim, xlim=xlim, conf.int=F) 
	box()

  	mtext(xlab,side=1, line=2)
  
	if(plot.legend & nlev > 1)
    		legend("topright",subgroups.labels, lwd=2, col=col, lty=lty)
  
	axis(1,at=seq(0,xlim[2],nrisk.interval),seq(0,xlim[2],nrisk.interval))
 	axis(2,at=seq(ylim[1],ylim[2],0.1), seq(ylim[1],ylim[2],0.1),las=2); abline(h=0)
  
	if(plot.grid) abline(h=grids, col="gray")
  	if(plot.nrisk){
    		for(i in 1:nlev){
      			mtext(side=1, at=xlim[1]-1.2, line=i+3,text=levels(strat.vec)[i],col=col[i],adj=1,cex=cex.nrisk*3/4)
      			mtext(side=1, at=time.pt, line=i+3,text=n.risk[i,],col=col[i],cex=cex.nrisk)
    			}
     	}

	if(nlev>1)meds <- summary(fit)$table[,"median"]
	if(nlev==1)meds <- summary(fit)$table["median"]
	if(plot.median){
		for(i in 1:nlev){
		if(!is.na(meds[i])){
			text(x=meds[i],y=0.05,labels=round(meds[i], digits), col=col[i],cex=median.cex)
			lines(c(meds[i],meds[i]), c(0,.5),lty=3, lwd=1, col=col[i])
}}
	}
 PlotParam()
 out <- fit
}


