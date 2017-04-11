#' Generate KM curve(s) for full population or subgroups defined by single factor or multiple factors
#'
#' The function generates KM curves for full population or subgroups. The subgroups may be defined as a single factor or multiple factors.
#' 
#' @author Ning Leng \email{leng.ning@gene.com}, Alexey Pronin \email{pronin.alexey@gene.com}, and previous team members (see DESCRIPTION)
#' 
#' @param bep name of the column which indicates subpopulation (e.g. biomarker evaluable population)
#' If parameter bep is not defined, the KM curve(s) will be draw using all samples.
#' If bep is defined, the KM curve(s) will be draw using only samples in BEP.
#' @param subgroups name (or names) of the column which indicates the subgroups (e.g. treatment group).
#' If it is defined as name of a single column, this column is expected to be categorical. If this column is in character class and subgroups.levels is not specified, 
#' it will be converted to a factor by factor() function. If subgroups.levels is defined, the column will be converted to
#' a factor following the level order in subgroups.levels. 
#' In the legend, the subgroups will be ordered based on the order of factor levels. 
#' The parameter subgroups can also be a vector of multiple column names.
#' In this case, subgroups will be defined by classes in multiple columns (e.g. treatment and biomarker)
#' @param subgroups.levels levels in the subgroups. It should be a vector if the parameter subgroups is a single column name. 
#' It should be a list if more than one columns are specified in the prarameter subgroups. Each element of the list should contains a vector.
#' The elements in the list should match the multiple columns defined in parameter subgroups.
#' @param subgroups.labels preferred labels for the subgroups. 
#' subgroups.levels should be provided if subgroupd.labels is specified. The order in subgroups.labels should match subgroups.levels.
#' It should be a vector if the parameter subgroups is a single column name. 
#' It should be a list if more than one columns are specified in the prarameter subgroups. Each element of the list should contains a vector.
#' The elements in the list should match the multiple columns defined in parameter subgroups.
#' @param plot.nrisk whether show number of patients at risk at the below the graph. If it is specified as TRUE, number of patients
#' at risk will be summarized by subgroup.
#' @param nrisk.interval interval to summarize number of patients at risk . Default is to summarize every 2 (months)
#' @param cex.nrisk font size for the number of patients at risk.
#' @param plot.grid whether show horizontal grids
#' @param grids horizontal grids
#' @param plot.legend whether show legend
#' @param legend.loc legend location. a single keyword from the list "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center". 
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
#' @examples
#' data(input)
#' sample.data <- input
#' PlotKM(data=sample.data, tte="OS",cens="OS.CNSR", main="OS ITT by treatment", subgroups="Arm")
#' @export

PlotKM <- function(data, tte, cens, subgroups=NULL, subgroups.levels=NULL, subgroups.labels=NULL,
		   bep=NULL, bep.indicator=1,
                    plot.nrisk=TRUE, nrisk.interval=2, cex.nrisk=.8,
                    plot.grid=TRUE, grids=seq(0,1,0.1), plot.legend=TRUE,legend.loc="topright",
                    col=NULL, lty=NULL, lwd=3,
                    xlab="Months To Event Or Censoring", ylim=c(0,1), xlim=NULL,  ylab="Survival Probability",
                    main="",sub="", plot.median=FALSE,median.cex=.8,digits=2,
		    pdf.name=NULL, pdf.param=list(height=5), par.param=list(mar=c(12,9,3,2))){
  

	stopifnot(class(data) == "data.frame")
	if(!is.null(bep))if(! bep %in% colnames(data))stop("bep should in column names in the input data!")
	if(!is.null(subgroups))if(! all(subgroups %in% colnames(data)))stop("names in 'subgroups' should be in column names in the input data!")
	if(!is.null(bep))data <- data[which(data[,bep]==bep.indicator),]
	
	if(!is.null(subgroups.labels) & is.null(subgroups.levels)) stop("subgroups.levels should be provided if subgroups.labels is specified!")

	if(length(subgroups)==1){
	if(!is.null(subgroups.levels))if(nlevels(factor(data[[subgroups]]))!=length(subgroups.levels))
		stop(paste("number of elements in subgroups.levels should match number of unique values in",subgroups ))
	
	if(!is.null(subgroups.labels))if(nlevels(factor(data[[subgroups]]))!=length(subgroups.labels))
		stop(paste("number of elements in subgroups.labels should match number of unique values in",subgroups ))

	}

	if(length(subgroups)>1){
		if(!is.null(subgroups.levels))if(length(subgroups)!=length(subgroups.levels))
		stop(paste("number of elements in subgroups.levels should match number of column names in parameter 'subgroups'"))
	
		if(!is.null(subgroups.labels))if(length(subgroups)!=length(subgroups.labels))
		stop(paste("number of elements in subgroups.labels should match number of column names in parameter 'subgroups'"))

		
		for(i in 1:length(subgroups)){
		
		if(!is.null(subgroups.levels))if(nlevels(factor(data[[subgroups[i]]]))!=length(subgroups.levels[[i]]))
		stop(paste("number of elements in subgroups.levels should match number of unique values in",subgroups[i] ))
	
		if(!is.null(subgroups.labels))if(nlevels(factor(data[[subgroups[i]]]))!=length(subgroups.labels[[i]]))
		stop(paste("number of elements in subgroups.labels should match number of unique values in",subgroups[i] ))
	
	}}
	
	subgroups.ori <- subgroups
	subgroups <- "tmp.subgroup"
	n.subs <- length(subgroups.ori)
	if(n.subs==0) data$tmp.subgroup <- ""
	if(n.subs==1) data$tmp.subgroup <- data[[subgroups.ori]]
	if(n.subs>1) data$tmp.subgroup <-  apply(data[,subgroups.ori],1,function(i)paste0(i,collapse=","))
	
	if(is.null(subgroups.levels)){
		tmp.levels <- sapply(data[,subgroups.ori],function(i)levels(factor(i)), simplify=FALSE)
		if(n.subs>1)data[,subgroups] <- factor(data[,subgroups], levels = apply(expand.grid(tmp.levels[n.subs:1])[,n.subs:1],1,function(i)paste0(i,collapse=",")))
		if(n.subs<=1)data[,subgroups] <- factor(data[,subgroups])
	}
	if(!is.null(subgroups.levels)){
		if(length(subgroups.ori)>1) subgroups.levels <- apply(expand.grid(subgroups.levels[n.subs:1])[,n.subs:1],1,function(i)paste0(i,collapse=","))
		data[,subgroups] <- factor(data[,subgroups], levels=subgroups.levels)
		if(!is.null(subgroups.labels)) {
		if(length(subgroups.ori)>1) subgroups.labels <- apply(expand.grid(subgroups.labels[n.subs:1])[,n.subs:1],1,function(i)paste0(i,collapse=","))
			levels(data[,subgroups]) <- subgroups.labels
}}

	if(is.null(par.param$mar))par.param$mar <- c(12,9,3,2)
  
	col.v <- c("blue","red","darkgreen","brown","darkgrey","skyblue","purple","cyan","pink","oragne")
	strat.vec <- data[,subgroups]
	nlev <- nlevels(strat.vec)
	if(n.subs<=1){
	  if(is.null(col)) col <-  col.v[1:nlev]
	  if(is.null(lty)) lty <- 1
	}
	# if more than one factors, use color to distinguish first several factors and use lty to distingush the last factor
	if(n.subs>1) {
	  nfirst <- length(unique(apply(data[,subgroups.ori[-length(subgroups.ori)], drop=FALSE],1,function(i)paste0(i,collapse=","))))
	  nlast <- length(unique(data[,subgroups.ori[length(subgroups.ori)]]))
	  if(is.null(col))col <- col.v[rep(1:nfirst, each=nlast)]
	  if(is.null(lty))lty <- rep(1:nlast, nfirst)
	}

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
       		main=main, sub=sub, axes=FALSE, ylim=ylim, xlim=xlim, conf.int=F, mark.time=TRUE) 
	box()

  	mtext(xlab,side=1, line=2)
  
	if(plot.legend & nlev > 1)
    		legend(legend.loc,subgroups.labels, lwd=2, col=col, lty=lty, bg="white")
  
	axis(1,at=seq(0,xlim[2],nrisk.interval),seq(0,xlim[2],nrisk.interval))
 	axis(2,at=seq(ylim[1],ylim[2],0.1), seq(ylim[1],ylim[2],0.1),las=2); abline(h=0)
  
	if(plot.grid) abline(h=grids, col="gray",lty=3)
  	if(plot.nrisk){
    		for(i in 1:nlev){
      			mtext(side=1, at=xlim[1]-1.2, line=i+3,text=levels(strat.vec)[i],col=col[i],adj=1,cex=cex.nrisk*3/4)
      			mtext(side=1, at=time.pt, line=i+3,text=n.risk[i,],col=col[i],cex=cex.nrisk)
    			}
     	}

	if(nlev>1)meds <- summary(fit)$table[,"median"]
	if(nlev==1)meds <- summary(fit)$table["median"]
	if(plot.median){
		lines(c(0,max(meds, na.rm=T)),c(.5,.5), col="gray", lty=2)
		jj <- 0
		for(i in 1:nlev){
		if(!is.na(meds[i])){
			text(x=meds[i],y=0.05,labels=paste0(subgroups.labels[i],"\nmedian ",round(meds[i], digits)), col=col[i],cex=median.cex)
			lines(c(meds[i],meds[i]), c(0,.5),lty=3, lwd=1, col=col[i])
}
		if(is.na(meds[i])){
			text(x=xlim[1]+(diff(xlim)/10),y=0.05+(ylim[1]+(diff(ylim)/10)*jj),labels=paste0(subgroups.labels[i],"\nmedian NA"), col=col[i],cex=median.cex)
		jj <- jj+1
		}
		}
	}
 PlotParam()
 #out <- CoxTab()
 out <- ""
}


