#' Log rank test p value and other summary statistics for subgroup analysis
#'
#' This function takes time to event outcome, and one categorical variable (parameter var). It generates log rank p value comparing different groups, and summary statistics
#'
#' @author Ning Leng \email{leng.ning@gene.com}, Alexey Pronin \email{pronin.alexey@gene.com}, and previous team members (see DESCRIPTION)
#'
#' @param fillname
#' (character) variable specifying the content for the cell that located in row one and column one, the default value for fillname is NULL.
#' @param time.unit Default is month.
#' @param ties Default is "efron". To match internal sas results, use "exact". See parameter "ties" in coxph.
#' @param surv.conf.type confidence interval type. Default is "plain". see conf.type in survfit
#' @param digitMedian number of digits to report medians
#' @param digitHR number of digits to report HR
#' @param placebo.code name of the control arm of the treatment variable. If you want to specify placebo code using this parameter, both placebo.code and active.code need to be provided.
#' @param active.code of the treatment/experimental arm of the treatment variable. If you want to specify active code using this parameter, both placebo.code and active.code need to be provided.
#' @note This function takes time to event outcome, and one categorical variable (parameter var).
#' This function generates a table that contains number of patients, median time to event, and log rank test p value ( against first level in var)
#' For more general time to event modeling, see function CoxTab() (e.g. continuous factor, multiple factors, stratification, hazard ratio estimation, etc)
#' @inheritParams CoxTab
#' @examples
#' data(input)
#' LogRankTab(data=input,tte="PFS", cens="PFS.event",var="Arm")
#' @export
#'
LogRankTab <- function(data, tte, cens, var, time.unit="month", surv.conf.type="plain",ties="efron", fillname="",  digitMedian=2, digitHR=2, placebo.code=NULL, active.code=NULL){
  stopifnot(class(data)=="data.frame")

  if(length(var)!=1) stop("only one element should be specified in parameter var!")
  stopifnot(all(c(tte, cens, var)%in%colnames(data)))
  group <- var
  nArms <- length(unique(data[,group]))

  if(class(data[,group])%in%c("numeric","integer")) stop(paste("var column cannot be in class numeric or integer"))

  if (nArms > 1){
    Arms <- unique( data[,group] )
    if (!is.null(placebo.code)) { # reference arm specified
      if(length(placebo.code)!=1)stop("placebo.code should have length 1")
      if(!placebo.code %in% Arms)stop("placebo.code should be an element in treatment column")
      Arms <- c(placebo.code, setdiff(Arms, placebo.code)) # first placebo then others
      if (!is.null(active.code)) {
#        if(length(active.code)!=1)stop("active.code should have length 1")
        if(!all(active.code %in% Arms))stop("active.code should be elements in treatment column")
        if(length(intersect(placebo.code, active.code))>1)
          stop("code cannot be in both active.code and placebo.code!")
        Arms <- c(Arms[1], active.code) #order by specified input
      }
    }
    data[,group] <- factor(as.character(data[,group]), levels=Arms)
  }else{
    data[,group] <- factor(as.character(data[,group]))
  }

  ## Ns
  tabN <- table(data[,cens], data[,group])[2:1,,drop=FALSE]
  csum <- colSums(tabN)
  tabp <- round(prop.table(tabN, margin=2)*100,1)

  tab1 <- tabN
  for(i in 1:ncol(tabN)) {
    tab1[,i] <- paste(tabN[,i], " (", tabp[,i] , "%)", sep="")
  }




  ## Median&Co
  fit <- summary(survfit(as.formula(paste("Surv(",tte,",",cens,") ~ ", group)), data=data, conf.type=surv.conf.type))
  if('numeric' %in% class(fit$tab)) return(matrix(fit$tab, nrow=1, dimnames=list(c(), names(fit$tab))))

  if(nArms==1) {
    med <- as.character(round(fit$tab["median"],digitMedian))
    medci <- paste("(",round(fit$tab["0.95LCL"],digitMedian),";", round(fit$tab["0.95UCL"],digitMedian),")", sep="")
  }
  else {
    med <- as.character(round(fit$tab[,"median"],digitMedian))
    medci <- paste("(",round(fit$tab[,"0.95LCL"],digitMedian),";", round(fit$tab[,"0.95UCL"],digitMedian),")", sep="")
  }
  med[is.na(med)] <- "NA"

  ## quartiles by group
  lev <- levels(as.factor(data[,group]))
  nlev <- length(lev)

  quart <- NULL
  rg <- NULL
  for(i in 1:nlev){
    tt1 <- survfit(as.formula(paste("Surv(",tte,",",cens,") ~ 1")),data=data[which(data[,group]==lev[i]),])
    ind75 <- which(tt1$surv <= 25/100)[1]
    ind25 <- which(tt1$surv <= 75/100)[1]
    quart <- cbind(quart, paste(round(tt1$time[ind25],digitMedian),round(tt1$time[ind75],digitMedian), sep=";"))
    rg <- cbind(rg, paste(round(min(tt1$time),digitMedian), round(max(tt1$time),digitMedian), sep=" to "))
  }

  ##logrank and HR to first level of factor
  pval <- c(" ")
  hr <- c(" ")
  hrci <- c(" ")
  if(nlev>1)
    for(i in 2:nlev){
      logrank <- survdiff(as.formula(paste("Surv(",tte,",",cens,") ~ factor(",group,")")),data=data[which(data[,group]==lev[1]|data[,group]==lev[i]),])
      pval <- cbind(pval, round(1 - pchisq(logrank$chisq, length(logrank$n) - 1),4))
      #cox <- summary(coxph(as.formula(paste("Surv(",tte,",",cens,") ~ factor(",group,")")),data=data[which(data[,group]==lev[1]|data[,group]==lev[i]),], method="breslow"))
      cox <- summary(coxph(as.formula(paste("Surv(",tte,",",cens,") ~ factor(",group,")")),data=data[which(data[,group]==lev[1]|data[,group]==lev[i]),],ties=ties))
      hr <- cbind(hr,round(cox$coefficients[,"exp(coef)"],digitHR))
      hrci <- cbind(hrci, paste("(",round(cox$conf.int[,"lower .95"],digitHR),";", round(cox$conf.int[,"upper .95"],digitHR), ")",sep=""))
    }


  taball <- rbind(tab1,rep("",nlev), med, medci, quart,rg, pval, hr, hrci)

  extracol <- c("Patients with event", "Patients without event",
                paste("Time to event (",time.unit,")",sep=""),
                "     Median (KM)", "     95% CI Median",
                "     25% and 75%-ile", "     Range (inc. cens.)",
                "     p-value (Log-Rank Test)",
                "Hazard Ratio", " 95% CI")

  taball <- cbind(extracol, taball)
  taball <- rbind(c(fillname,lev),c("", paste("N=", csum, sep="")), taball)
  rownames(taball) <- colnames(taball) <- NULL
  taball
}
