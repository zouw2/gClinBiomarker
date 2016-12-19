#' @export
#' @title Summary multiple covariates, test across treatment and/or population 
#' @param var a vector of covariate names - the clinical covariate to test
#' @param var.name preferred display names of the clinical covariates 
#' If it is NULL, var will be used.
#' @param ordered.factor.levels.list a list indicates ordered levels for ordered.factor. Each ordered.factor
#' should have a corresponding element in this list. 
#' @return output object is a matrix with summary statistics. It can be passed to knitr::kable(). 
#' @note trt allows for more than 2 levels. However, only 2 levels are allowed for bep.
#' For more general use, a user can specify trt to get summary statistics for any
#' sub-group defination (and leave bep as NULL).
#' @inheritParams SumSingle
#' @summary This function provides summary statistics of a vector of clinical covariates. Using default parameters,
#' the function provides a table to compare summary statistics in ITT vs. in BEP (biomarker evaluable population),
#' within treatment arm
#' @author Ning Leng, Alexey Pronin, and previous team members (see DESCRIPTION)
#' @examples
SummaryVars <- function (data, var, var.name = NULL, 
			trt = NULL, trt.name = NULL, 
      bep = NULL, bep.name = NULL, bep.indicator=1, compare.itt=TRUE,itt.name="ITT",
			var.class, ordered.factor.levels.list=NULL,
			cont.show = c("N" ,"Mean","Median", "Min-Max","NA's"),
			digits = 2, trt.order = NULL, test.bep=FALSE, 
				 na.action = "error") 
{
  stopifnot(na.action%in% c("na.omit", "error"))
  stopifnot(class(data) == "data.frame")
  if(is.null(trt) & is.null(bep))stop("trt and bep are both empty! need to specify at least one of them")
  if(!all(c(var, trt, bep) %in% colnames(data)))stop("var, trt and bep should have matched column names in the input data!")
  if(!is.null(bep)) if(nlevels(as.factor(data[,bep]))<2)stop("subpopulation column has only one unique value!")
  possible.show <- c("N" ,"Mean","SEM", "SD","Median",
                     "Min","Max" ,"Min-Max","1st Qrtl.","3rd Qrtl.",
                     "IQR" ,"NA's")
  if(length(setdiff(cont.show, possible.show))>0) 
    stop(paste("possible cont.show elements are:", possible.show ))
  if(test.bep & is.null(bep)){
    test.bep <- FALSE
    message("test.bep=TRUE but bep is not specified. Reset test.bep as FALSE")
  }

  if(length(var)!= length(var.class)) stop("length of var.class should match length of var!")
 
  possible.class <-c("categorical","numeric","ordered.factor")
  if(!all(var.class%in%possible.class))stop(paste('var.class should be in', paste(possible.class,collapse=",")))
  
  k.var <- length(var)
  for(i in 1:k.var){
   tmp <- try(SumSingle (data=data, var=var[i], 
			trt = trt, trt.name = trt.name, 
                        bep = bep, bep.name = bep.name, bep.indicator=bep.indicator, 
			compare.itt=compare.itt, itt.name=itt.name,
			var.class=var.class[i], ordered.factor.levels=ordered.factor.levels.list[[var[i]]],
			cont.show = cont.show, 
			digits = digits, trt.order = trt.order, test.bep=test.bep, 
				 na.action = na.action))
   if(class(tmp)=="try-error") stop(paste("error in", var[i] ))
   if(i==1)kcol <- ncol(tmp)
   line1 <- rep("", kcol) 
   tmp2 <- rbind(line1, tmp)
   rownames(tmp2)[1] <- ifelse(is.null(var.name[i]),var[i], var.name[i]) 
   if(i==1)mat <- tmp2 
   if(i>1)mat <- rbind(mat, tmp2)
  }
 
 mat 

}

