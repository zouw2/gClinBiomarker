#' @export
#' @title Summary a single covariate, test across treatment and/or population 
#' @param data
#' @param var name of the clinical covariate to test
#' @param trt name of the treatment column
#' @param trt.name preferred display name of the treatment variable
#' If it is NULL, trt will be used.
#' @param bep name of the column which indicates subpopulation (e.g. biomarker evaluable population)
#' @param bep.name preferred display name of the subpopulation (e.g. biomarker evaluable population)
#' If it is NULL, bep will be used.
#' @param bep.indicator In the subpopulation column, which value is used
#' to define the subpopulation (e.g. biomarker evaluable population). 
#' Default is 1. The non-subpopulation enrties is not allowed to be specified as NA.
#' @param compare.itt whether compare subpopulation vs. ITT, or compare 
#' subpopulation vs. non-subpopulation. Default is TRUE
#' @param var.class class of the variable. possible categories are "numeric", "categorical" and
#' "ordered.factor".  "ordered.factor" can be used to categorical variable with
#' ordered levels - e.g. IC score 0/1/2/3. If class is ordered.factor ,
#' ordered.factor.levels need to be specified.
#' @param ordered.factor.levels ordered levels of the ordered factor. 
#' @param cont.show what summary statistics to show for a continuous covariate.
#' Default is c("N" ,"Mean","Median", "Min-Max","NA's").
#' Possible options are "N" ,"Mean","SEM", "SD","Median",
#' "Min","Max" ,"Min-Max","1st Qrtl.","3rd Qrtl.","IQR" ,"NA's"
#' @param digits digits for rounding
#' @param trt.order If the user wants to display the treatments in a certain
#' order, it can be defined here. All elements in trt.order should be the same
#' unique values in the treatment column.
#' @param na.action defaults to "na.omit". Possible options are "na.omit", "error"
#' When it is specified as "na.omit", entries with missing trt or bep
#' will be automatically removed before calculation.
#' @param compare.itt whether show summary statistics of BEP and ITT. If it is FALSE,
#' the output will show summary statistics of BEP and nonBEP. Default is TRUE.
#' @param test.bep whether test across subpopulations within treatment arm. If class is numeric,
#' kruskal wallis rank sum test will be performed. If class is categorical, fisher's exact test will be performed.
#' If class is ordered.factor, cmh test will be performed. The test is always performed between BEP vs nonBEP.
#' P value columns will be included in the output table if it is specified as TRUE.
#' Testing is not recommendated if either BEP of non-BEP has small sample size.
#' @note trt allows for more than 2 levels. However, only 2 levels are allowed for bep.
#' For more general use, a user can specify trt to get summary statistics for any
#' sub-group defination (and leave bep as NULL).
#' @author Ning Leng, Alexey Pronin, Wei Zou, Ron Yu, YuanYuan Xiao, Christina Rabe
#' @examples 

SumSingle <- function (data, var, 
			trt = NULL, trt.name = NULL, 
      bep = NULL, bep.name = NULL, bep.indicator=1, compare.itt=TRUE,
			var.class, ordered.factor.levels=NULL,
			cont.show = c("N" ,"Mean","Median", "Min-Max","NA's"),
			digits = 2, trt.order = NULL, test.bep=FALSE, 
				 na.action = "error") 
{
  na.action <- match.arg(na.action, c("na.omit", "error"))
  stopifnot(class(data) == "data.frame")
  if(!all(c(var, trt, bep) %in% colnames(data)))stop("var, trt and bep should have matched column names in the input data!")
  if((!is.null(bep)) & nlevels(as.factor(data[,bep]))<2)stop("subpopulation column has only one unique value!")
  possible.show <- c("N" ,"Mean","SEM", "SD","Median",
                     "Min","Max" ,"Min-Max","1st Qrtl.","3rd Qrtl.",
                     "IQR" ,"NA's")
  if(length(setdiff(cont.show, possible.show))>0) 
    stop(paste("possible cont.show elements are:", possible.show ))
  if(test.bep & is.null(bep)){
    test.bep <- FALSE
    message("test.bep=TRUE but bep is not specified. Reset test.bep as FALSE")
  }
  
  possible.class <-c("categorical","numeric","ordered.factor")
  if(!all(var.class%in%possible.class))stop(paste('var.class should be in', paste(possible.class,collapse=",")))

  if(var.class=="ordered.factor"){
    if(is.null(ordered.factor.levels)) stop("If class is ordered.factor,
		ordered.factor.levels need to be specified.")
    if(!is.null(ordered.factor.levels)){
      ordered.factor.levels<- as.character(ordered.factor.levels)
      if(!identical(sort(ordered.factor.levels),sort(levels(factor(data[,var])))))stop("ordered factor levels should match unique elements in the variable!")
      data[,var] <- factor(data[,var],levels=ordered.factor.levels, ordered=TRUE)
      Lev <- ordered.factor.levels
    }
  }
  
  if( (!is.null(bep))) bep.name <- ifelse(is.null(bep.name), bep, bep.name)
  if( (!is.null(trt))) trt.name <- ifelse(is.null(trt.name), trt, trt.name)
  

# TRT missing value
  if (!is.null(trt)) {
    if (any(is.na(data[, trt]))) {
      if (na.action == "na.omit") {
        data <- data[complete.cases(data[, trt]), ]
        warning("There were observations without 'trt' information! These were omitted (na.action=\"na.omit\")!")
      }
      else stop("There were observations without 'trt' information!")
    }
  }

# bep missing value
  if (!is.null(bep)) {
    if (length(bep) == 1 && bep %in% colnames(data)) {
      if (any(is.na(data[, bep]))) {
        if (na.action == "na.omit") {
          data <- data[complete.cases(data[, bep]), ]
          warning("There were observations without 'bep' information! These were omitted (na.action=\"na.omit\")!")
        }
        else stop("There were observations without 'bep' information!")
      }
    }
  }

 


  # if trt is NULL, create a column with all 1s as indicator
  if (is.null(trt)) {
    data$trt <- rep(1, nrow(data))
    trt.lev <- unique(data$trt)
  } else {
    if (!is.null(trt.order)) {
      tmp <- levels(factor(data[, trt]))
      if(!identical(sort(trt.order),sort(tmp))) stop("trt.order should match levels in trt column!")
      data$trt <- factor(data[, trt], levels = trt.order)
    }
    else data$trt <- factor(data[, trt])
    trt.lev <- levels(data$trt)
  }

  # if bep is NULL, create a column with all 1s as indicator
 
    
    if (!is.null(bep)) {
      if(compare.itt){
        data[,bep] <- ifelse(data[,bep]%in%bep.indicator,1,0)
        data$ITT <- rep(1, nrow(data))
        bep.name <- c("ITT",bep.name)
        bep <- c("ITT",bep)
      }
      if(!compare.itt){
        bep.l <- paste0(bep.name,"_", c(bep.indicator,paste0("not_",bep.indicator)))
        data[,bep.l[1]] <- ifelse(data[,bep]%in%bep.indicator,1,0)
        data[,bep.l[2]] <- ifelse(data[,bep]%in%bep.indicator,0,1)
        bep <- bep.l
        bep.name <- bep.l
      }
  }
  
  # output matrix (trt by population)
  res <- matrix(ncol = sum(1, length(trt.lev) * length(bep)), 
                nrow = 0)

  if (var.class== "categorical") {
    data[,var] <- factor(data[,var])
    Lev <- levels(data[, var])
  }
  result <- vector("list", length(trt.lev))
  names(result) <- rep("",length(trt.lev))
  
  if (!is.null(trt.name)) 
      names(result) <- trt.lev
  

  res.ind <- 1
  for (i in 1:length(trt.lev)) {
    trt.mat <- data[which(data$trt == trt.lev[i]), ]
    for (j in 1:length(bep.name)) {
      bep.mat <- trt.mat[which(trt.mat[, bep[j]]==1), ]
      Nna <- length(which(is.na(bep.mat[, var])))
      Na <- nrow(bep.mat) - Nna

      if (var.class%in%c("categorical","ordered.factor") ){
        res.bep <- data.frame(1)
          res.bep$Total <- Na
          res.bep$"NA's" <- Nna
          for (k in Lev) {
            res.bep[, k] <- length(which(bep.mat[, var] ==  k))
            res.bep[, k] <- paste(res.bep[, k], " ", 
                                    "(", 100 * round(res.bep[, k]/Na, digits + 
                                                       2), "%)", sep = "")
          }
        #}
        res.bep <- t(res.bep[, -1])
      } else {
        smry <- summary(bep.mat[, var], digits = 16)
        res.bep <- data.frame(1)
          res.bep$N <- Na
          res.bep$Mean <- round(ifelse(is.nan(smry["Mean"]), 
                                              NA, smry["Mean"]),  digits=digits)
          res.bep$SEM <- round(sd(bep.mat[, var], 
                                         na.rm = TRUE)/sqrt(length(which(!is.na(bep.mat[, 
                                                            var])))),  digits=digits)
          res.bep$SD <- round(sd(bep.mat[, var], 
                                        na.rm = TRUE),  digits=digits)
          res.bep$Median <- round(smry["Median"], 
                                         digits=digits)
          res.bep$Min <- round(smry["Min."],  digits=digits)
          res.bep$Max <- round(smry["Max."],  digits=digits)
          if (any(is.na(smry[c("Min.", "Max.")]))) 
            res.bep$"Min-Max" <- NA
          else res.bep$"Min-Max" <- paste(round(smry["Min."], 
                                                        digits=digits), 
					round(smry["Max."],  digits=digits), 
                                          sep = "...")
        
          res.bep$"1st Qrtl." <- round(smry["1st Qu."],  digits=digits)
          res.bep$"3rd Qrtl." <- round(smry["3rd Qu."],   digits=digits)
          res.bep$IQR <- round(smry["3rd Qu."] -   smry["1st Qu."],  digits=digits)
          res.bep$"NA's" <- Nna
          res.bep <- t(res.bep[cont.show])

      }
      if (j == 1) 
        p.res <- res.bep
      else p.res <- cbind(p.res, res.bep)
    }
    colnames(p.res) <- bep.name
    # If all ITT are in BEP, no test can be performed
    if(test.bep){
      if(length(which(trt.mat[,bep[2]]!=1))==0){
        test.bep  <-  FALSE
        message("All ITT patients are in BEP. bep.test is set to FALSE")
      }  
    }
    if(test.bep){
      if(var.class=="numeric")
        tt <- kruskal.test(list(trt.mat[which(trt.mat[,bep[2]]!=1),var],
                     trt.mat[which(trt.mat[,bep[2]]==1),var]))$p.value
    if(var.class=="categorical")
      tt <- fisher.test(table(trt.mat[,c(var,bep[2])]))$p.value
    if(var.class=="ordered.factor"){
      require(coin)
      tmp.mat <- trt.mat
      tmp.mat[[bep[2]]] <- as.factor(tmp.mat[[bep[2]]])
      tt <- pvalue(cmh_test(as.formula(paste0(var,' ~ ',bep[2])),data=tmp.mat))
    }
    p.res <- cbind(p.res, pvalue=c(round(tt,digits),rep("",nrow(p.res)-1)))
  }
    if(length(result)>1)colnames(p.res) <- paste0(colnames(p.res),"(",names(result)[i],")")  
    result[[i]] <- p.res
  }
  
  colns <- unlist(sapply(result,colnames,simplify=F))
  rowns <- unique(unlist(sapply(result,rownames,simplify=F)))
  outmat <- matrix("",nrow=length(rowns),ncol=length(colns),dimnames=list(rowns, colns))
  for(i in 1:length(result)) outmat[rownames(result[[i]]),colnames(result[[i]])] <- result[[i]]
  outmat
}

