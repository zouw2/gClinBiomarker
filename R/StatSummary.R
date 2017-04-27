#' Summary statistics of two-group comparison
#' 
#' This function returns a summary of two group comparison in terms of effect size, lower and upper limits of CI, and p-value.
#' Three types of "outcome" variable can be analyzed using this function: 
#' 1) Cox proportional hazards model for survival outcome using coxph()
#' 2) T-test for continuous outcome using lm()
#' 3) Z-test for binary outcome using prop.test().
#' 
#' @author Alexey Pronin \email{pronin.alexey@gene.com}, Ning Leng \email{leng.ning@gene.com}, and previous team members (see DESCRIPTION)
#'
#' @param outcome a vector specifying the outcome variable. For 'binary' outcome, it should be a vector of 1 or 0. In case of a 'survival' variable, this will be a matrix of two columns: 1) time to event 2) censorship.
#' @param subgroup a vector of row index specifying the subgroup to be included for the analysis. If NULL (default), all data will be used.
#' @param treatment the name of the treatment variable.
#' @param placebo.code the name of the control group within the treatment variable.
#' @param active.code the name of the treatment/experimental group within the treatment variable.
#' @param outcome.type the outcome type of the 'outcome' variable. One of the 3 values - "survival", "binary", or "continuous".
#' @param alpha the confidence level (CI) for point estimate, i.e. 0.05 (default) for 95 percent CI.
#' @param covariate a vector specifying the covariate variables. This can be added to adjust for in the analysis for survival and continuous outcome variable types. Default is NULL.
#' @param strat.factor a vector specifying the stratification variables. This can be added for the survival outcome variable type. Default is NULL.
#' @param return.fit if TRUE, returns a table of summary statistics. Default is FALSE.
#' @param fit.para a list of fitting parameters. Currently only \code{'prop.test.use.continuity.correction'} in use. 
#' If \code{'prop.test.use.continuity.correction' = T} (default), the 'correct' parameter in \code{\link{prop.test}} will be set as TRUE.
#'    
#' @return A named vector of following entries: 
#' if binary - Effect.Size (Proportion Difference), Lower, Upper, P, Rsp.Placebo, Rsp.Active, N.Placebo, N.Active; 
#' if survival - [Events, N, Median Suvival Time] for each group, Effect.Size (Hazard Ratio), Lower, Upper, P; 
#' if continuous - Effect.Size (Mean Difference), Lower, Upper, P.
#' 
#' @note This function requires "survival" package to call the coxph() function. Two treatment arms are required. 
#' Treatment group variable can be forced into a factor. Censorship variable is 1 if an event happened, 0 if censored.
#' 
#' @examples
#' data(input)
#' StatSummary(outcome = input$OS, treatment = input$Arm, placebo.code = "CTRL", active.code = "TRT", outcome.type = "continuous")
#' 
#' @export

StatSummary <- function(outcome,
                        subgroup=NULL,
                        treatment,
                        placebo.code,
                        active.code,
                        outcome.type,
                        alpha=0.05,
                        covariate=NULL,
                        strat.factor=NULL,
                        return.fit=FALSE,
                        fit.para = list('prop.test.use.continuity.correction'=T)) {
    
    # If subgroup is not defined, use all input data
    if (is.null(subgroup)) {
        subgroup <- 1:length(treatment)
    }
    
    # Make the treatment assingment a factor, with ordered levels
    treatment <- factor(treatment, levels=c(placebo.code, active.code))
    # Binary outcome - e.g., response
    if (outcome.type == "binary" ){
        
        y1 <- outcome[subgroup][treatment[subgroup] == active.code]
        n1 <- length(na.omit(y1))            
        y2 <- outcome[subgroup][treatment[subgroup] == placebo.code]
        n2 <- length(na.omit(y2))                 
        
        r1 <- sum(y1, na.rm = TRUE)
        r2 <- sum(y2, na.rm = TRUE)
        
        mytest <- prop.test(c(r1, r2), c(n1, n2), conf.level = 1 - alpha,correct =  fit.para[['prop.test.use.continuity.correction']])
        
        ret <- c("Effect.Size" = r1/n1 - r2/n2
                 , "Lower" = mytest$conf.int[1]
                 , "Upper" = mytest$conf.int[2]
                 , "P" = mytest$p.value
                 , "Rsp.Placebo" = mytest$estimate[2]
                 , "Rsp.Active" = mytest$estimate[1]
                 , "N.Placebo" = n2
                 , "N.Active" = n1)
        names(ret)[5:6] <- c("Rsp.Placebo", "Rsp.Active")
    } # end binary
    
    # Continuous outcome - e.g., blood cholesterol level
    else if (outcome.type=="continuous"){
        
        # No covariate to adjust for
        if (missing(covariate) | is.null(covariate)){
            myfit <- lm(outcome[subgroup] ~ treatment[subgroup])      
            # the last row (2nd) is the 'slope' estimate and its associated quantities
            # which corresponds to the treatment effect size
            coef.1 <- summary(myfit)$coef
            mytest <- coef.1[nrow(coef.1), ]
            myCI <- confint(myfit, level = 1-alpha)[nrow(coef.1), ]
        }
        
        # Covariates to adjust for
        else {
            
            # One variable?
            if (is.vector(covariate) | is.factor(covariate)) {
                myfit <- lm(outcome[subgroup] ~ covariate[subgroup] + treatment[subgroup])
                # the last row (3rd) is the 'slope' estimate and its associated quantities
                # which corresponds to the treatment effect size
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), ]
                myCI <- confint(myfit, level = 1-alpha)[nrow(coef.1), ]        
            }
            
            # More than one variable?
            else {
                nCV <- ncol(covariate)
                mycov <- paste("covariate[subgroup,", 1:nCV, "]", collapse = " + ")
                myformula <- paste("lm(outcome[subgroup] ~"
                                   , mycov, "+ treatment[subgroup])")
                myfit <- eval(parse(text = myformula))
                # (nCV+2)th row (last) is the 'slope' estimate and its associated quantities
                # which corresponds to the treatment effect size
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), ]
                myCI <- confint(myfit, level = 1-alpha)[nrow(coef.1), ]        
            }
        }
        
        ret <- c(mytest[1], myCI, mytest[4])
        names(ret) <- c("Effect.Size","Lower","Upper","P") 
    } # end continuous
    
    # Survival outcome - e.g., progression-free survival
    else if (outcome.type == "survival") {
        #require(survival)
        Response <- outcome[, 1]
        Event <- outcome[, 2]
        ####### Unstratified analysis, not adjusting for any covariate(s)
        if ((missing(covariate)||is.null(covariate)) & (missing(strat.factor)||is.null(strat.factor))){
            myfit <- coxph(Surv(Response[subgroup], Event[subgroup]) ~ treatment[subgroup])
            coef.1 <- summary(myfit)$coef[1:5]
            mytest <- coef.1
        }
        ####### Stratified analysis, not adjusting for any covariate(s)
        else if ((missing(covariate) || is.null(covariate)) & (!missing(strat.factor)&&!is.null(strat.factor))){
            # One stratification variable
            if (is.vector(strat.factor) | is.factor(strat.factor)){
                myfit <- coxph(Surv(Response[subgroup], Event[subgroup]) ~ 
                                   treatment[subgroup] + strata(strat.factor[subgroup]))
                coef.1 <- summary(myfit)$coef[1:5]
                mytest <- coef.1
            }
            # More than one stratification variable
            else {
                nSF <- ncol(strat.factor)
                mysf <- paste("strat.factor[subgroup,", 1:nSF, "]", collapse = ", ")
                myformula <- paste("coxph(Surv(Response[subgroup], Event[subgroup]) ~ treatment[subgroup] + strata("
                                   , mysf, "))")
                myfit <- eval(parse(text = myformula))
                coef.1 <- summary(myfit)$coef[1:5]
                mytest <- coef.1
            }
        }
        
        ####### Unstratified analysis, adjusting for covariate(s)
        else if ((!missing(covariate)&!is.null(covariate)) & (missing(strat.factor)||is.null(strat.factor))){
            # One covariate
            if (is.vector(covariate) | is.factor(covariate)){
                myfit <- coxph(Surv(Response[subgroup], Event[subgroup]) ~ 
                                   covariate[subgroup] + treatment[subgroup])
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), 1:5]
            }
            # More than one covariate
            else {
                nCV <- ncol(covariate)
                mycov <- paste("covariate[subgroup,", 1:nCV, "]", collapse = " + ")
                myformula <- paste("coxph(Surv(Response[subgroup], Event[subgroup]) ~"
                                   , mycov, "+ treatment[subgroup])")
                myfit <- eval(parse(text = myformula))
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), 1:5]
            }
            
        }
        ####### Stratified analysis adjusting for covariate(s)
        else {
            # One covariate and one stratification variable
            if ((is.vector(covariate) | is.factor(covariate)) &
                (is.vector(strat.factor) | is.factor(strat.factor))){
                myfit <- coxph(Surv(Response[subgroup], Event[subgroup]) ~ 
                                   covariate[subgroup] + treatment[subgroup] + 
                                   strata(strat.factor[subgroup]))
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), 1:5]
            }
            # Multiple covariates and one stratification variable
            else if (is.vector(strat.factor) | is.factor(strat.factor)){
                nCV <- ncol(covariate)
                mycov <- paste("covariate[subgroup,", 1:nCV, "]", collapse = " + ")
                myformula <- paste("coxph(Surv(Response[subgroup], Event[subgroup]) ~"
                                   , mycov, "+ treatment[subgroup] + 
                                   strata(strat.factor[subgroup]))")
                myfit <- eval(parse(text = myformula))
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), 1:5]
            }
            # One covariate and multiple stratification variables
            else if (is.vector(covariate) | is.factor(covariate)){
                nSF <- ncol(strat.factor)
                mysf <- paste("strat.factor[subgroup,", 1:nSF, "]", collapse = ", ")
                myformula <- paste("coxph(Surv(Response[subgroup], Event[subgroup]) ~ 
                                   covariate[subgroup] + treatment[subgroup] + strata("
                                   , mysf, "))")
                myfit <- eval(parse(text = myformula))
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), 1:5]
            }
            # Multiple covariates and multiple stratification variables
            else {
                nCV <- ncol(covariate)
                mycov <- paste("covariate[subgroup,", 1:nCV, "]", collapse = " + ")
                nSF <- ncol(strat.factor)
                mysf <- paste("strat.factor[subgroup,", 1:nSF, "]", collapse = ", ")
                
                myformula <- paste("coxph(Surv(Response[subgroup], Event[subgroup]) ~"
                                   , mycov, "+ treatment[subgroup] + strata("
                                   , mysf, "))")
                myfit <- eval(parse(text = myformula))
                coef.1 <- summary(myfit)$coef
                mytest <- coef.1[nrow(coef.1), 1:5]
            }
        }
        
        Effect.Size <- mytest[2]
        Lower <- exp(mytest[1] - qnorm(1 - alpha/2) * mytest[3])
        Upper <- exp(mytest[1] + qnorm(1 - alpha/2) * mytest[3])
        
        ret <- c(as.numeric(t(summary(survfit(Surv(Response[subgroup], Event[subgroup]) ~ treatment[subgroup]))$table[,c("events","n.start","median"),drop=FALSE])),
                 Effect.Size, Lower, Upper, mytest[5])
        
        names(ret) <- c(paste(rep(levels(treatment),each=3), rep(c("events","n","MST"),2),sep=".")
                        ,"Effect.Size","Lower","Upper","P")
    } # end survival
    
    else {
        stop("Please input outcome.type as one of these three options: binary, continuous or survival...\n")
    }
    
    if(!return.fit) { 
        return(ret)
    } else {
        return(list(ret, myfit))
    }
}