#' @keywords internal
 
ForestPlotSurvival <- function(data,
                               outcome.var,
                               treatment.var=NULL,  
                               biomarker.var,
                               covariate.var=NULL,
                               strat.var=NULL,
                               placebo.code=NULL,
                               active.code=NULL,
                               biomarker.code=NULL,
                               alpha=0.05,
                               main=NULL,
                               sub=NULL,
                               clip=NULL,
                               cex.headings=1.1,
                               cex.note=1,
                               cols=4,
                               pdf.name="Forestplot.pdf",
                               pdf.param=list(width=6, height=4.5),
                               par.param=list(cex=1.2, cex.main=1.5, cex.sub=1, cex.axis=1)) {
    
    Outcome <- data[, outcome.var]
    
    if (is.null(treatment.var)) {
        Treatment <- rep("Single Arm", nrow(data))
    } else {
        Treatment <- as.character(data[, treatment.var])
    }
    
    Biomarker <- factor(data[, biomarker.var])
    
    if(is.null(covariate.var)) {
        Covariate <- NULL
    } else {
        Covariate <- data[, covariate.var]
    }
    
    if(is.null(strat.var)) {
        Strat.fac <- NULL
    } else {
        Strat.fac <- data[, strat.var]
    }
    
    if (!is.null(biomarker.code)) {
        if (length(biomarker.code) == length(levels(Biomarker))) {
            Biomarker <- factor(Biomarker, labels=biomarker.code)
        } else {
            stop("length of biomarker.code needs to be the same of levels of Biomarker...\n")
        }
    } else {
        biomarker.code <- levels(Biomarker)
    }
    
    Arms <- unique(Treatment)
    
    if (!is.null(placebo.code)) {
        stopifnot(placebo.code %in% Arms)
    }
    
    if (!is.null(active.code)) {
        stopifnot(all(active.code %in% Arms))
    }
    
    thisgroup <- Biomarker == biomarker.code[1]
    res <- NULL
    
    calc.inter.p <- FALSE # set to TRUE if interaction p-value is to be computed
    inter.p <- NULL
    
    if (length(Arms) == 1)  { # single-arm study
        res <- rbind(res, StatSummary(outcome.var=Outcome, subgroup.var=rep(TRUE, length(Treatment)), treatment.var=thisgroup,
                                    placebo.code="TRUE", active.code="FALSE", outcome.type="survival", alpha=alpha,
                                    covariate.var=Covariate,
                                    strat.var=Strat.fac))
    }
    
    if (length(Arms) > 1) { # multi-arm study
        if (!is.null(placebo.code)) { # reference arm specified
            calc.inter.p <- TRUE
            Arms <- c(placebo.code, setdiff(Arms, placebo.code))
            if (!is.null(active.code)) {
                Arms <- c(Arms[1], active.code)
            }
        }
        
        subgroup <- Treatment == Arms[1]
        res <- rbind(res, StatSummary(outcome.var=Outcome, subgroup.var=subgroup, treatment.var=thisgroup,
                                    placebo.code="TRUE", active.code="FALSE", outcome.type="survival", alpha=alpha,
                                    covariate.var=Covariate,
                                    strat.var=Strat.fac))
        
        for (ac in Arms[-1]) {
            subgroup <- Treatment == ac
            res <- rbind(res, StatSummary(outcome.var=Outcome, subgroup.var=subgroup, treatment.var=thisgroup,
                                        placebo.code="TRUE", active.code="FALSE", outcome.type="survival", alpha=alpha,
                                        covariate.var=Covariate,
                                        strat.var=Strat.fac))
            if(calc.inter.p) {
                fit1 <- coxph(Surv(Outcome[, 1], Outcome[, 2]) ~ as.character(Treatment) * Biomarker, subset=as.character(Treatment) %in% c(placebo.code, ac))
                fit2 <- coxph(Surv(Outcome[, 1], Outcome[, 2]) ~ as.character(Treatment) + Biomarker, subset=as.character(Treatment) %in% c(placebo.code, ac))
                L1 <- summary(fit1)[[5]][2]
                n1 <- summary(fit1)[[9]][2]
                L2 <- summary(fit2)[[5]][2]
                n2 <- summary(fit2)[[9]][2]
                stat <- -2*L2 + 2*L1
                inter.p <- c(inter.p, pchisq(stat, df=n1-n2, lower.tail=FALSE))
            }
        }
    }
    
    tabletext <- rbind(c("Arm", "Pt Group", "Event/N", "MST", "HR", "CI", "raw P"),
                       cbind(insert(Arms, ""),
                             rep(biomarker.code, length(Arms)),
                             as.vector(t(cbind(paste(res[, 1], "/", res[, 2], sep=" "),
                                               paste(res[, 4], "/", res[, 5], sep=" ")))),
                             as.vector(t(round(res[, c(3, 6)], 2))),
                             as.vector(t(cbind(rep("", nrow(res)), round(res[, 7], 2)))),
                             as.vector(t(cbind(rep("", nrow(res)), paste(round(res[, 8], 2), round(res[, 9], 2), sep=" - ")))),
                             as.vector(t(cbind(rep("", nrow(res)), round.signif(res[, 10], 2))))))
    
    if (is.null(main)) {
        main.text <- ifelse(is.null(inter.p), "Prognostic Effect of Biomarker", "Prognostic and Predictive Effects of Biomarker")
        main.text <- paste(main.text, " (", biomarker.var, ")", sep="")
    } else {
        main.text <- main
    }
    
    if (is.null(sub)) {
        sub1.text <- ifelse(length(covariate.var) > 0, paste("Results adjusted by ", paste(covariate.var, collapse=" , "), sep=""), "")
        sub2.text <- ifelse(length(strat.var) > 0, paste("Results stratified by ", paste(strat.var, collapse=" , "), sep=""), "")
        if (sub1.text == "" & sub2.text == "") {
            sub.text <- "Unadjusted, unstratified analysis"
        } else {
            sub.text <- paste(sub1.text, sub2.text, sep="\n")
        }
    } else {
        sub.text <- sub
    }
    
    PlotParam(pdf.name, pdf.param, par.param)  
    
    if (is.null(clip)) {
        good1 <- !is.na(res[, 8]) & is.finite(res[, 8]) & res[, 8] != 0
        good2 <- !is.na(res[, 9]) & is.finite(res[, 9])
        xrange <- c(min(round(res[good1, 8], 2)), max(as.numeric(round(res[good2, 9], 2))))
        clip <- exp(c(-max(abs(log(xrange))), max(abs(log(xrange)))))
    }
    
    wid <- max(nchar(sapply(biomarker.code, function(z)strsplit(z, "\n")[[1]][1])))/4
    
    ForestTab(labeltext=tabletext[-c(1), ],
              mean=as.numeric(tabletext[-1, 5]),
              lower=as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][1])),
              upper=as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][2])),
              headings=c(tabletext[1, ], c("Forest plot")),
              cols=cols,
              xlog=TRUE,
              xticks=NULL,
              boxsize=rep(2.5, nrow(tabletext)-1),
              main=main.text,
              sub=sub.text,
              hline=1:nrow(tabletext),
              vline=c(1),
              group.hline=c(2, 4, 6, 8),
              note=ifelse(is.null(inter.p), "", c(paste("* Interaction P = ", paste(round.signif(inter.p, 2), collapse=" ; "), sep=""))),
              clip=clip,
              widths=c(2, wid, 1.5, 1, 1, 2, 1, 5),
              sub.main=c(paste(biomarker.code[2], "better", sep=" "),
                         paste(biomarker.code[1], "better", sep=" ")),
              cex.headings=cex.headings,
              cex.note=cex.note, 
              lwd=2,
              pdf.name=pdf.name,
              pdf.param=pdf.param,
              par.param=par.parm
    )
    
    PlotParam()  
}
