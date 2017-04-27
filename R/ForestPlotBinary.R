#' @keywords internal

ForestPlotBinary <- function(data,
                             outcome.var="",
                             treatment.var="",
                             biomarker.var="",
                             covariate.var=NULL,
                             strat.var=NULL,
                             placebo.code=NULL,
                             active.code=NULL,
                             biomarker.code=NULL,
                             alpha=0.05,
                             main="Association of biomarker effect within treatment arms",
                             sub=NULL,
                             clip=NULL,
                             cex.headings=1.1,
                             cex.note=1,
                             cols=4,
                             pdf.name="Forestplot.pdf",
                             pdf.param=list(width=6, height=4.5),
                             par.param=list(cex=1.2, cex.main=1.5, cex.sub=1, cex.axis=1)) {
    
    Outcome <- data[, outcome.var]
    
    if (treatment.var == "") {
        Treatment <- rep("Single Arm", nrow(data))
    } else {
        Treatment <- as.character(data[, treatment.var])
    }
    
    Biomarker <- factor(data[, biomarker.var])
    
    if (is.null(covariate.var)) {
        Covariate <- NULL
    } else {
        Covariate <- data[, covariate.var]
    }
    
    if (is.null(strat.var)) {
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
    
    if (treatment.var == "") {
        stopifnot(is.null(placebo.code) && is.null(active.code))
        placebo.code <- "Single Arm"
        active.code <- NULL
    }
    
    subgroup <- Treatment == placebo.code
    thisgroup <- Biomarker == biomarker.code[1]
    res <- NULL
    res <- rbind(res, c(StatSummary(outcome.var=Outcome, subgroup.var=subgroup, treatment.var=thisgroup,
                                  placebo.code="TRUE", active.code="FALSE", outcome.type="binary", alpha=alpha,
                                  covariate.var=Covariate,
                                  strat.var=Strat.fac),
                        "NRsp.Placebo"=sum(Outcome[subgroup & Biomarker == biomarker.code[1]], na.rm=TRUE),
                        "NRsp.Active"=sum(Outcome[subgroup & Biomarker == biomarker.code[2]], na.rm=TRUE)))
    
    inter.p <- NULL
    
    for (ac in active.code) {
        subgroup <- Treatment == ac
        res <- rbind(res, c(StatSummary(outcome.var=Outcome, subgroup.var=subgroup, treatment.var=thisgroup,
                                      placebo.code="TRUE", active.code="FALSE", outcome.type="binary", alpha=alpha,
                                      covariate.var=Covariate,
                                      strat.var=Strat.fac),
                            "NRsp.Placebo"=sum(Outcome[subgroup&Biomarker == biomarker.code[1]], na.rm=TRUE),
                            "NRsp.Active"=sum(Outcome[subgroup&Biomarker == biomarker.code[2]], na.rm=TRUE)))
        
        fit1 <- glm(Outcome~as.character(Treatment)*Biomarker, subset=as.character(Treatment) %in% c(placebo.code, ac), family=binomial)
        fit2 <- glm(Outcome~as.character(Treatment)+Biomarker, subset=as.character(Treatment) %in% c(placebo.code, ac), family=binomial)
        L1 <- summary(fit1)$deviance
        n1 <- summary(fit1)[[7]]
        L2 <- summary(fit2)$deviance
        n2 <- summary(fit2)[[7]]
        stat <- L2-L1
        inter.p <- c(inter.p, pchisq(stat, df=n2-n1, lower.tail=FALSE))
    }
    
    tabletext <- rbind(c("Arm", "Pt Group", "Resp/N", "Resp Rate", "deltaRR", "CI", "raw P"),
                       cbind(c(placebo.code, "", insert(active.code, "")),
                             rep(biomarker.code, length(c(placebo.code, active.code))),
                             as.vector(t(cbind(paste(res[, 9], "/", res[, 7], sep=" "),
                                               paste(res[, 10], "/", res[, 8], sep=" ")))),
                             as.vector(t(round(res[,c(5, 6)], 2))),
                             as.vector(t(cbind(rep("", nrow(res)), round(res[, 1], 2)))),
                             as.vector(t(cbind(rep("", nrow(res)), paste(round(res[, 2], 2), round(res[, 3], 2), sep=" - ")))),
                             as.vector(t(cbind(rep("", nrow(res)), round.signif(res[, 4], 2))))))
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
        good1 <- !is.na(res[, "Lower"]) & is.finite(res[, "Lower"])
        good2 <- !is.na(res[, "Upper"]) & is.finite(res[, "Upper"])
        xrange <-  c(min(as.numeric(res[good1, "Lower"])), max(as.numeric(res[good2, "Upper"]), na.rm=TRUE))
        clip <- round(c(-max(abs(xrange)), max(abs(xrange))), 2)
    }
    
    wid <- max(nchar(sapply(biomarker.code, function(z)strsplit(z, "\n")[[1]][1])))/4
    
    if(is.null(inter.p)) {
        note <- c()
    } else {
        note <- c(paste("* Interaction P = ", paste(round.signif(inter.p, 2), collapse=" ; "), sep=""))
    }
    
    ForestTab(labeltext=tabletext[-c(1), ],
              mean=as.numeric(tabletext[-1,5]),
              lower=as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][1])),
              upper=as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][2])),
              headings=c(tabletext[1, ], c("Forest plot")), cols=cols,
              xlog=FALSE, xticks=NULL, boxsize=rep(2.5, nrow(tabletext)-1),
              main=main,  sub=sub.text,
              hline=1:nrow(tabletext), vline=c(0),
              group.hline=c(2,4,6,8),
              note=note,
              clip=clip, widths=c(2, wid, 1.5, 1.5, 1.5, 2, 1, 5),
              sub.main=c(paste(biomarker.code[1], "better", sep=" "),
                         paste(biomarker.code[2], "better", sep=" ")),
              cex.headings=cex.headings,
              cex.note=cex.note, 
              lwd=2,
              pdf.name=pdf.name,
              pdf.param=pdf.param,
              par.param=par.param
    )
    
    PlotParam()  
}
