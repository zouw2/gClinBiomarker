#' Forest plot that examines biomarker effects within treatment arms
#' 
#' This function creates a forest plot along with table with summary statistics to examine the biomarker effects within treatment arms. 
#' 
#' @author Alexey Pronin \email{pronin.alexey@gene.com}, Ning Leng \email{leng.ning@gene.com}, and previous team members (see DESCRIPTION)
#' 
#' @param data input data frame. Rows are patients and columns are variables (e.g. demographics variables, time to event variables, 
#' biomarker variables, treatment indicator, etc.). One patient per row. 
#' @param outcome.type type of the outcome variable. Default is \code{c("survival", "binary")}
#' @param outcome.var name of the outcome varible. For time to event outcome, both names of the 'time to event' and 'censorship' variables need to be specified. 
#' @param treatment.var name of the treatment variable
#' @param biomarker.var name of the biomarker variable
#' @param covariate.var a vector specifying the covariate variables to be adjusted in the model. Default is set to NULL, meaning no adjustment.
#' @param strat.var name of the stratification variables. Default is set to NULL, meaning no stratification.
#' @param placebo.code name of the control arm of the treatment variable 
#' @param active.code of the treatment/experimental arm of the treatment variable
#' @param biomarker.code levels of the biomarker variable
#' @param alpha type I error rate. Default is 0.05.
#' @param main main title of the forest plot. Default is "Association of biomarker effect within treatment arms".
#' @param sub footnote under the forest plot. Default is NULL.
#' @param clip range of the x-axis of the forest plot. Default is NULL.
#' @param cex.headings amount of magnification of headings of the forest plot relative to cex. Default is 1.1.
#' @param cex.note amount of magnification of the note. Default is 1.
#' @param cols Color of the 'effect size' displayed in the forest plot.
#' @param pdf.name name of output pdf file. If it's NULL, the plots will be displayed but not saved as pdf. Default is "Forestplot.pdf".
#' @param pdf.param a list of parameters that define pdf graphics device. See \code{\link{pdf}}. Default is \code{list(width=6, height=4.5)}. 
#' @param par.param a list of parameters that define graphcial parameters. See \code{\link{par}}. Default is \code{list(mar=c(4,4,3,2))}.
#' 
#' @export

ForestPlotBmkrEffects <- function(data,
                                  outcome.type=c("survival", "binary"),
                                  outcome.var, #c(OS,OS.CNSR)
                                  treatment.var, #ARM
                                  biomarker.var, #KRAS...
                                  covariate.var=NULL, #Sex
                                  strat.var=NULL, #Age
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
    
    outcome.type <- match.arg(outcome.type, c("survival", "binary"))
    
    switch(outcome.type,
           survival=ForestPlotSurvival(data=data,
                                       outcome.var=outcome.var,
                                       treatment.var=treatment.var,
                                       biomarker.var=biomarker.var,
                                       covariate.var=covariate.var,
                                       strat.var=strat.var,
                                       placebo.code=placebo.code,
                                       active.code=active.code,
                                       biomarker.code=biomarker.code,
                                       alpha=alpha,
                                       main=main,sub=sub,
                                       clip=clip,
                                       cex.headings=cex.headings,
                                       cex.note=cex.note,
                                       cols=cols,
                                       pdf.name=pdf.name,
                                       pdf.param=pdf.param,
                                       par.param=par.param
           ),
           binary=ForestPlotBinary(data=data,
                                   outcome.var=outcome.var,
                                   treatment.var=treatment.var,
                                   biomarker.var=biomarker.var,
                                   covariate.var=covariate.var,
                                   strat.var=strat.var,
                                   placebo.code=placebo.code,
                                   active.code=active.code,
                                   biomarker.code=biomarker.code,
                                   alpha=alpha,
                                   main=main,
                                   sub=sub,
                                   clip=clip,
                                   cex.headings=cex.headings,
                                   cex.note=cex.note,
                                   cols=cols,
                                   pdf.name=pdf.name,
                                   pdf.param=pdf.param,
                                   par.param=par.param
           )
    )
}
