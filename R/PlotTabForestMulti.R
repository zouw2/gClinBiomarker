#' Generate forest plot and summarization table for 2-arm or within-arm comparison for multiple variables
#'
#' This function creates a forest plot along with table with summary statistics to infer effect of multiple clinical variables, within a single arm
#' or across two treatment arms.  The outcome could be survival, binary or continuous. This function can be used to summarize a group of
#' variables. This function may be used to compare effect of these variables in ITT vs biomarker evaluable population. Or compare effect
#' of these variables in different biomarker subgroups
#'
#' @author  Ning Leng \email{leng.ning@gene.com}, Alexey Pronin \email{pronin.alexey@gene.com}, and previous team members (see DESCRIPTION)
#'
#' @param data input data frame. Rows are patients and columns are variables (e.g. demographics variables, time to event variables,
#' biomarker variables, treatment indicator, etc.). One patient per row.
#' @param outcome.class type of the outcome variable. Default is \code{c("survival", "binary", "continuous")}
#' @param outcome.var name of the outcome varible. If the outcome.class is binary or coutinuous, only one value should be provided.
#' If the outcome.class is survival, two values should be provided - name of the 'time to event' variable and 'censorship' variable
#'  For the censoring variable, 1 indicates event and 0 indicates censoring
#' @param trt name of the treatment variable. If this is NULL, within-arm analysis will be performed
#' @param var name of the biomarker variable. only one variable should be specified.
#' @param var.class class of the variable. valid categories are "numeric", "categorical". If the class is continuous,
#' user needs to specify percentile.cutoff to dichotomize the continuous measure into subgroups
#' @param var.name display name for the biomarker variable
#' @param percentile.cutoff percentile to dichotomize continuous biomarker measure. This could be a vector with multiple elements.
#' Values should be between 0 and 1
#' @param greater whether calculate summary statistics within the subgroup whose biomarker value is greater than or equal to
#' cutoff value. If this is TRUE, in 2-arm study, across-arm HR within biomarker high group will be calculated.
#' In single arm study HR of biomarker high vs low will be calculated.
#' @param less whether calculate summary statistics within the subgroup whose biomarker value is less than the cutoff value.
#' greater and less can both be TRUE. If compare across subgroups (compare.subgroup=TRUE), both greater and less will be set as TRUE
#' @param within.bin whether calculate summary statistics within bin (e.g. > cutoff1 and <= cutoff2). If within.bin is TRUE,
#' greater and less will be set as FALSE.
#' @param compare.bep.itt whether want to generate two groups of results to compare the summary statistics
#' in ITT vs in BEP. If this is TRUE, paramemeter bep should be specified. If this is FALSE,
#' parameters bep, bep.name, itt.name, bep.indicator will be ignored
#' @param compare.subgroup whether want to generate multiple groups of results to compare the summary statistics
#' in each subgroup defined by parameter subgroup
#' @param subgroup The column which defines subgroups. If compare.subgroup is TRUE, the program will generate forest plot of the vars within each subgroup
#' @param bep name of the column which indicates biomarker evaluable population.
#' @param bep.name preferred display name of the biomarker evaluable population.
#' If it is NULL, bep will be used.
#' @param itt.name preferred display name of ITT
#' If it is NULL, "ITT" will be used.
#' @param bep.indicator In the subpopulation column, which value is used
#' to define the biomarker evaluable population.
#' @param show.itt when performing subgroup comparison (compare.subgroup=TRUE), whether also calculate summary statistics using all patients in itt
#' @param show.bep when performing subgroup comparison (compare.subgroup=TRUE), whether also calculate summary statistics using all patients in BEP (biomarker evaluable population).
#' BEP is defined by variable bep
#' @param covariate a vector specifying the covariate variables to be adjusted in the model. Default is set to NULL, meaning no adjustment.
#' @param strata name of the stratification variables. Default is set to NULL, meaning no stratification.
#' @param placebo.code name of the control arm of the treatment variable
#' @param active.code of the treatment/experimental arm of the treatment variable
#' @param var.code ordered levels of the biomarker variable. This will be ignored for continuous biomarker.
#' If the biomarker is categorical and this is NULL, biomarker subgroups will be ordered by the order from factor() function
#' @param alpha type I error rate. Default is 0.05.
#' @param tabforest Default is FALSE. If it is FALSE, forest plot will be generated using forestplot() function.
#' If it is TRUE, a table will be generated with forest plots incorpriated
#' @param main main title of the forest plot. Default is "Association of biomarker effect within treatment arms".
#' @param sub sub title under the forest plot. Default is NULL.
#' @param clip range of the x-axis of the forest plot. Default is NULL.
#' @param cex.headings amount of magnification of headings of the forest plot relative to cex. Default is 1.1.
#' @param cex.note amount of magnification of the note. Default is 1.
#' @param cols Color of the 'effect size' displayed in the forest plot.
#' @param pdf.name name of output pdf file. If it's NULL, the plots will be displayed but not saved as pdf. Default is "Forestplot.pdf".
#' @param pdf.param a list of parameters that define pdf graphics device. See \code{\link{pdf}}. Default is \code{list(width=6, height=4.5)}.
#' @param par.param a list of parameters that define graphcial parameters. See \code{\link{par}}. Default is \code{list(mar=c(4,4,3,2))}.
#'
#' @export
#' @examples
#' data(input)
#' PlotTabForestBiomarker(data=input,
#'                       outcome.class=c("survival"),
#'                       outcome.var=c("PFS","PFS.event"),
#'                       trt="Arm",
#'                       var="KRAS.mutant",
#'                       var.class="categorical")



PlotTabForestMulti <- function(data,
                                  outcome.class=c("survival", "binary"),
                                  outcome.var, #c(OS,OS.event)
                                  trt=NULL,
                                  var, #KRAS...
                                  var.class=NULL, var.name=NULL,
                                  percentile.cutoff=0.5,
                                  greater=TRUE, less=TRUE,
                                  within.bin=FALSE,compare.bep.itt=TRUE, compare.subgroup=FALSE,
                                  show.itt=FALSE, show.bep=FALSE,
                                  subgroup=NULL,
                                  bep = NULL, bep.name = "BEP", itt.name="ITT",bep.indicator=1,
                                  covariate=NULL, #Sex
                                  strata=NULL, #Age
                                  quantile.type=2,
                                  placebo.code=NULL,
                                  active.code=NULL,
                                  var.code=NULL,
                                  tabforest=FALSE,
                                  alpha=0.05,
                                  main=NULL,
                                  sub=NULL,
                                  clip=NULL,
                                  cex.headings=1.1,
                                  cex.note=1,
                                  cols="darkgreen",
                                  pdf.name=NULL,
                                  pdf.param=list(width=6, height=4.5),
                                  par.param=list(cex=1.2, cex.main=1.5, cex.sub=1, cex.axis=1)) {

  if(compare.bep.itt & compare.subgroup) stop("compare.bep.itt & compare.subgroup cannot both be true!")
  if(compare.bep.itt)if(is.null(bep))stop("compare.bep.itt is TRUE, bep needs to be specified!")

  if(!is.null(subgroup)){
    stopifnot(subgroup%in%colnames(data))
    data[[subgroup]] <- factor(data[[subgroup]])
    groups.level <- levels(data[[subgroup]])
    ngroups <- nlevels(data[[subgroup]])
    if(any(greater, less)) {
      greater<- TRUE
      less <- TRUE
    }
  }
  data.list <- list(ITT=data)
  names(data.list)[1] <- itt.name
  if(compare.bep.itt){
    data.list <- list(ITT=data, BEP=data[which(data[[bep]]%in%bep.indicator),])
    names(data.list)[2] <- bep.name
    }
  if(compare.subgroup){
    data.list <- sapply(groups.level, function(i)data[which(data[[subgroup]]==i),], simplify=F)
    if(show.bep){
      if(is.null(bep)){
        if(any(is.na(data[[subgroup]])))message("show.bep is TRUE but bep is not specified, will define the non NA entries in subgroup column as BEP")
        data$BEPnew <- ifelse(is.na(data[[subgroup]]),0,bep.indicator)
        bep <- "BEPnew"
      }
      data.list <- c(list(BEP=data[which(data[[bep]]%in%bep.indicator),]), data.list)
    }
    if(show.itt) data.list <- c(list(ITT=data), data.list)
    }

  if(length(unique(data[,trt]))==1) {
    trt <- NULL
  }
  if(is.null(trt)) nArms <- 1
  if (!is.null(trt)) { # multi-arm study
    Treatment <- data[,trt]
    Arms <- levels(factor(Treatment))
    if (!is.null(placebo.code)) { # reference arm specified
      if(length(placebo.code)!=1)stop("placebo.code should have length 1")
      if(!placebo.code %in% Arms)stop("placebo.code should be an element in treatment column")
      Arms <- c(placebo.code, setdiff(Arms, placebo.code)) # first placebo then others
      if (!is.null(active.code)) {
        if(length(active.code)!=1)stop("active.code should have length 1")
        if(!all(active.code %in% Arms))stop("active.code should be elements in treatment column")
        if(length(intersect(placebo.code, active.code))>1)
          stop("code cannot be in both active.code and placebo.code!")
        Arms <- c(Arms[1], active.code) #order by specified input
      }
    }
    nArms <- length(Arms)
    if(!nArms%in%c(1,2))stop("only 1 or 2 arms are allowed")
    data[,trt] <- factor(data[,trt],levels=Arms)
    if(is.null(placebo.code))placebo.code <- Arms[1]
    if(is.null(active.code))active.code <- Arms[-1]
  }



  res.list <- sapply(data.list, function(dd){
    sapply(1:length(var), function(v){
    var.class.tmp <- var.class
    if(!is.null(var.class))var.class.tmp <- var.class[v]
    var.name.tmp <- var.name
    if(!is.null(var.name))var.name.tmp <- var.name[v]
    PlotTabForestBiomarker (data=dd,
                                       outcome.class=outcome.class,
                                       outcome.var=outcome.var,
                                       trt=trt,
                                       var=var[v],
                                       var.class=var.class.tmp,
                                       var.name=var.name.tmp,
                                       percentile.cutoff=percentile.cutoff,
                                       numerical.cutoff=NULL,
                                       greater=greater, less=less,
                                       within.bin=within.bin,
                                       show.itt=FALSE, show.bep=FALSE,
                                       bep = NULL, bep.name = "BEP", itt.name="ITT",bep.indicator=1,
                                       covariate=covariate, #Sex
                                       strata=strata, #Age
                                       tabforest=tabforest,
                                       quantile.type=quantile.type,
                                       placebo.code=placebo.code,
                                       active.code=active.code,
                                       alpha=alpha,
                                       only.stat=TRUE)},simplify=FALSE)
  },simplify=FALSE)

  res.list2 <- res.list
  for(i in 1:length(res.list)){
    for(j in 1:length(var)){
      nlev <- (nrow(res.list2[[i]][[j]])-1)/2
      res.list2[[i]][[j]][(1:nlev)*2,1] <- paste(names(res.list)[i], res.list[[i]][[j]][(1:nlev)*2,1])
  }}

  final.tab <- res.list[[1]][[1]][1,]
  hl <- NULL
  ct <- 0
  for(j in 1:length(var)){
    nlev <- (nrow(res.list2[[1]][[j]])-1)/2
    for(k in 1:nlev){
    for(i in 1:length(res.list)){
        ct <- ct+1
        final.tab <- rbind(final.tab,res.list2[[i]][[j]][c(2+(k-1)*2, 1+(k*2)),])
      }
      }
  hl <- c(hl, nrow(final.tab)-1)
    }
  tabletext <- final.tab

  if (is.null(main)) {
    main.text <- ifelse(nArms==1, "Within arm", "Across arm")
    if(compare.bep.itt)main.text <- paste0(main.text, ", Compare ",bep.name, " vs. " , itt.name )
    if(compare.subgroup) main.text <- paste0(main.text, ", Compare ",subgroup," subgroup")
    main.text <- paste0(main.text, "\n", outcome.var[1])

    } else {
    main.text <- main
  }

  if (is.null(sub)) {
    sub1.text <- NULL
    if(length(covariate) > 0)sub1.text <- paste("Results adjusted by ", paste(covariate, collapse=" , "), sep="")
    sub2.text <- NULL
    if(length(strata) > 0)
      sub2.text <-  paste("Results stratified by ", paste(strata, collapse=" , "), sep="")
    if (is.null(sub1.text)  & is.null(sub2.text) ) {
      sub.text <- "Unadjusted, unstratified analysis"
    } else {
      sub.text <- paste(sub1.text, sub2.text, sep=";")
    }
  } else {
    sub.text <- sub
  }

  PlotParam(pdf.name, pdf.param, par.param)

  if (is.null(clip)) {
    r1 <- as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][1]))
    r2 <- as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][2]))
    good1 <- !is.na(r1) & is.finite(r1) & r1!= 0
    good2 <- !is.na(r2) & is.finite(r2)
    xrange <- c(min(round(r1[good1], 2)), max(as.numeric(round(r2[good2], 2))))
    clip <- exp(c(-max(abs(log(xrange))), max(abs(log(xrange)))))
  }

  wid <- max(nchar(sapply(tabletext[,1], function(z)strsplit(z, "\n")[[1]][1])),na.rm=T)/6

  note <- ""
  if(length(cols)==nrow(tabletext)/2) cols <- rep(cols,each=2)

  if(tabforest){
    PlotTabForest(label.text=tabletext[-c(1), ],
                mean=as.numeric(tabletext[-1, 5]),
                lower=as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][1])),
                upper=as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][2])),
                headings=c(tabletext[1, ], c("Forest plot")),
                cols=cols,
                xlog=TRUE,
                xticks=NULL,
                box.size=rep(2.5, nrow(tabletext)-1),
                main=main.text,
                sub=sub.text,
                hline=1:nrow(tabletext),
                vline=c(1),
                group.hline=hl,
                note=note,clip=clip,
                widths=c( wid,2, 1.5, 1, 1, 2, 1, 5),
                sub.main=c(paste(active.code, "better", sep=" "),
                           paste(placebo.code, "better", sep=" ")),
                cex.headings=cex.headings,
                cex.note=cex.note,
                par.param=par.parm
  )
  }

  if(!tabforest){

      hz <- vector("list",1)
      for(i in 1:length(hl)){
        if(hl[i] < nrow(tabletext)){
          hz[[i]] <- gpar(lwd=2, col="#99999999")
          names(hz)[i] <- hl[i]+2
        }
      }

      tabletext2 <- tabletext
      tabletext2[seq(1,nrow(tabletext2),2),6] <- paste0("(",tabletext2[seq(1,nrow(tabletext2),2),6],")")

      forestplot(tabletext2,
                 mean=c(NA,as.numeric(tabletext[-1,5])),
                 lower=c(NA,as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][1]))),
                 upper=c(NA,as.numeric(sapply(tabletext[-1, 6], function(z)strsplit(z, " - ")[[1]][2]))),
                 xlab=paste("<--",active.code, "better [HR] ",placebo.code, "better -->",
                            "\n", note),
                 hrzl_lines=hz,align="l",
                 lwd.xaxis=2, lwd.ci=2,col=fpColors(box=cols, line=cols),
                  xlog=TRUE,
                 title=paste(main.text,"\n",sub.text),
                 #graphwidth=unit(100, 'mm'),
                 colgap=unit(cex.note*4,"mm"),
                 line.margin =unit(cex.note*2,"mm"),
                 txt_gp=fpTxtGp(label=gpar(cex=cex.note),
                                ticks=gpar(cex=cex.note),
                                xlab=gpar(cex = cex.note))
      )

    }
  PlotParam()

  out <- tabletext
}















