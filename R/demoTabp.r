
# a more general purpose function
# varClass and varLevel are used to create ordered factors to trigger correct cmh analysis
demoTabp <- function(ds, var, var.name=NULL, trt, varClass, varLevel, outF=NA, ...){
# ds: input dataset
# var: the baseline variables to compare. do not include treatment variable here. variable names cannot overlap with its values( eg. a variable 'IC1' cannot contain values like 'IC1'. In this case, the variable can be renamed by providing var.name values
# var.name: the variable label for demoTab
# trt: the variable name for the column head, trt variable must be included in varLevel unless trt is a factor variable already
# varClass: a named vector to specify the class for each var. eg varClass=c('age'='numeric','sex'='character', 'IC'='factor'). All variables that are specified as a factor will be converted into a factor before calling demoTab, and will be converted into an ordered factor before making pvalues.
# varLevel: a named vector to specify the factor levels.
    stopifnot(is.data.frame(ds))
    if(missing(var.name) | is.null(var.name)) var.name <- var
    stopifnot(!any(duplicated(var.name)))
    stopifnot(all(c(trt, var) %in% colnames(ds)) && nrow(ds) > 0 )
    stopifnot(class(ds[, trt]) %in% c('factor', 'character'))

    for(i in var){
# allow empty varClass - NL. If original numeric or integer -> numeric; if original character/factor and varLevels not specified -> char; if varLevel specified -> factor
        if(! (i %in% names(varClass))){
            if(! (i %in% names(varLevel))){
                if(class(ds[[i]])%in%c('numeric','integer')) varClass <- c(varClass,'numeric')
                if(class(ds[[i]])%in%c('factor','character')) varClass <- c(varClass,'character')
            }else{
                varClass <- c(varClass, 'factor') # if level is specified it will be factor
            }

            names(varClass)[length(varClass)] <- i
        }
    }


# allow empty varLevel for trt - NL
    if(!trt%in%(names(varLevel))) {
        varLevel <- c(varLevel, list(levels(factor(ds[,trt]))))
        names(varLevel)[length(varLevel)] <- trt
}


    stopifnot( all(names(varClass) %in% var) && all(varClass %in% c('character','factor','numeric')) )

    stopifnot(is.list(varLevel) && all(names(varLevel) %in% c(trt, names(varClass))) && all(varClass[intersect(names(varLevel), names(varClass))]=='factor'  ) && (trt %in% names(varLevel) || is.factor(ds[, trt])))

    for (i in names(varClass)){
        if ( varClass[i] == 'numeric' && !class(ds[, i]) %in% c('numeric','integer')) ds[, i] <- as.numeric(as.character(ds[, i]))
        if ( varClass[i] == 'character' && ! class(ds[, i]) %in% c('factor','character')) ds[, i] <- as.character(ds[, i])
        if ( class(ds[, i]) == 'character') ds[(!(is.na(ds[, i]))) & nchar(ds[, i])==0, i] <- NA
        if ( varClass[i] == 'factor') {
            stopifnot( i %in% names(varLevel) )
            ds[, i] <- factor(as.character(ds[, i]), levels=varLevel[[i]])
        }
    }

#     for (i in setdiff(var, names(varClass))){
#         print(paste(i, 'will be analyzed as', class(ds[, i])))
#         }

    if(class(ds[, trt]) != 'factor') ds[, trt] <- factor(ds[, trt], levels= varLevel[[trt]])

    dt <- demoTab(ds, var = var, trt = trt, var.name=var.name) # demoTab does not handle ordered factor well.

    for (i in names(varClass)){
        if ( varClass[i] == 'factor') {
            stopifnot( i %in% names(varLevel) )
            ds[, i] <- factor(as.character(ds[, i]), levels=varLevel[[i]], ordered=T)
        }
    }

    p1 <- lapply(var, function(x){

        sel1 <- !(is.na(ds[, x]) | is.na(ds[, trt]))

        if(sum(sel1) > 1 && length(unique(droplevels(ds[sel1,trt]))) >=2) {

            if(is.factor(ds[, x])){
                if(is.ordered(ds[, x])){
                    require(coin)

                    return(pvalue(cmh_test(as.formula(paste("`",x,"`",' ~ ',trt, sep='')), data=droplevels(ds[sel1,]),...)))
                }else{

                    return( fisher.test(ds[sel1, x], ds[sel1, trt], ...)$p.value )
                }
            }

            if(class(ds[, x]) == 'character' ) return( fisher.test(ds[sel1, x], ds[sel1, trt], ...)$p.value )

            if(class(ds[, x]) %in% c('integer', 'numeric'))    return( kruskal.test(x=ds[sel1, x], g=ds[sel1, trt], ...)$p.value )
        }
        else return(NA)
    })
    p1 <- unlist(p1)
    names(p1) <- var.name

  total.or.n <- which(gsub('&nbsp;','',rownames(dt), fixed=T) %in% c('Total','N')) # summary statistics always start with a row named 'Total' or 'N'
  possible.var.pos <-   total.or.n - 1 # which lines are variable names
    for ( i in var.name){
        stopifnot(length(which( row.names(dt)[possible.var.pos] == i) )== 1) # make sure var.name is unique in the row.names
        # this may break when variable has a level named the same as one of the variable name and has another level named 'N' or 'Total'. Probably very unlikely
#	stopifnot(length(which( row.names(dt) == i) )== 1) # make sure var.name is unique in the row.names

        }

    dt1 <- cbind(dt , pvalue=rep('',nrow(dt)))
    dt1[names(p1),'pvalue'] <- round.signif(p1,2)
    if(!is.na(outF)) write.csv(dt1, outF)
    return(dt1[-1, ])
}



## interface function that calls the functions above for more than one variable and returns an object that can be saved as csv or printed by xtable
## or directly writes to a file if wanted

demoTab <- function (obj, var, var.name = NULL, trt = NULL, trt.names = NULL,
                     pop = NULL, pop.names = "", transf = NULL, control = list(Class = c(N.total = TRUE,
                                                                                         N.level = TRUE, perc = TRUE, na = FALSE), Numeric = c(Mean = TRUE,
                                                                                                                                               SEM = FALSE, SD = TRUE, Median = TRUE, Min = FALSE, Max = FALSE,
                                                                                                                                               MinMax = TRUE, N = TRUE, Qrt1 = FALSE, Qrt3 = FALSE,
                                                                                                                                               iqr = FALSE, na = FALSE)), Unique = 2, digits = 2, keep.order = FALSE,
                     format = NULL, na.action = "error", latex=FALSE, bold=NULL,
                     save.csv = FALSE, name.csv= NULL, save.tex = FALSE, name.tex=NULL,
                     caption=NULL, align=NULL,sani.function=I,...) {

  require(xtable)
  if(save.tex == TRUE){latex <- TRUE}

  res <- list()
  for(i in 1:length(var)){
    if(class(obj[, var[i]]) == 'character') obj[, var[i]] <- factor(obj[, var[i]])
    res[[i]] <- print.ASummary(summary_as(obj=obj, var=var[i], var.name = var.name[i], trt = trt, trt.names = trt.names,
                                          pop = pop, pop.names = pop.names, transf = transf, control = control,
                                          Unique = Unique, digits = digits, keep.order = keep.order,
                                          format = format, na.action = na.action), print=F, latex=latex, bold=bold)
    # if more than one remove first row (names of pop)
    if(i != 1){
      res[[i]] <- res[[i]][-1,,drop=F]
    }
  }


  #edits made by wei on Oct072018 to indent summary rows

  res <- lapply(res, function(x) {
    r1 <- row.names(x)
    startPos <- 2 + as.integer( r1[1] == '')
    if(length(r1) >= startPos) {
      r1[startPos : length(r1)] <- paste('&nbsp;&nbsp;', r1[startPos : length(r1)], sep='')
      row.names(x) <- r1
    }
    x
  })

  resall <- do.call("rbind", res)


  if(save.csv != FALSE){
    if(is.null(name.csv)){name.csv <- "tmp.csv"}
    write.csv(resall, name.csv)
  }

  if(save.tex != FALSE){
    if(is.null(name.tex)){name.tex <- "tmp.tex"} #YY
    xtab <- xtable(cbind(rownames(resall),resall), caption=caption, align=align)
    print(xtab, include.colnames=TRUE, include.rownames=FALSE, #floating.environment="tabular",
          file=name.tex,
          sanitize.text.function=sani.function, hline.after=c(-1,0,nrow(xtab))) #YY

  }


  resall

}
