
#' Title generate demographic tables
#'
#' @param ds  input dataset
#' @param var the baseline variables to compare. do not include treatment variable here. #' @param var.name Optional, the variable text to display in the output. variable names supplied to \code{var.name} cannot overlap with its values (eg. a variable 'IC1' cannot contain values like 'IC1') in the output; in this case, use \code{var.name} to display a different text.
#' @param trt the variable name for the column head, trt variable must be included in varLevel unless trt is a factor variable already
#' @param varClass Optional, a named vector to specify the class for each var. eg varClass=c('age'='numeric','sex'='character', 'IC'='factor'). All variables that are specified as a factor will be converted into a factor before calling demoTab, and will be converted into an ordered factor to trigger correct cmh analysis. If \code{varClass} is not specified, the function will guess: if original numeric or integer -> numeric; if original character/factor and varLevels not specified -> char; if \code{varLevel} specified -> factor
#'
#' @param varLevel a named vector to specify the factor levels.
#' @param outF a file path/name to save the results
#' @param ... additional parameters to \code{fisher.test} or \code{cmh_test}
#'
#' @return
#' @export
#'
#' @examples demoTabp(ds = input, var=c('KRAS.mutant','KRAS.exprs'),  trt='Country', varClass=c( ), varLevel=list(  ),simulate.p.value = T)
#'
demoTabp <- function(ds, var, var.name=NULL, trt, varClass, varLevel, outF=NA, ...){

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

summary_as <- function (obj, var, var.name = NULL, trt = NULL, trt.names = NULL,
                        pop = NULL, pop.names = "", transf = NULL,
                        control = list(Class = c(N.total = TRUE,
                                                 N.level = TRUE, perc = TRUE, na = FALSE), Numeric = c(Mean = TRUE,
                                                                                                       SEM = FALSE, SD = TRUE, Median = TRUE, Min = FALSE, Max = FALSE,
                                                                                                       MinMax = TRUE, N = TRUE, Qrt1 = FALSE, Qrt3 = FALSE,
                                                                                                       iqr = FALSE, na = FALSE)), Unique = 2, digits = 2, keep.order = FALSE,
                        format = NULL, na.action = "error")
{
  na.action <- match.arg(na.action, c("na.omit", "error"))
  if (!is.null(trt)) {
    if (any(is.na(obj[, trt]))) {
      if (na.action == "na.omit") {
        obj <- obj[complete.cases(obj[, trt]), ]
        warning("There were observations without 'trt' information! These were omitted (na.action=\"na.omit\")!")
      }
      else stop("There were observations without 'trt' information!")
    }
  }
  if (!is.null(pop)) {
    if (length(pop) == 1 && pop %in% colnames(obj)) {
      if (any(is.na(obj[, pop]))) {
        if (na.action == "na.omit") {
          obj <- obj[complete.cases(obj[, pop]), ]
          warning("There were observations without 'pop' information! These were omitted (na.action=\"na.omit\")!")
        }
        else stop("There were observations without 'pop' information!")
      }
    }
  }
  format.value <- function(val, format) {
    if (is.null(format))
      return(round(val, digits))
    else return(format_num(val, cnum = format["cnum"], dd = format["dd"]))
  }
  if (!is.null(transf)) {
    stopifnot(is.function(transf))
    obj[, var] <- sapply(obj[, var], transf)
  }
  stopifnot(class(obj) == "data.frame")
  stopifnot(var %in% colnames(obj))
  var.name <- ifelse(is.null(var.name), var, var.name)
  if (!is.null(pop)) {
    if (is.character(pop) && pop %in% colnames(obj)) {
      if (length(pop) == 1) {
        cn <- colnames(obj)
        pop.l <- sort(na.omit(unique(as.character(obj[,
                                                      pop]))))
        for (i in 1:length(pop.l)) {
          tmp <- rep(NA, nrow(obj))
          tmp[which(obj[, pop] == pop.l[i])] <- 1
          obj <- cbind(obj, tmp)
        }
        colnames(obj) <- c(cn, paste(pop, pop.l, sep = "."))
        pop <- paste(pop, pop.l, sep = ".")
      }
    }
    else stop("'pop' cannot be found in 'colnames(obj)'!")
  }
  default = list(Class = c(N.total = FALSE, N.level = TRUE,
                           perc = TRUE, na = FALSE), Numeric = c(Mean = TRUE, SD = TRUE,
                                                                 Median = TRUE, Min = FALSE, Max = FALSE, SEM = TRUE,
                                                                 MinMax = TRUE, N = TRUE, Qrt1 = FALSE, Qrt3 = FALSE,
                                                                 iqr = FALSE, na = FALSE))
  for (i in names(control)) {
    for (j in names(control[[i]])) default[[i]][j] <- control[[i]][j]
  }
  control <- default
  if (class(obj[, var]) %in% c("numeric", "integer")) {
    if (length(unique(na.omit(obj[, var]))) <= Unique)
      obj[, var] <- as.factor(obj[, var])
  }
  if (is.null(trt)) {
    obj$trt <- rep(1, nrow(obj))
    trt.lev <- unique(obj$trt)
  }
  else {
    if (keep.order)
      obj$trt <- factor(obj[, trt], levels = unique(obj[,
                                                        trt]))
    else obj$trt <- factor(obj[, trt])
    trt.lev <- levels(obj$trt)
  }
  if (is.null(pop)) {
    obj$pop <- rep(1, nrow(obj))
    pop.names <- ""
    pop <- "pop"
  }
  else {
    if (length(pop) != length(pop.names))
      pop.names <- pop
  }
  res <- matrix(ncol = sum(1, length(trt.lev) * length(pop)),
                nrow = 0)
  if (class(obj[, var]) == "factor")
    Lev <- levels(obj[, var])
  result <- vector("list", length(trt.lev))
  if (is.null(trt.names))
    names(result) <- trt.lev
  else {
    if (all(trt.lev %in% names(trt.names)))
      names(result) <- trt.names[trt.lev]
    else {
      names(result) <- trt.lev
      warning("'trt.names' could not be used because treatment-levels could not all be found in 'names(trt.names)'!")
    }
  }
  res.ind <- 1
  for (i in 1:length(trt.lev)) {
    trt.mat <- obj[which(obj$trt == trt.lev[i]), ]
    for (j in 1:length(pop.names)) {
      pop.mat <- trt.mat[which(!is.na(trt.mat[, pop[j]])),
                         ]
      Nna <- length(which(is.na(pop.mat[, var])))
      Na <- nrow(pop.mat) - Nna
      if (class(pop.mat[, var]) %in% c("factor", "character")) {
        res.pop <- data.frame(1)
        if (control$Class["N.total"])
          res.pop$Total <- Na
        if (control$Class["na"])
          res.pop$"NA's" <- Nna
        if (control$Class["N.level"]) {
          for (k in Lev) {
            res.pop[, k] <- length(which(pop.mat[, var] ==
                                           k))
            if (control$Class["perc"])
              res.pop[, k] <- paste(res.pop[, k], " ",
                                    "(", 100 * round(res.pop[, k]/Na, digits +
                                                       2), "%)", sep = "")
          }
        }
        res.pop <- t(res.pop[, -1])
      }
      else {
        smry <- summary(pop.mat[, var], digits = 16)
        res.pop <- data.frame(1)
        if (control$Numeric["N"])
          res.pop$N <- Na
        if (control$Numeric["Mean"])
          res.pop$Mean <- format.value(ifelse(is.nan(smry["Mean"]),
                                              NA, smry["Mean"]), format)
        if (control$Numeric["SEM"])
          res.pop$SEM <- format.value(sd(pop.mat[, var],
                                         na.rm = TRUE)/sqrt(length(which(!is.na(pop.mat[,
                                                                                        var])))), format)
        if (control$Numeric["SD"])
          res.pop$SD <- format.value(sd(pop.mat[, var],
                                        na.rm = TRUE), format)
        if (control$Numeric["Median"])
          res.pop$Median <- format.value(smry["Median"],
                                         format)
        if (control$Numeric["Min"])
          res.pop$Min <- format.value(smry["Min."], format)
        if (control$Numeric["Max"])
          res.pop$Max <- format.value(smry["Max."], format)
        if (control$Numeric["MinMax"]) {
          if (any(is.na(smry[c("Min.", "Max.")])))
            res.pop$"Min-Max" <- NA
          else res.pop$"Min-Max" <- paste(format.value(smry["Min."],
                                                       format), format.value(smry["Max."], format),
                                          sep = "...")
        }
        if (control$Numeric["Qrt1"])
          res.pop$"1st Qrtl." <- format.value(smry["1st Qu."],
                                              format)
        if (control$Numeric["Qrt3"])
          res.pop$"3rd Qrtl." <- format.value(smry["3rd Qu."],
                                              format)
        if (control$Numeric["iqr"])
          res.pop$IQR <- format.value(smry["3rd Qu."] -
                                        smry["1st Qu."], format)
        if (control$Numeric["na"])
          res.pop$"NA's" <- Nna

        res.pop <- t(res.pop[, -1])
      }
      if (j == 1)
        p.res <- res.pop
      else p.res <- cbind(p.res, res.pop)
    }
    colnames(p.res) <- pop.names
    result[[i]] <- p.res
  }


  class(result) <- "ASummary"
  attr(result, "var.name") <- var.name
  result
}


print.ASummary <- function (x, print = TRUE, Order = NULL, latex = FALSE, bold = NULL,
                            hspace = NULL, na.print = "", ...)
{
  obj <- x
  stopifnot(class(obj) == "ASummary")
  if (!is.null(Order)) {
    stopifnot(all(sort(Order) == (1:length(obj))))
    atts <- attributes(obj)
    tmp <- names(obj)
    obj <- obj[Order]
    atts$names <- atts$names[Order]
    class(obj) <- "ASummary"
    attributes(obj) <- atts
  }
  bold.default <- c(var = TRUE, col = TRUE, stat = TRUE)
  if (!is.null(bold)) {
    bold.default[names(bold)] <- bold
    bold <- bold.default
  }
  mat <- matrix("", nrow = 2 + nrow(obj[[1]]), ncol = length(obj) *
                  ncol(obj[[1]]))
  rownames(mat) <- c("", attr(obj, "var.name"), rownames(obj[[1]]))
  if (!print && latex && !is.null(bold)) {
    if (bold["var"])
      rownames(mat)[2] <- paste("\\textbf{", rownames(mat)[2],
                                "}", sep = "")
    if (!is.null(hspace))
      mat[2, ] <- rep(paste("\\hspace{", hspace, "}", sep = ""),
                      length(obj) * ncol(obj[[1]]))
  }
  mat[1, 1:ncol(mat)] <- rep(colnames(obj[[1]]), length(obj))
  cn <- rep("", ncol(mat))
  obj.names <- names(obj)
  cn[seq(1, ncol(mat) - ncol(obj[[1]]) + 1, ncol(obj[[1]]))] <- names(obj)
  if (!is.null(bold) && bold["col"])
    cn <- paste("\\textbf{", cn, "}", sep = "")
  colnames(mat) <- cn
  for (i in 1:length(obj)) {
    tmp.val <- obj[[i]]
    tmp.val <- sub("\\.\\.\\. ", "\\.\\.\\.", tmp.val)
    tmp.val <- sub("\\.\\.\\. ", "\\.\\.\\.", tmp.val)
    mat[3:nrow(mat), (1 + (i - 1) * ncol(obj[[1]])):(i *
                                                       ncol(obj[[1]]))] <- tmp.val
  }
  rep.perc <- function(x) {
    tmp <- unlist(strsplit(x, ""))
    if ("%" %in% tmp) {
      tmp[which(tmp == "%")] <- "\\%"
      x <- paste(tmp, collapse = "")
    }
    return(x)
  }
  if (!print && latex)
    mat <- apply(mat, 1:2, rep.perc)
  ul <- c(mat[3:nrow(mat), ])
  max.len <- max(sapply(ul, nchar))
  fun <- function(x, max.len) {
    extra <- max.len - nchar(x) - 1
    if (extra > 0)
      x <- gsub("^", paste(rep(" ", 1 + extra), collapse = ""),
                x)
    return(x)
  }
  for (i in 3:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      mat[i, j] <- fun(mat[i, j], max.len)
    }
  }
  if (!print && latex && !is.null(bold)) {
    if (bold["stat"])
      mat <- cbind(c(rownames(mat)[1:2], paste("\\textbf{",
                                               rownames(mat)[-c(1:2)], "}", sep = "")), mat)
    else mat <- cbind(rownames(mat), mat)
    rownames(mat) <- NULL
  }
  colnames(mat)[which(colnames(mat) == "")] <- " "
  if (print)
    print(noquote(mat), na.print = na.print)
  invisible(mat)
}

