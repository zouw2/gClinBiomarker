
BoxPlot <- function (..., obj, form = NULL, var = NULL, box.type = "b", 
          horizontal = FALSE, col = "white", sc.col = "#00000040", 
          sc.pch = 16L, sc.cex = 1, sc.jitter = 0.1, Xaxis = list(side = 1, 
                                                                  mgp = c(3, 0.5, 0), font = 1, tick = TRUE), Xaxis2 = list(side = 3, 
                                                                                                                            font = 2, mgp = c(3, 0.5, 0), tick = TRUE), XaxisTab = NULL, 
          Yaxis = list(side = 2, mgp = c(3, 1, 0)), Ylabel = list(text = "", 
                                                                  side = 2, line = 2.5, font = 1, cex = 1), Xlabel = list(text = "", 
                                                                                                                          side = 1, line = 2.5, font = 1, cex = 1), Title = list(line = 2.5), 
          Grid = NULL, transf = NULL, trend = NULL, trend.lty = 1L, 
          trend.lwd = 1, trend.col = "blue", threshold = 5L, border = "black", 
          mean.pch = 3L, mean.cex = 1.5, mean.col = "yellow", mean.lwd = 2L, 
          vline = NULL, vl.lwd = 1L, vl.lty = 1L, vl.col = "black", 
          Box = TRUE) 
{
  box.type <- match.arg(tolower(box.type), c("b", "bp"))
  stopifnot(is.logical(horizontal))
  if (box.type == "bp" && horizontal) 
    stop("Box-percentile plots cannot be plotted horizontally!")
  args <- list(...)
  if (length(args) != 0) {
    if (is.null(names(args)) || any(names(args) == "")) {
      if (is.null(names(args))) {
        unargs <- args
        args <- NULL
      }
      else {
        unargs <- args[which(names(args) == "")]
        args <- args[which(names(args) != "")]
        if ("xlab" %in% names(args)) 
          args <- args[-which(names(args) == "xlab")]
        if ("ylab" %in% names(args)) 
          args <- args[-which(names(args) == "ylab")]
      }
    }
    else unargs <- NULL
    if (!"mar" %in% names(args)) {
      if (horizontal) 
        Mar <- c(4.1, 4.1, 4.1, 4.1)
      else Mar <- par("mar")
    }
    else {
      Mar <- args[["mar"]]
      args <- args[-which(names(args) == "mar")]
    }
    if (horizontal) 
      old.par <- par(mar = Mar, xaxs = "r", yaxs = "i")
    else old.par <- par(mar = Mar, xaxs = "i", yaxs = "r")
    cls <- unlist(lapply(unargs, class))
    if (any(cls == "data.frame")) {
      obj <- unargs[[which(cls == "data.frame")]]
      if (any(cls == "formula") && is.null(form)) 
        form <- unargs[[which(cls == "formula")]]
    }
    else if (any(cls == "matrix")) {
      mat <- unargs[[which(cls == "matrix")]]
      if (class(mat[1, 1]) %in% c("integer", "numeric")) {
        if (is.null(colnames(mat))) 
          colnames(mat) <- 1:ncol(mat)
        obj <- as.data.frame(mat)
        var <- colnames(obj)
      }
    }
    else if (any(cls %in% c("integer", "numeric"))) {
      ind <- which(cls %in% c("integer", "numeric"))
      len <- unlist(lapply(unargs[ind], length))
      tmp <- matrix(nrow = max(len), ncol = length(unargs[ind]))
      for (i in 1:ncol(tmp)) {
        vec <- unargs[[ind[i]]]
        tmp[1:length(vec), i] <- vec
      }
      colnames(tmp) <- 1:length(ind)
      obj <- as.data.frame(tmp)
      var <- colnames(obj)
    }
    else if (any(cls == "list")) {
      tmpL <- unargs[[which(cls == "list")]]
      namesL <- names(tmpL)
      if (all(sapply(tmpL, class) %in% c("integer", "numeric"))) {
        len <- sapply(tmpL, length)
        tmp <- matrix(nrow = max(len), ncol = length(tmpL))
        for (i in 1:ncol(tmp)) {
          vec <- tmpL[[i]]
          tmp[1:length(vec), i] <- vec
        }
        if (is.null(namesL)) 
          colnames(tmp) <- 1:ncol(tmp)
        else colnames(tmp) <- namesL
        obj <- as.data.frame(tmp)
        var <- colnames(tmp)
      }
    }
    if (any(cls == "formula")) 
      form <- unargs[[which(cls == "formula")]]
  }
  else old.par <- par()
  if (!is.null(form) && class(form) == "formula") {
    if (grepl("\\+", as.character(form)[3])) 
      stop("One must not use the '+' operator in formulas! Use ':' instead for crossing multiple factors!")
    tmp.var <- apply(attributes(terms(form))$factors, 1, 
                     sum)
    dep.var <- names(tmp.var[which(tmp.var == 0)])
    tmp.var <- names(tmp.var[-which(tmp.var == 0)])
    if (!is.null(transf)) {
      if (class(transf) == "function") 
        obj[, dep.var] <- do.call(transf, list(obj[, 
                                                   dep.var]))
      else warning("Data transformation could no be applied! 'transf' was not correctly specified!")
    }
    obj[, tmp.var] <- lapply(obj[, tmp.var, drop = FALSE], 
                             factor)
    bp <- boxplot(form, data = obj, plot = FALSE)
    fit <- lm(formula(paste(deparse(form), "-1", sep = "")), 
              obj)
    Means <- coef(fit)
    if (length(tmp.var) == 1) {
      names(Means) <- gsub(tmp.var, "", names(Means))
      tmp <- rep(NA, length(levels(obj[, tmp.var])))
      names(tmp) <- levels(obj[, tmp.var])
      tmp[names(Means)] <- Means
      Means <- tmp
    }
    if (!is.null(XaxisTab) && is.list(XaxisTab)) {
      Xaxis <- NULL
      mat.Xtab <- getMatrix(form, bp$names)
    }
  }
  else {
    if (is.null(var)) {
      warning("Boxplot cannot be drawn because there is neither formula 'form' nor variable names 'var' provided!")
      return(1)
    }
    stopifnot(all(var %in% colnames(obj)))
    if (!is.null(transf)) {
      if (class(transf) == "function") 
        obj[, var] <- lapply(obj[, var, drop = FALSE], 
                             transf)
      else warning("Data transformation could no be applied! 'transf' was not correctly specified!")
    }
    bp <- boxplot(obj[, var], plot = FALSE)
    Means <- apply(obj[, var, drop = FALSE], 2, mean, na.rm = TRUE)
  }
  if (is.null(args) || !"ylim" %in% names(args)) {
    args$ylim <- range(c(c(bp$stats), bp$out), na.rm = TRUE)
  }
  Nbox <- length(bp$n)
  if (Nbox != length(border)) {
    border <- rep(border, ceiling(Nbox/length(border)))[1:Nbox]
    col <- rep(col, ceiling(Nbox/length(col)))[1:Nbox]
  }
  if (any(bp$n < threshold)) {
    border[bp$n < threshold] <- "white"
    col[bp$n < threshold] <- "white"
  }
  if (box.type == "b") {
    ARGS <- list(bp, show.names = FALSE, axes = FALSE, main = NA, 
                 border = border, boxfill = col, outline = FALSE, 
                 horizontal = horizontal)
    ARGS <- c(ARGS, args)
    do.call(bxp, ARGS)
  }
  else {
    ARGS <- list(obj = obj, form = form, var = var, col = col, 
                 ylab = NA, labels = NA, add = FALSE, main = NA, line.col = border, 
                 add.xlab = FALSE)
    ARGS <- c(ARGS, args)
    do.call(BPplot, ARGS)
  }
  if (!is.null(Grid)) {
    grid.default <- list(x = NULL, y = NULL, col = "lightgray", 
                         lty = 2L, lwd = 1L)
    grid.default[names(Grid)] <- Grid
    Grid <- grid.default
    if (horizontal) {
      tmp <- Grid$x
      Grid$x <- Grid$y
      Grid$y <- tmp
    }
    do.call("addGrid", Grid)
  }
  if (!is.null(Xlabel)) {
    xlab.default <- list(text = "", side = 1, line = 2.5, 
                         font = 1, cex = 1)
    xlab.default[names(Xlabel)] <- Xlabel
    Xlabel <- xlab.default
    if (horizontal) 
      Xlabel$side = 2
    do.call(mtext, Xlabel)
  }
  if (!is.null(Ylabel)) {
    ylab.default <- list(text = "", side = 2, line = 2.5, 
                         font = 1, cex = 1)
    ylab.default[names(Ylabel)] <- Ylabel
    Ylabel <- ylab.default
    if (horizontal) 
      Ylabel$side <- 1
    do.call(mtext, Ylabel)
  }
  if (!is.null(Xaxis)) {
    xaxis.default <- list(side = 1, at = 1:length(bp$n), 
                          labels = bp$names, mgp = c(3, 0.5, 0), font = 1, 
                          tick = TRUE)
    xaxis.default[names(Xaxis)] <- Xaxis
    Xaxis <- xaxis.default
    if (horizontal) 
      Xaxis$side = 2
    do.call("axis", Xaxis)
  }
  if (!is.null(Xaxis2)) {
    xaxis2.default <- list(side = 3, at = 1:length(bp$n), 
                           labels = paste("N", bp$n, sep = "="), mgp = c(3, 
                                                                         0.5, 0), font = 2, tick = TRUE, las = ifelse(horizontal, 
                                                                                                                      1, 0))
    xaxis2.default[names(Xaxis2)] <- Xaxis2
    Xaxis2 <- xaxis2.default
    if (horizontal) 
      Xaxis2$side = 4
    do.call("axis", Xaxis2)
  }
  if (!is.null(XaxisTab)) {
    Label <- Text <- NULL
    if ("Text" %in% names(XaxisTab) && is.list(XaxisTab$Text)) 
      Text <- XaxisTab$Text
    if ("Label" %in% names(XaxisTab) && is.list(XaxisTab$Label)) 
      Label <- XaxisTab$Label
    addTableToMargin(mat = mat.Xtab, margin = ifelse(horizontal, 
                                                     "left", "bottom"), Label = Label, Text = Text, reorder = ifelse(horizontal, 
                                                                                                                     TRUE, FALSE))
  }
  if (!is.null(Yaxis)) {
    yaxis.default <- list(side = 2, mgp = c(3, 1, 0))
    yaxis.default[names(Yaxis)] <- Yaxis
    Yaxis <- yaxis.default
    if (horizontal) 
      Yaxis$side <- 1
    do.call("axis", Yaxis)
  }
  if (!is.null(Title)) {
    title.default <- list(main = ifelse(box.type == "b", 
                                        "Box Plot", "Box-Percentile Plot"), line = 2.5)
    title.default[names(Title)] <- Title
    Title <- title.default
    do.call("title", Title)
  }
  if (Box) 
    box()
  if (!is.null(form) && class(form) == "formula") 
    sc.exprs <- "stripchart(form, data=obj.temp"
  else sc.exprs <- "stripchart(obj.temp[, var]"
  lcol <- length(sc.col)
  lpch <- length(sc.pch)
  lcex <- length(sc.cex)
  if ((lcol != lpch && all(c(lcol, lpch) > 1)) || (lcol != 
                                                   lcex && all(c(lcol, lcex) > 1)) || (lpch != lcex && all(c(lpch, 
                                                                                                             lcex) > 1))) {
    warning("Parameter settings of 'sc.col', 'sc.pch' and 'sc.cex' probably result in missleading graphical output!")
  }
  sc.col <- rep(sc.col, ceiling(nrow(obj)/lcol))
  sc.pch <- rep(sc.pch, ceiling(nrow(obj)/lpch))
  sc.cex <- rep(sc.cex, ceiling(nrow(obj)/lcex))
  sc.combi <- data.frame(col = sc.col, pch = sc.pch, cex = sc.cex, 
                         stringsAsFactors = FALSE)
  sc.combi <- unique(sc.combi)
  sc.exprs <- paste(sc.exprs, "col=sc.combi$col[i]", "pch=sc.combi$pch[i]", 
                    "cex=sc.combi$cex[i]", sep = ", ")
  sc.exprs <- paste(sc.exprs, "method = \"jitter\", vertical = !horizontal", 
                    "jitter = sc.jitter, add = TRUE)", sep = ", ")
  for (i in 1:nrow(sc.combi)) {
    obj.temp <- obj[sc.col == sc.combi$col[i] & sc.pch == 
                      sc.combi$pch[i] & sc.cex == sc.combi$cex[i], , drop = FALSE]
    eval(parse(text = sc.exprs))
  }
  if (!is.null(trend)) {
    if (tolower(trend) == "mean") {
      if (horizontal) 
        lines(na.omit(Means), which(!is.na(Means)), lty = trend.lty, 
              col = trend.col, lwd = trend.lwd)
      else lines(which(!is.na(Means)), na.omit(Means), 
                 lty = trend.lty, col = trend.col, lwd = trend.lwd)
    }
    if (tolower(trend) == "median") {
      if (horizontal) 
        lines(na.omit(bp$stats[3, ]), which(!is.na(bp$stats[3, 
                                                            ])), lty = trend.lty, col = trend.col, lwd = trend.lwd)
      else lines(which(!is.na(bp$stats[3, ])), na.omit(bp$stats[3, 
                                                                ]), lty = trend.lty, col = trend.col, lwd = trend.lwd)
    }
  }
  if (!is.null(vline) && class(vline) %in% c("integer", "numeric")) {
    if (horizontal) 
      abline(h = vline, lwd = vl.lwd, lty = vl.lty, col = vl.col)
    else abline(v = vline, lwd = vl.lwd, lty = vl.lty, col = vl.col)
  }
  if (horizontal) 
    points(Means, 1:length(bp$n), pch = mean.pch, col = mean.col, 
           cex = mean.cex, lwd = mean.lwd)
  else points(1:length(bp$n), Means, pch = mean.pch, col = mean.col, 
              cex = mean.cex, lwd = mean.lwd)
  suppressWarnings(par(old.par))
  invisible(bp)
}