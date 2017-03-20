#' Box/Box-Percentile Plot with additional vertical stripchart, mean(s), and trend-lines.
#' 
#' This function produces boxplots as function \code{boxplot} and box-percentile plots as function \code{bpplot} (package \code{Hmisc}),
#' offering a formula interface, data specification via a vector of variable names, as numeric matrix where all columns will be interpreted as
#' variables or by specifying one or multiple numeric vectors (see examples below). It additionally/optionally 
#' adds vertical jitterplots to each boxplot, adds the number of observations, adds mean-values, and allows to add a trend-line connecting either
#' mean-values (trend="mean") or median-values (trend="median"). Furthermore, this function filters for groups with less than 'threshold'
#' observations. For these (sub-)groups no boxplots will be drawn but a jitterplot will be produced. Labels for each group (X-axis labels) can be customized via 
#' 'xlab.srt', 'xlab.cex', 'xlab.col', 'xlab.font'. Note: When using the formula-interface, one cannot specify formulas as in function 'boxplot'
#' i.e. using something like 'a + b' to get factor crossing, one has to use the correct formula instead as used in e.g. 'lm', i.e. 'a:b'. 
#' 
#' @param ...           a numeric matrix, multiple numeric vectors or a (possibly named) list with multiple numeric vectors to be plotted and additional 
#'                      graphical parameters (identified by names) to be passed on.
#' @param obj           (data.frame, matrix) corresponding to the dataset to be used.
#' @param form          (formula) such as 'y~grp', where 'y' is a numeric vector of data values to be split into
#'                      groups according to the grouping variable 'grp'.
#' @param var           (character) vector specifying the columns in 'obj' to be used for plotting. The order of elements is retained in the boxplot.
#' @param box.type      (character) "b" = for regular boxes, "bp" = for box-percentile boxes 
#' 						(see ? bpplot of package \code{Hmisc}).
#' @param horizontal	(logical) TRUE = the boxes are drawn horizontally, FALSE = vertical boxplot. Note, this has no effect when box.type="bp", since
#' 						the underlying function \code{\link{bpplot}} of the \code{Hmisc} package cannot do it. Also note, that \code{horizontal=TRUE} automatically
#'                      changes the meaning of X- and Y-axis. All arguments referring to Y-axis will then implicitely mean the X-axis and vice versa.
#' @param col           (character) string(s) specifying colors to be used to color the bodies of the box plots. By default they are in the background color.
#' @param Xaxis         (list) passed to function 'axis' which allows to fully specify the X-axis labelling (see ?axis). Default is to determine group labels 
#'                      automatically and plotting group labels below the plot. Set to NULL for omitting group labels.
#'                      For custom group-labels use the list-element "labels". The order of these labels must correspond to the order of group-levels in case the
#'                      formula interface was used, i.e. check the order of sort(unique(obj$grp)). To obtain your desired ordering, specify classification variables as 
#'                      factor-objects using 'factor(dat$trt, levels=c("Level_1", "Level_2", ..., "Level_n"))' where "trt" is the classification variable and Level_1 to Level_n
#'                      represents your desired order of factor levels (which is used in the plot).
#' @param Xaxis2        (list) passed to function 'axis' which allows to fully specify the X-axis labelling above the plot (see ?axis). This is used to specify the number
#'                      of observations per sub-group according to the group-labels. Default is to plot these numbers above the plot. To omit set to NULL.
#'  					Note, if 'horizontal=TRUE' this axis-labelling will appear in the right margin and labels will be oriented perpendicular to the axis.
#'                      Set 'Xaxis2=list(las=0)' to overwrite the default for usual parallel orientation.
#' @param XaxisTab		(list) or NULL, if a (possibly empty) list AND 'form' is specified, 'Xaxis' will be set to NULL and a table will be used a X-axis label
#' 						representing the combination of factor-levels for all factors appearing in 'form' (see examples and see \code{\link{addXlabTable}}).
#'                      If 'horizontal=TRUE' the table will appear in the left margin as any other setting originally intended to apply to an X-axis element.
#' 						Note, there has to be enough space in the bottom margin for adding the table (use "mai" or "mar" in \code{\link{par}}).
#' 						Your may specify 'XaxisTab' as list with two sub-lists "Label" and "Text", which will then be evaluated for rownames of the table ("Label")
#'                      and the text in the cells of the table ("Text") separately (see examples of function \code{\link{addXlabTable}} for details).
#' @param Yaxis         (list) passed to function 'axis' which allows to fully specify the the appearence of the Y-axis (see ?axis).
#' @param Ylabel        (list) passed to function 'mtext' which can be used to fully specify the Y-axis label. 
#' @param Xlabel        (list) passed to function 'mtext' which can be used to fully specify the X-axis label or even group labels (see examples).
#' @param Title         (list) passed to function 'title'.
#' @param Grid          (list) passed to function 'grid'. Set to NULL to omit (default). For adding a grid simply set 'Grid=TRUE' which applies default settings
#'                      for the added grid or fully specify the grid to be added by specifying each argument (see ?addGrid for details).
#' @param transf        (function) name of a function to be used to tranform data before plotting, e.g. log, log10 or user-defined functions, 
#'                      i.e. func=function(x){x[x==0]<-min(x, na.rm=TRUE)/2} and setting 'transf=func'.
#' @param sc.col        (character) string or vector of strings specifying color(s) of plotting symbols in the stripchart. 
#' 						By default "black" with 80\% transparency (alpha=.2). In case of a vector of strings, it is best practise to provide as
#' 						many elements as there are elements in 'obj', which can be used to highlight an additional grouping factor within a single box.
#' 						Note, it is the responsibility of the user to provide 'sc.col', 'sc.pch' or 'sc.cex' in such a way, that the graphical output
#'                      is meaningful. In case of providing less than 'nrow(obj)' elements, 'sc.col' will be replicated to the required length.
#' @param sc.pch        (integer) value or vector of integers specifying plotting smybols for the stripchart. The same rules as for 'sc.col' and 'sc.cex' apply here.
#' @param sc.cex        (numeric) value or vector of numeric values specifying the magnification of plotting symbols in the stripchart. The same rules as for 'sc.col' and 'sc.cex' apply here.
#' @param sc.jitter     (numeric) specifying the amount of jittering in the stripchart.
#' @param trend         (character) "mean" = mean values are connected by a line emphasizing the dynamics (especially for time course data)
#'                                  "median" = median values are used, set to NULL to omit.
#' @param trend.lty     (integer) line type of the trend line.
#' @param trend.lwd     (numeric) line width of the trend line.
#' @param trend.col     (character) color of the trend line.
#' @param threshold     (integer) minimum number of points required for plotting a boxplot, otherwise only the stripchart will be plotted.
#' @param border        (character) string specifying the border color(s) of boxes. This can be a vector with different colors for multiple boxes.
#' @param mean.pch      (integer) plotting symbol for mean-values, which are added to the plot. Use '-1' to prevent plotting of mean-values.
#' @param mean.cex      (numeric) specifying the magnification of mean-value plotting symbols.
#' @param mean.col      (character) specifying the color of mean-value plotting symbols.
#' @param mean.lwd      (integer) specifying the line width of mean-value plotting symbols (cross, plus, asterisk etc.).
#' @param vline         (numeric) value(s) specifying vertical lines added to the plot.
#' @param vl.lwd        (integer) line width of vertical lines.
#' @param vl.lty        (integer) line type of vertical lines.
#' @param vl.col        (character) color of vertical lines.
#' @param Box           (logical) TRUE = a box is plotted surrounding the plot, FALSE = no box.
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}, Vinzent Rolny \email{vinzent.rolny@@roche.com}, christina Rabe \email{rabe.christina@@gene.com}
#' 
#' @examples 
#' 
#' ## generate example dataset
#'   example <- data.frame( y=c(rnorm(30)+10, rnorm(4)+20, rnorm(15)+15, NA), 
#' 							time=c(rep("t2", 30), rep("t4",4), rep("t1", 15), "t3"),
#' 							grp=sample(1:3, 50, TRUE), sex=sample(1:2, 50, TRUE))
#' 
#' ## specify data as named list
#'   BoxPlot(list(a=rnorm(50,,3), b=rnorm(25,1,4), c=rnorm(75,2,1)))
#' 
#' ## same plot, now horizontally plotted
#'   BoxPlot(list(a=rnorm(50,,3), b=rnorm(25,1,4), c=rnorm(75,2,1)), horizontal=TRUE,
#' 			 Xaxis=list(las=2, hadj=2), Xaxis2=list(las=2, hadj=-.25))
#' 
#' ## specify data as numeric matrix
#'   BoxPlot(matrix(rnorm(100), ncol=4, dimnames=list(NULL, LETTERS[1:4])) )
#' 
#' ## specify data as numeric (unnamed) vectors
#'   BoxPlot(rnorm(50,,3), rnorm(25,1,2), rnorm(75,2,1))
#' 
#' ## ... horizontally plotted (no axis-label rotation done here)
#'   BoxPlot(rnorm(50,,3), rnorm(25,1,2), rnorm(75,2,1), horizontal=TRUE)
#' 
#' ## plot values 'y' according to time 'time' (factor levels are automatically 
#' ## ordered as e.g. function \code{sort} does)
#'   BoxPlot(example, y~time, sc.pch=16)
#' 
#' ## now as box-percentile plot
#'   BoxPlot(example, y~time, sc.pch=16, box.type="bp")
#' 
#' ### with custom main title
#'   BoxPlot(example, y~time, sc.pch=16, box.type="bp", Title=list(main="Custom Main Title"))
#' 
#' ## the same plot with trend line (connects either means or medians) and Y-axis label
#'   BoxPlot(example, y~time, trend="median", ylab="Y-Axis Label")
#' 
#' ## use an addition grouping variable to color points in the stripchart
#'   BoxPlot(example, y~time, trend="median", ylab="Y-Axis Label",
#'           sc.col=c("red", "blue", "green")[example$grp] )
#' 
#' ## use yet another grouping factor for plotting symbols in the stripchart
#'   BoxPlot(example, y~time, trend="median", ylab="Y-Axis Label",
#'           sc.col=c("red", "blue", "green")[example$grp],
#' 		     sc.pch=c(5,10)[example$sex] )
#' 
#'   legend("bottomright", fill=c("red", "green", "blue", "white", "white"), 
#'           legend=c("Stage I", "Stage II", "Stage III", "Female", "Male"), 
#'           pch=c(-1, -1, -1, 5, 10), border=NA)
#' 
#' ## generate new dataset with different stucture
#'   example2 <- data.frame(y1=12+rnorm(15), y2=15+rnorm(15), y3=25+rnorm(15))
#' 
#' ## boxplot with trend lines (mean) and grid added to the plot
#'   BoxPlot(example2, var=c("y1", "y2", "y3"), Grid=TRUE, trend="mean")
#' 
#' ## now as box-percentile plot
#'   BoxPlot(example2, var=c("y1", "y2", "y3"), box.type="bp", Grid=TRUE)
#' 
#' ## use yet another way to specify the data
#'    BoxPlot(rnorm(1856, 5), runif(1245, 2,10), exp(rnorm(2311)), sc.col=as.rgb("black", .05), 
#'            box.type="bp", Xaxis=list(labels=c("~N(5,1)", "~Unif(2,10)", "~exp(N(0,1))")), ylim=c(0,15))            
#' 
#' ## specifying data as numeric matrix and using a trend-line connecting the means
#'    mat <- matrix(c(rep(10,45), rep(15,45), rep(35,45))+rnorm(135), ncol=3)
#'    BoxPlot(mat, trend="mean", trend.col="red", Ylabel=list(text="Example Measurements"))
#' 
#' ## multiple grouping factors can be specified via the formula interface which is exemplified using the mtcars dataset,
#' ## of interest is miles per gallon (mgp) depending on number of gears and on the number of cylinders
#'   data(mtcars)
#'   BoxPlot(mtcars, mpg~gear:cyl)  
#' 
#' ## now 'cyl' is explicitly nested within 'gear' which changes the ordering of 
#' ## combined grouping factors (which is identical to using formula 'mpg~cyl:gear').
#'   BoxPlot(mtcars, mpg~cyl %in% gear)     
#' 
#' ## more meaningful group-labels are best specified using custom factor-level names 
#' ## (increase width of the plot window)
#'   dat <- mtcars
#'   dat$cyl <- factor(dat$cyl, levels=c(4,6,8), labels=c("4Cyl", "6Cyl", "8Cyl"))
#'   dat$gear <- factor(dat$gear, levels=c(3,4,5), labels=c("3Gear", "4Gear", "5Gear")) 
#'   BoxPlot(dat, mpg~cyl %in% gear)
#' 
#' ## one can use a table as Xaxis label representing the factor-level 
#' ## combination defining sub-classes 
#'   BoxPlot(dat, mpg~cyl:gear, XaxisTab=list(),mar=c(8,3,5,1))
#' 
#' ## with the original factor levels and as horizontal plot
#'   BoxPlot(mtcars, mpg~cyl:gear, XaxisTab=list(), mar=c(5,8,5,4), 
#' 			 horizontal=TRUE, Ylabel=list(text="Y-axis label now appearing on X-axis"))
#' 
#' # using smaller bottom margin will result in smaller table height
#'   BoxPlot(dat, mpg~cyl:gear, XaxisTab=list(font=2, col="darkblue", cex=1.25), mar=c(5,3,5,1))
#' 
#' # one can use different font-settings for rownames and cells of the table
#'   BoxPlot(dat, mpg~cyl:gear, XaxisTab=list(Label=list(font=2, col="darkblue", cex=1.25), 
#' 			 Text=list(col="red")), mar=c(5,3,5,1))
#' 
#' # use more crossed factors
#'  BoxPlot(dat, mpg~cyl:gear:vs, XaxisTab=list(), mar=c(5,3,5,1))
#'  
#' ### alternatively one can use the 'Xaxis' argument, but the ordering of these labels is not checked
#' ### which is not important for automatically generated group-labels as shown in the previous example
#'   BoxPlot(mtcars, mpg~cyl %in% gear, 
#' 			 Xaxis=list(labels=paste(rep(c("4Cyl", "6Cyl", "8Cyl"),3), 
#' 						c(rep("3Gear",3), rep("4Gear",3), rep("5Gear",3)), sep=".")))
#' 
#' ## the same plot with some fency options
#'   BoxPlot(dat, mpg~cyl %in% gear, Title=list(main="Miles per Gallon by Number of Gears", 
#' 			 col.main="Green", cex.main=2.5), vline=c(3.5, 6.5), vl.lty=2, vl.col="gray", vl.lwd=2,
#'           Xaxis=list(labels=NA, at=1:9, tick=TRUE), col=c(rep("blue", 3), rep("red", 3), rep("green", 3)),
#'           Xaxis2=list(tick=FALSE), Yaxis=list(at=seq(10,34,2)), Grid=list(x=1:9, y=seq(10,34,2)),
#'           Xlabel=list(text=paste(rep(c("4Cyl", "6Cyl", "8Cyl"),3), c(rep("3Gear",3), rep("4Gear",3), rep("5Gear",3))),
#'                       at=1:9, las=2, adj=1, line=0.75, col=c(rep("blue", 3), rep("red", 3), rep("green", 3))), 
#'           mean.col=c(rep("cyan", 3), rep("orange", 3), rep("magenta", 3)), Box=FALSE, trend="mean",mar=c(6,4,5,2) )
#' 
#' ## horizontal fency plot   
#' BoxPlot(mtcars, mpg~cyl %in% gear, Title=list(main="Miles per Gallon by Number of Gears", col.main="#84d52b", cex.main=1.5), 
#' 		vline=c(3.5, 6.5), vl.lty=2, vl.col="gray", vl.lwd=2,
#' 		Xaxis2=list(tick=FALSE, las=2, hadj=-.25), Yaxis=list(at=seq(10,34,2)), Grid=list(x=1:9, y=seq(10,34,2)),
#' 		mean.col=c(rep("cyan", 3), rep("orange", 3), rep("magenta", 3)), Box=FALSE, trend="mean",
#' 		mar=c(3, 7, 4, 4), horizontal=TRUE, sc.pch=c(0, 15)[dat$am+1], sc.col="wheat4",
#' 		XaxisTab=list(Text=list(col=c(rep("cyan", 3), rep("orange", 3), rep("magenta", 3)))) )
#' 
#' legend(x="topright", pch=c(0, 15), legend=c("automatic", "manual"), box.lty=0, col="wheat4")


BoxPlot <- function(..., obj, form=NULL, var=NULL, box.type="b", 
		horizontal=FALSE, col="white", 
		sc.col="#00000040", sc.pch=15L, sc.cex=1, sc.jitter=.1, 
		Xaxis=list(side=1, mgp=c(3, .5, 0), font=1, tick=TRUE), 
		Xaxis2=list(side=3, font=2, mgp=c(3, .5, 0), tick=TRUE),
		XaxisTab=NULL,
		Yaxis=list(side=2, mgp=c(3,1,0)),
		Ylabel=list(text="", side=2, line=2.5, font=1, cex=1),
		Xlabel=list(text="", side=1, line=2.5, font=1, cex=1),
		Title=list(line=2.5), Grid=NULL, transf=NULL,
		trend=NULL, trend.lty=1L, trend.lwd=1, trend.col="blue", threshold=5L, 
		border="black", mean.pch=3L, mean.cex=1.5, mean.col="yellow", mean.lwd=2L,
		vline=NULL, vl.lwd=1L, vl.lty=1L, vl.col="black", Box=TRUE )
{
	box.type <- match.arg(tolower(box.type), c("b", "bp"))
	
	stopifnot(is.logical(horizontal))
	if(box.type == "bp" && horizontal)
		stop("Box-percentile plots cannot be plotted horizontally!")
	
	args <- list(...)   
	
	if(length(args) != 0)                                                       # additional arguments specified
	{
		if(is.null(names(args)) || any(names(args) == ""))                      # split args into named and unnamed arguments 
		{
			if(is.null(names(args)))                                            # all arguments unnamed
			{
				unargs <- args   
				args <- NULL
			}
			else
			{
				unargs <- args[which(names(args) == "")]                        # note which args are unnamed
				args <- args[which(names(args) != "")]
				if("xlab" %in% names(args))
					args <- args[-which(names(args) == "xlab")]
				if("ylab" %in% names(args))
					args <- args[-which(names(args) == "ylab")]
			}            
		}
		else
			unargs <- NULL
		
		if(!"mar" %in% names(args))												# do not overwrite user-specification of mar
		{
			if(horizontal)
				Mar <- c(4.1, 4.1, 4.1, 4.1)
			else
				Mar <- par("mar")
		}	
		else
		{
			Mar <- args[["mar"]]
			args <- args[-which(names(args) == "mar")]
		}
		
		if(horizontal)
			old.par <- par(mar=Mar, xaxs="r", yaxs="i")
		else
			old.par <- par(mar=Mar, xaxs="i", yaxs="r")
		
		cls <- unlist(lapply(unargs, class))
		
		if(any(cls == "data.frame"))                                            # data.frame specified (arg 'obj')
		{
			obj <- unargs[[which(cls == "data.frame")]]
			if(any(cls == "formula") && is.null(form))
				form <- unargs[[which(cls == "formula")]]
		}
		else if(any(cls == "matrix"))                                           # numeric/integer matrix specified
		{
			mat <- unargs[[which(cls == "matrix")]]
			if(class(mat[1,1]) %in% c("integer", "numeric"))                    # numeric matrix --> use columns as groups
			{
				if(is.null(colnames(mat)))
					colnames(mat) <- 1:ncol(mat)
				obj <- as.data.frame(mat)
				var <- colnames(obj)
			}
		}
		else if(any(cls %in% c("integer", "numeric")))                          # multiple numeric/integer vectors specified
		{
			ind <- which(cls %in% c("integer", "numeric"))
			len <- unlist(lapply(unargs[ind], length))
			tmp <- matrix(nrow=max(len), ncol=length(unargs[ind]))
			for(i in 1:ncol(tmp))
			{
				vec <- unargs[[ind[i]]]
				tmp[1:length(vec),i] <- vec
			}
			colnames(tmp) <- 1:length(ind)
			obj <- as.data.frame(tmp)
			var <- colnames(obj)
		}
		else if(any(cls == "list"))
		{
			tmpL <- unargs[[which(cls == "list")]]
			namesL <- names(tmpL)
			if( all( sapply(tmpL, class) %in% c("integer", "numeric")) )
			{
				len <- sapply(tmpL, length)
				tmp <- matrix(nrow=max(len), ncol=length(tmpL))
				for(i in 1:ncol(tmp))
				{
					vec <- tmpL[[i]]
					tmp[1:length(vec), i] <- vec
				}
				if(is.null(namesL))
					colnames(tmp) <- 1:ncol(tmp)
				else
					colnames(tmp) <- namesL
				obj <- as.data.frame(tmp)
				var <- colnames(tmp)
			}
		}
		
		if(any(cls == "formula"))
			form <- unargs[[which(cls == "formula")]]
	}
	else
		old.par <- par()
	
	if(!is.null(form) && class(form)=="formula")
	{
		if(grepl("\\+", as.character(form)[3]))
			stop("One must not use the '+' operator in formulas! Use ':' instead for crossing multiple factors!")
		tmp.var <- apply(attributes(terms(form))$factors, 1, sum)           # get var names + status
		dep.var <- names(tmp.var[which(tmp.var==0)])        
		tmp.var <- names(tmp.var[-which(tmp.var==0)])                       # removes dependent variable (0)
		if(!is.null(transf))             
		{
			if(class(transf) == "function")
				obj[,dep.var] <- do.call(transf, list(obj[,dep.var]))       # apply data transformation/manipulation
			else
				warning("Data transformation could no be applied! 'transf' was not correctly specified!")
		}
		obj[,tmp.var] <- lapply(obj[,tmp.var, drop=FALSE], factor)          # convert grouing variables to factors
		bp <- boxplot(form, data=obj, plot=FALSE)
		fit <- lm(formula(paste(deparse(form), "-1", sep="")), obj)
		Means <- coef(fit)
		if(length(tmp.var) == 1)                                            # just one grouping variable
		{
			names(Means) <- gsub(tmp.var, "", names(Means))                 # names now refer to factor levels
			tmp <- rep(NA, length(levels(obj[,tmp.var])))
			names(tmp) <- levels(obj[,tmp.var])
			tmp[names(Means)] <- Means
			Means <- tmp
		}
		
		if( !is.null(XaxisTab) && is.list(XaxisTab) )
		{
			Xaxis <- NULL
			mat.Xtab <- getMatrix(form, bp$names)	
		}
	}
	else
	{
		if(is.null(var))
		{
			warning("Boxplot cannot be drawn because there is neither formula 'form' nor variable names 'var' provided!")
			return(1)
		}
		stopifnot(all(var %in% colnames(obj)))
		if(!is.null(transf))                                                # apply data-transformation
		{
			if(class(transf) == "function")
				obj[, var] <- lapply(obj[,var, drop=FALSE], transf)         # apply data transformation/manipulation
			else
				warning("Data transformation could no be applied! 'transf' was not correctly specified!") 
		}
		bp <- boxplot(obj[,var], plot=FALSE)
		Means <- apply(obj[,var, drop=FALSE], 2, mean, na.rm=TRUE)
	}
	
	if(is.null(args) || !"ylim" %in% names(args))
	{
		args$ylim <- range( c(c(bp$stats), bp$out), na.rm=TRUE )            # NAs have to be removed for 'ylim'
	}
	
	
	Nbox <- length(bp$n)
	if(Nbox != length(border))                                              # replicate border colors
	{
		border <- rep(border, ceiling(Nbox/length(border)))[1:Nbox]
		col <- rep(col, ceiling(Nbox/length(col)))[1:Nbox] 
	}
	if(any(bp$n < threshold))                                               # prevent boxes from being plotted if n < 'threshold'
	{
		border[bp$n < threshold] <- "white"
		col[bp$n < threshold] <- "white"
	}
	if(box.type == "b")
	{
		ARGS <- list(bp, show.names=FALSE, axes=FALSE, main=NA,             # combine required arguments with additional graphical args from '...'      
				border=border, boxfill=col, outline=FALSE, 
				horizontal=horizontal)
		ARGS <- c(ARGS, args)
		do.call(bxp, ARGS)                                                  # actually plot the boxplot
	}
	else
	{
		ARGS <- list(obj=obj, form=form, var=var, col=col, ylab=NA, 
				labels=NA, add=FALSE, main=NA, line.col=border,
				add.xlab=FALSE)
		ARGS <- c(ARGS, args)
		do.call(BPplot, ARGS)
	}
	if(!is.null(Grid))
	{
		grid.default <- list(x=NULL, y=NULL, col="lightgray", lty=2L, lwd=1L)
		grid.default[names(Grid)] <- Grid
		Grid <- grid.default
		if(horizontal)
		{
			tmp <- Grid$x
			Grid$x <- Grid$y
			Grid$y <- tmp
		}
		do.call("addGrid", Grid)
	}
	
	if(!is.null(Xlabel))
	{
		xlab.default <- list(text="", side=1, line=2.5, font=1, cex=1)              # change specified parameters
		xlab.default[names(Xlabel)] <- Xlabel
		Xlabel <- xlab.default
		if(horizontal)
			Xlabel$side=2
		do.call(mtext, Xlabel)
	}
	if(!is.null(Ylabel))
	{
		ylab.default <- list(text="", side=2, line=2.5, font=1, cex=1)   # change specified parameters
		ylab.default[names(Ylabel)] <- Ylabel		
		Ylabel <- ylab.default
		if(horizontal)
			Ylabel$side <- 1
		do.call(mtext, Ylabel)
	}
	
	if(!is.null(Xaxis))
	{
		xaxis.default <- list(side=1, at=1:length(bp$n), labels=bp$names, mgp=c(3,.5,0), font=1, tick=TRUE)
		xaxis.default[names(Xaxis)] <- Xaxis
		Xaxis <- xaxis.default 
		if(horizontal)
			Xaxis$side=2
		do.call("axis", Xaxis)
	}
	
	if(!is.null(Xaxis2))
	{
		xaxis2.default <- list(side=3, at=1:length(bp$n), labels=paste("N", bp$n, sep="="), 
				mgp=c(3,.5,0), font=2, tick=TRUE, las=ifelse(horizontal, 1, 0))
		xaxis2.default[names(Xaxis2)] <- Xaxis2
		Xaxis2 <- xaxis2.default 
		if(horizontal)
			Xaxis2$side=4
		do.call("axis", Xaxis2)
	}
	
	if(!is.null(XaxisTab))
	{		
		Label <- Text <- NULL
		
		if("Text" %in% names(XaxisTab) && is.list(XaxisTab$Text))
			Text <- XaxisTab$Text
		
		if("Label" %in% names(XaxisTab) && is.list(XaxisTab$Label))
			Label <- XaxisTab$Label
		
		addTableToMargin(mat=mat.Xtab, margin=ifelse(horizontal, "left", "bottom"), 
				Label=Label, Text=Text, reorder=ifelse(horizontal, TRUE, FALSE))
	}
	
	if(!is.null(Yaxis))
	{
		yaxis.default <- list(side=2, mgp=c(3,1,0))
		yaxis.default[names(Yaxis)] <- Yaxis
		Yaxis <- yaxis.default
		if(horizontal)
			Yaxis$side <- 1
		do.call("axis", Yaxis)
	}   
	
	if(!is.null(Title))
	{
		title.default <- list(main=ifelse(box.type=="b", "Box Plot", "Box-Percentile Plot"), line=2.5)
		title.default[names(Title)] <- Title
		Title <- title.default
		do.call("title", Title)
	}
	
	if(Box)
		box()
	
	if (!is.null(form) && class(form) == "formula") 				### stripchart-argument evaluations
		sc.exprs <- "stripchart(form, data=obj.temp"				# formula provided
	else
		sc.exprs <- "stripchart(obj.temp[, var]"					# non-formula
	
	lcol <- length(sc.col)
	lpch <- length(sc.pch)
	lcex <- length(sc.cex)
	
	if(	(lcol != lpch && all( c(lcol, lpch) > 1) ) || 				# any pair has more than one level but is of differnt length --> do not fit together
			(lcol != lcex && all( c(lcol, lcex) > 1) ) || 
			(lpch != lcex && all( c(lpch, lcex) > 1) ) )
	{
		warning("Parameter settings of 'sc.col', 'sc.pch' and 'sc.cex' probably result in missleading graphical output!")
	}
	
	sc.col <- rep(sc.col, ceiling(nrow(obj)/lcol))					# replicate arguments to sufficient length
	sc.pch <- rep(sc.pch, ceiling(nrow(obj)/lpch))
	sc.cex <- rep(sc.cex, ceiling(nrow(obj)/lcex))
	
	sc.combi <- data.frame( col=sc.col, pch=sc.pch, cex=sc.cex,		# determine unique combinations of all 3 sc-arguments
			stringsAsFactors=FALSE)
	
	sc.combi <- unique(sc.combi)
	
	sc.exprs <- paste(	sc.exprs, 
			"col=sc.combi$col[i]", 
			"pch=sc.combi$pch[i]",
			"cex=sc.combi$cex[i]", sep=", ")		
	
	sc.exprs <- paste(	sc.exprs, 
			"method = \"jitter\", vertical = !horizontal", 
			"jitter = sc.jitter, add = TRUE)", sep=", ")
	
	for(i in 1:nrow(sc.combi))
	{
		obj.temp <- obj[sc.col == sc.combi$col[i] &					# sc-grouping according to combination of 3 sc-arguments
						sc.pch == sc.combi$pch[i] &
						sc.cex == sc.combi$cex[i], , drop=FALSE] 				
		eval(parse(text=sc.exprs))									# color or use symbols according to user-specification
	}
	
	if(!is.null(trend))
	{
		if(tolower(trend) == "mean")
		{
			if(horizontal)
				lines(na.omit(Means), which(!is.na(Means)), lty=trend.lty, col=trend.col, lwd=trend.lwd)
			else
				lines(which(!is.na(Means)), na.omit(Means), lty=trend.lty, col=trend.col, lwd=trend.lwd)	
		}
		
		if(tolower(trend) == "median")
		{
			if(horizontal)
				lines(na.omit(bp$stats[3,]), which(!is.na(bp$stats[3,])), lty=trend.lty, col=trend.col, lwd=trend.lwd)
			else
				lines(which(!is.na(bp$stats[3,])), na.omit(bp$stats[3,]), lty=trend.lty, col=trend.col, lwd=trend.lwd)
		}            
	}
	if(!is.null(vline) && class(vline) %in% c("integer", "numeric"))
	{
		if(horizontal)
			abline(h=vline, lwd=vl.lwd, lty=vl.lty, col=vl.col)
		else
			abline(v=vline, lwd=vl.lwd, lty=vl.lty, col=vl.col)
	}
	
	if(horizontal)
		points(Means, 1:length(bp$n), pch=mean.pch, col=mean.col, cex=mean.cex, lwd=mean.lwd)
	else
		points(1:length(bp$n), Means, pch=mean.pch, col=mean.col, cex=mean.cex, lwd=mean.lwd)
	
	suppressWarnings(par(old.par))
	invisible(bp)
}



#' Enhanced Box-Percentile Plots.
#' 
#' Please use function \code{BoxPlot} with \code{box.type="bp"} for box-percentile plots, since this function here
#' was designed as helper function for function \code{BoxPlot}.
#' It uses the R-code of function \code{bpplot} which generates box-percentile plots as implemented in 
#' package \code{Hmisc}. This R-code is changed and augmented to implement an formula-interface and to be able to 
#' add box-percentile plots to an existing plot. It is assumed that \code{form} is a simple formula object, 
#' e.g. \code{y~grp:time} with just simple interactions or even simpler with just a single grouping factor, e.g. \code{y~grp}.
#' 
#' @param obj (data.frame, matrix) corresponding to the dataset to be used
#' @param form (formula) such as \code{y~grp}, where \code{y} is a numeric vector of data values to be split into
#'              groups according to the grouping variable \code{grp}.
#' @param var (character) vector specifying the columns in 'obj' to be used for plotting. The order of elements is retained in the boxplot.
#' @param main (character) string giving the main title of the plot, left out if \code{add=TRUE}
#' @param add (logical) TRUE=add box-percentile plot(s) to an existing plot
#' @param col (character) vector specifying the colors of percentile-boxes, defaults to no color
#' @param labels (character) string(s) for group labels, which are drawn under each boxplot. Note: The order must correspond to the order of group-levels
#'               if the formula interface was used, i.e. check the order of sort(unique(obj$grp)).
#' @param ylab (character) character string specifying the optional label of the Y-axis (vertically centered)
#' @param ylab.line (numeric) specifying the line 'ylab' is put (useful for custom figure margins). 
#' @param ylab.font (integer) specifying the font to be used for the Y-axis label
#' @param swag (numeric) values within ]0,1] specifying the swaging of percentile boxes, for better separating boxes from each other
#' @param line.col (character) color of boundry lines of the percentile boxes
#' @param line.lwd (integer) line width of the boundry lines of the percentile boxes
#' @param line.lty (integer) line type of the boundry lines of the percentile boxes
#' @param add.xlab (logical) TRUE = automically determined group-labels are plotted below each box/percentile-box
#' @param ... additional graphical parameters passed on
#' 
#' @author Andre Schuetzenmeister (using source-code of function \code{bpplot} from package \code{Hmisc})
#' 
#' @seealso \link{BoxPlot}, function \code{bpplot} in package \code{Hmisc}

BPplot <- function (obj, form=NULL, var=NULL, main = "Box-Percentile Plot", add=FALSE, col=NULL, labels=NULL,
		ylab=NULL, ylab.line=2.5, ylab.font=1L, swag=.9, line.col="black", line.lwd=1L,
		line.lty=1L, add.xlab=TRUE, ...) 
{
	stopifnot(swag > 0 && swag <= 1)
	
	bpx <- function (y, offset, swag)                         # helper function for Hmisc-function 'bpplot'
	{
		y <- y[!is.na(y)]
		n <- length(y)
		delta <- 1/(n + 1)
		prob <- seq(delta, 1 - delta, delta)
		quan <- sort(y)
		med <- median(y)
		q1 <- median(y[y < med])
		q3 <- median(y[y > med])
		first.half.p <- prob[quan <= med]
		second.half.p <- 1 - prob[quan > med]
		plotx <- c(first.half.p, second.half.p)*swag
		qx <- approx(quan, plotx, xout = q1)$y
		q1.x <- c(-qx, qx) + offset
		qx <- approx(quan, plotx, xout = q3)$y
		q3.x <- c(-qx, qx) + offset
		q1.y <- c(q1, q1)
		q3.y <- c(q3, q3)
		med.x <- c(-max(first.half.p)*swag, max(first.half.p)*swag) + offset
		med.y <- c(med, med)
		return(list(x1 = (-plotx) + offset, y1 = quan, x2 = plotx + 
								offset, y2 = quan, q1.y = q1.y, q1.x = q1.x, q3.y = q3.y, 
						q3.x = q3.x, med.y = med.y, med.x = med.x))
	}
	
	if(!is.null(form) && class(form)=="formula")
	{
		char <- as.character(form)
		form <- formula(paste(paste(char[c(2,1,3)], collapse=""), "-1", sep="")) 
		mf <- model.frame(form, data=obj, na.action=na.pass)                    # NAs will be removed if not calling model.frame with "na.pass"
		mm <- model.matrix(form, mf)                                            
		mm <- apply(mm, 1:2, function(x) ifelse(x==0, NA, 1))                   # substitute NAs for 0s
		xnames <- boxplot(form, obj, plot=FALSE)$names 
		dat <- apply(mm, 2, function(x) obj[,char[2]] * x)                      # assign observations to classes
		all.x <- list()
		for(i in 1:ncol(dat))
			all.x[[i]] <- na.omit(dat[,i])
	}
	else
	{
		if(is.null(var))
		{
			warning("Boxplot cannot be drawn because there is neither formula 'form' nor variable names 'var' provided!")
			return(1)
		}
		xnames <- var
		all.x <- list()
		for(i in 1:length(var))
			all.x[[i]] <- na.omit(obj[,var[i]])
	}
	
	n <- length(all.x)
	centers <- seq(from = 1, by = 1, length = n)
	ymax <- max(sapply(all.x, function(x){
						if(all(is.na(x)))
							return(NA) 
						else 
							return(max(x, na.rm = TRUE))
					}), na.rm=TRUE)
	ymin <- min(sapply(all.x, function(x){
						if(all(is.na(x)))
							return(NA)
						else
							return(min(x, na.rm = TRUE))
					}), na.rm=TRUE)
	xmax <- max(centers) + 0.5
	xmin <- 0.5
	
	if(!add)
	{
		plot(c(xmin, xmax), c(ymin, ymax), type = "n", main = main, 
				xaxt = "n", xlab=NA, ylab=NA, ...)
		if(!is.null(ylab))
			mtext(ylab, side=2, line=ylab.line, font=ylab.font)
		if(is.null(labels) && add.xlab)
			axis(1, at=1:length(all.x), labels=xnames)
	}
	for (i in 1:n) {
		if(length(is.na(all.x[[i]])) < 2)           # function 'bxp' needs at least 2 non-NA values
			next
		plot.values <- bpx(all.x[[i]], centers[i], swag=swag)
		if(!is.null(col))
		{
			if(length(col) != length(all.x))
				col <- rep(col, ceiling(length(all.x)/length(col)))[1:length(all.x)] 
			if(length(line.col) != length(all.x))
				line.col <- rep(line.col, ceiling(length(all.x)/length(line.col)))[1:length(all.x)]
			
			polygon(c(plot.values$x1, rev(plot.values$x2)), 
					c(plot.values$y1, rev(plot.values$y2)), col=col[i], border=line.col[i])
		}
		lines(plot.values$x1, plot.values$y1, col=line.col[i], lwd=line.lwd, lty=line.lty)
		lines(plot.values$x2, plot.values$y2, col=line.col[i], lwd=line.lwd, lty=line.lty)
		lines(plot.values$q1.x, plot.values$q1.y, col=line.col[i], lwd=line.lwd, lty=line.lty)
		lines(plot.values$q3.x, plot.values$q3.y, col=line.col[i], lwd=line.lwd, lty=line.lty)
		lines(plot.values$med.x, plot.values$med.y, col=line.col[i], lwd=line.lwd, lty=line.lty)
	}    
}









#' Choose an R standard color or get the RGB-code of an arbitrary color.
#' 
#' Function lets the user choose a color via a tcltk widget in case it is called with default value of
#' paramater 'continuous'. Otherwise, it plots a grid of differently colored tiles, which can be used to select as many
#' colors as desired by simply clicking at the corresponding tiles.
#' 
#' @param continuous (logical) TRUE = one or multiple colors can be chosen from the RGB color-space via a tcltk-widget
#'                            FALSE = a grid is drawn where R standard colors can be selected by clicking on colored-tiles
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#' 
#' ## get R standard color
#'  get.colors(FALSE)
#' 
#' ## get RGB-code (hexadecimal) via tcltk-widget
#'  \dontrun{
#'      get.colors()
#'  }

get.colors <- function(continuous=TRUE)
{
	if(continuous)
	{
		tt <- tktoplevel()
		tkwm.title(tt,"Color Selection")
		color <- "blue"
		n <- 1
		canvas <- tkcanvas(tt,width="80",height="25",bg=color)
		ChangeColor <- function()
		{
			color <<- tclvalue(.Tcl(paste("tk_chooseColor",.Tcl.args(initialcolor=color,title="Choose a color"))))
			if (nchar(color)>0)
				tkconfigure(canvas, bg=color)
		}
		AcceptColor <- function()
		{
			tkmessageBox(message=paste("You selected:", color), icon="info", type="ok")
			cat("\n", paste(n,".", paste(rep(" ", 2-nchar(n)), collapse=""), sep=""),"color:", color)
			n <<- n+1
		}
		Exit <- function()
		{
			tkdestroy(tt)
		}
		ChangeColor.button <- tkbutton(tt,text="Choose Color",command=ChangeColor)
		AcceptButton <- tkbutton(tt,text="Accept Color", command=AcceptColor)
		ExitButton <- tkbutton(tt, text="        Exit        ", command=Exit)
		tkgrid(tklabel(tt, text="                                   "))
		tkgrid(canvas,ChangeColor.button, tklabel(tt, text="      "), AcceptButton, tklabel(tt, text="      "))
		tkgrid(tklabel(tt, text="                                   "))   
		tkgrid(tklabel(tt, text=" "), tklabel(tt, text=" "), tklabel(tt, text=" "), ExitButton)   
		tkgrid(tklabel(tt, text="                                   "))   
		tkfocus(tt)   
	}
	else
	{
		plot(0:26,0:26, xlab="", ylab="", main="Choose Colors")
		Colors <- colors()
		rows <- numeric()
		cols <- numeric()
		mat <- matrix(nrow=26,ncol=26)
		for(i in 1:26)                              # rows
		{
			for(j in 1:26)                            # columns
			{
				rect(i-1, j-1, i, j, col=Colors[(i-1)*26+j])
				mat[i,j] <- Colors[(i-1)*26+j]
			}
		}
		while(TRUE)
		{
			loc <- locator(1)
			if(is.null(loc))
				break
			x <- ceiling(loc$x)
			y <- ceiling(loc$y)
			cat("\nx =",x,"y =",y,"color:",mat[x,y])
			flush.console()
		} 
	}    
}



#' Convert color-names or RGB-code to possibly semi-transparent RGB-code.
#' 
#' Function takes the name of a color and converts it into the rgb space. Parameter "alpha" allows
#' to specify the transparency within [0,1], 0 meaning completey transparent and 1 meaning completey
#' opaque. If an RGB-code is provided and alpha != 1, the RGB-code of the transparency adapted color 
#' will be returned.
#' 
#' @param col (character) name of the color to be converted/transformed into RGB-space (code). Only
#'               those colors can be used which are part of the set returned by function colors(). Defaults
#'               to "black".
#' @param alpha (numeric) value specifying the transparency to be used, 0 = completely transparent, 
#'               1 = opaque.
#' 
#' @return RGB-code
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#'      
#' # convert character string representing a color to RGB-code using alpha-channel of .25 (75\% transparent)
#'      as.rgb("red", alpha=.25)
#' 
#' # same thing now using the RGB-code of red (alpha=1, i.e. as.rgb("red"))
#'      as.rgb("#FF0000FF", alpha=.25)

as.rgb <- function(col="black", alpha=1)
{
	if(length(col) > 1 && (length(alpha) == 1 || length(alpha) < length(col)))         # unclear which alpha to use or only one alpha specified
	{
		if(length(alpha) < length(col) && length(alpha) > 1)
			warning("Multiple (but too few) 'alpha' specified! Only use 'alpha[1]' for each color!")
		return(sapply(col, as.rgb, alpha=alpha[1]))
	}
	if(length(col) > 1 && length(col) <= length(alpha))                                 # process each color separately
	{
		res <- character()
		for(i in 1:length(col))
			res <- c(res, as.rgb(col[i], alpha[i]))
		return(res)
	}
	if( col %in% colors() )
		return( rgb(t(col2rgb(col))/255, alpha=alpha) )
	else
	{
		col <- sub("#", "", col)
		R <- as.numeric(paste("0x", substr(col, 1,2), sep=""))
		G <- as.numeric(paste("0x", substr(col, 3,4), sep=""))
		B <- as.numeric(paste("0x", substr(col, 5,6), sep=""))
		return( rgb(R/255, G/255, B/255, alpha=alpha, maxColorValue=1) )
	}        
}




#' Add a Grid to an Existing Plot.
#' 
#' It is possible to use automatically determined grid lines (\code{x=NULL, y=NULL}) or specifying the number 
#' of cells \code{x=3, y=4} as done by \code{grid}. Additionally, x- and y-locations of grid-lines can be specified,
#' e.g. \code{x=1:10, y=seq(0,10,2)}.
#' 
#' @param x (integer, numeric) single integer specifies number of cells, numeric vector specifies vertical grid-lines
#' @param y (integer, numeric) single integer specifies number of cells, numeric vector specifies horizontal grid-lines
#' @param col (character) color of grid-lines
#' @param lwd (integer) line width of grid-lines
#' @param lty (integer) line type of grid-lines
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}

addGrid <- function(x=NULL, y=NULL, col="lightgray", lwd=1L, lty=3L)
{
	if(all(is.null(c(x,y))) || all(length(c(x,y))<2))               # call grid function
		grid(nx=x, ny=y, col=col, lwd=lwd, lty=lty)
	else
	{
		if(length(x) == 0)                                          # NULL
			xticks <- axTicks(side=1)
		else if(length(x) == 1)
		{
			U <- par("usr")
			xticks <- seq.int(U[1L], U[2L], length.out = x + 1)
		}
		else
			xticks <- x
		
		if(length(y) == 0)                                          # NULL
			yticks <- axTicks(side=2)
		else if(length(y) == 1)
		{
			U <- par("usr")
			yticks <- seq.int(U[3L], U[4L], length.out = y + 1)
		}
		else
			yticks <- y
		
		abline(v=xticks, col=col, lwd=lwd, lty=lty)
		abline(h=yticks, col=col, lwd=lwd, lty=lty)
	}
}



#' Create Tabular Environment from Matrix and Add it to Bottom Margin.
#' 
#' This is a helper function intended to add tabular environments as X-axis
#' labels representing specific structure, e.g. formulas in boxplots.
#' Function takes a matrix and constructs a tabular environment from this.
#' 
#' @param mat			(matrix) representing the table which will be added to lower margin;
#'                      rows represent factors, columns represent factor-levels, rownames will
#'                      be re-used as rownames of the table in the plot
#' @param merge			(logial) TRUE = neighboring cells of 'mat' with the same content will
#'                      be merged to a wider cell
#' @param bmargin		(numeric) value representing the height of the (b)ottom margin as proportion
#'                      of the total height of the figure region. Note, that this will determine the height
#'                      of the table in the bottom margin also depending on the amount of space available there.
#' @param Label			(list) specifying all parameters applicable in function 'text', x- and y-values
#'                      will be set automatically for factor-labels (rows of the table)
#' @param Text			(list) specifying all parameters applicable in function 'text', x- and y-values will
#'                      be set automatically for character strings appearing in the cells of the table
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples 
#' 
#' bp  <- BoxPlot(mpg~cyl:gear, mtcars, Xaxis=NULL)
#' mat <- getMatrix(mpg~cyl:gear, bp$names)
#' addXlabTable(mat)
#' 
#' # changing appearance for labels and cell-content
#' 
#' bp  <- BoxPlot(mpg~cyl:gear, mtcars, Xaxis=NULL)
#' addXlabTable(mat, Label=list(font=2, col="red"), Text=list(font=3, col="blue"))

addXlabTable <- function(mat, merge=TRUE, bmargin=.025, Label=list(), Text=list())
{
	stopifnot(is.logical(merge))
	stopifnot(is.matrix(mat))
	stopifnot(is.numeric(bmargin))
	stopifnot(0 <= bmargin && bmargin <= 1)
	
	old.par <- par(xpd=TRUE)								# stop clipping to plotting region
	
	usr <- par("usr")										# user-coordinates of the plotting region ("plt")
	fig <- par("fig")										# figure-coordinates
	plt <- par("plt")										# plotting-region coordinates on the scale of "usr"
	
	dxp <- plt[2] - plt[1]									# delta_x in plt-coordinates
	dyp <- plt[4] - plt[3]									# delta_y in plt-coordinates
	dxu <- usr[2] - usr[1]									# delta_x in usr-coordinates
	dyu <- usr[4] - usr[3]									# delta_y in usr-coordinates
	
	fuc    <- numeric(4)									# figure-region in user coordinates
	fuc[1] <- usr[2] - dxu * plt[2] / dxp  
	fuc[2] <- fuc[1] + (usr[2] - fuc[1]) / plt[2]
	fuc[3] <- usr[4] - dyu * plt[4] / dyp  
	fuc[4] <- fuc[3] + (usr[4] - fuc[3]) / plt[4]
	
	l  <- usr[1]											# left border
	r  <- usr[2]											# right border
	u  <- usr[3]											# upper border
	b  <- fuc[3] + bmargin * diff(fuc[3:4])					# bottom border
	w  <- abs(diff(c(l,r)))									# table width
	h  <- abs(diff(c(u, b)))								# table height
	rh <- h/nrow(mat)										# row height
	cw <- w/ncol(mat)										# cell width
	
	tmp.Text <- list()
	tmp.Text[names(Text)] <- Text
	Text <- tmp.Text
	
	# draw table
	
	tmp.l <- NA
	
	for( i in 1:(nrow(mat)+1) )
	{
		lines(c(l,r), rep(u-(i-1)*rh, 2))							# horizontal lines
		
		for( j in 1:(ncol(mat)+1) )									# vertical lines
		{
			if(i == nrow(mat) + 1)
				next
			
			if(j %in%  c(1, ncol(mat)+1) )							# outer lines
			{
				lines(rep(l+(j-1)*cw, 2), c(u-(i-1)*rh, u-i*rh))
			}
			else													# inner lines
			{
				if(mat[i,j-1] != mat[i,j] || !merge)				# draws lines between cells with different content
				{
					swit  <- TRUE
					lines(rep(l+(j-1)*cw, 2), c(u-(i-1)*rh, u-i*rh))
				}
			}
			
			if(j < ncol(mat) + 1)									# add text to cells
			{
				if( j == 1 )
					tmp.l <- l
				
				if( merge )
				{
					if( j < ncol(mat) )
					{
						if(mat[i, j] != mat[i, j+1])
						{
							tmp.r <- l + j * cw	
							Text$x <- mean(c(tmp.l, tmp.r))
							Text$y <- mean(c(u-(i-1)*rh, u-i*rh))
							Text$labels <- mat[i,j]
							Text$adj <- c(.5, .5)
							do.call("text", Text)
							tmp.l <- tmp.r
						}						
					}
					else
					{
						tmp.r <- l + j * cw	
						
						Text$x <- mean(c(tmp.l, tmp.r))
						Text$y <- mean(c(u-(i-1)*rh, u-i*rh))
						Text$labels <- mat[i,j]
						Text$adj <- c(.5, .5)
						do.call("text", Text)
					}
				}
				else
				{
					Text$x <- l+(j-1)*cw+cw/2
					Text$y <- mean(c(u-(i-1)*rh, u-i*rh))
					Text$labels <- mat[i,j]
					Text$adj <- c(.5, .5)
					
					do.call("text", Text)
				}				
			}
			
		}
	}
	
	# add rownames
	tmp.Label <- list(x = l - .01 * (fuc[2]-fuc[1]),
			y = seq(u-rh/2, b+rh/2, by=ifelse(u < 0, -rh,rh)),
			labels=rownames(mat),
			adj=c(1, .5))
	
	tmp.Label[names(Label)] <- Label
	Label <- tmp.Label	
	do.call("text", Label)
	
	par(old.par)
}



#' Create Tabular Environment from Matrix and Add it to Margin.
#' 
#' This is a generalization of function \code{\link{addXlabTable}}, intended to be a helper 
#' function adding tables to plots instead of usual axis annotations.
#' These tables could represent specific structures, e.g. formulas in boxplots.
#' This function takes a matrix and constructs a tabular environment from this. For
#' all options of argument 'margin' the table will always be ordered from inside to outside,
#' e.g. in boxplots factor levels of the nested factor will appear near to the plot and the
#' outer factor will appear more distant (see examples).
#' 
#' @param mat			(matrix) representing the table which will be added to margin;
#'                      rows represent factors, columns represent factor-levels, rownames will
#'                      be re-used as rownames of the table in the plot
#' @param margin		(character) string specifying the margin to which the table should be added,
#'                      partial matching is supported
#' @param merge			(logial) TRUE = neighboring cells of 'mat' with the same content will
#'                      be merged to a wider cell. If provided as vector of logicals, each elements will be
#'                      applied to rows of 'mat' separately, allowing to merge some variables and leaving others untouched.
#' @param propMargin	(numeric) value representing the height (bottom, top) or width (left, right) of the margin as proportion
#'                      of the total height of the figure region. Note, that this will determine the height
#'                      of the table in the bottom margin also depending on the amount of space available there.
#' @param Label			(list) specifying all parameters applicable in function 'text', x- and y-values
#'                      will be set automatically for factor-labels (rows of the table)
#' @param Text			(list) specifying all parameters applicable in function 'text', x- and y-values will
#'                      be set automatically for character strings appearing in the cells of the table
#' @param reorder		(logical) TRUE = 'mat' will be reorder to match the way horizontal boxplots are drawn,
#'                      i.e. the column order will be inverted, FALSE = 'mat' will be used in the given order
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @seealso \code{\link{addXlabTable}}
#' 
#' @examples 
#' 
#' # these examples should work when run by the user
#' 
#' \dontrun{
#' old.par <- par(mar=c(8,4,4,2), yaxs="r")
#' bp  <- boxplot(mpg~cyl:gear, mtcars)
#' bxp(bp, axes=FALSE)
#' box()
#' axis(2)
#' mat <- getMatrix(mpg~cyl:gear, bp$names)
#'
#' # default is table below the plot (margin="bottom") 
#' addTableToMargin(mat)
#' par(old.par)
#'
#' # place table in left margin
#' old.par <- par(mar=c(4,8,4,3), xaxs="r")
#' bxp(boxplot(mpg~cyl:gear, mtcars, horizontal=TRUE ), axes=FALSE, 
#' 	   horizontal=TRUE)
#' box()
#' axis(1)
#' addTableToMargin(mat, margin="left", reorder=TRUE)
#' par(old.par)
#' 
#' # place table in upper margin
#' old.par <- par(mar=c(4,3,8,2), yaxs="r")
#' bxp(boxplot(mpg~cyl:gear, mtcars), axes=FALSE)
#' box()
#' axis(2)
#' addTableToMargin(mat, margin="top") 
#' par(old.par)
#'
#' # place table in right margin
#' old.par <- par(mar=c(4,3,3,8))
#' bxp(boxplot(mpg~cyl:gear, mtcars, horizontal=TRUE), axes=FALSE, 
#' 	   horizontal=TRUE)
#' box()
#' axis(1)
#' addTableToMargin(mat, margin="right", reorder=TRUE) 
#' par(old.par)
#' 
#' # changing appearance for labels and cell-content
#' # Note: table is added as is, without adapting to the way the boxplot is drawn
#' old.par <- par(mar=c(4,3,3,8))
#' bxp(boxplot(mpg~cyl:gear, mtcars, horizontal=TRUE), axes=FALSE, horizontal=TRUE)
#' box()
#' axis(1)
#' addTableToMargin(mat, margin="right", Label=list(font=2, col="red"), 
#'					Text=list(font=3, col="blue")) 
#' par(old.par)
#' }


addTableToMargin <- function(mat, margin=c("bottom", "left", "top", "right"),
		merge=TRUE, propMargin=.025, Label=list(), Text=list(),
		reorder=FALSE)
{
	stopifnot(is.logical(merge))
	stopifnot(is.matrix(mat))
	stopifnot(is.numeric(propMargin))
	stopifnot(0 <= propMargin && propMargin <= 1)
	
	if(margin %in% c("left", "right") && reorder)
	{
		tmp.mat <- matrix(ncol=ncol(mat), nrow=nrow(mat))	#reorder matrix to be consistent with horizontal
		for(i in ncol(mat):1)
			tmp.mat[,(ncol(mat)+1)-i] <- mat[,i]
		rownames(tmp.mat) <- rownames(mat)
		mat <- tmp.mat
	}
	
	if(length(merge) < nrow(mat))
		merge <- rep(merge, ceiling(nrow(mat) / length(merge)))		# replicate if necessary
	
	margin <- match.arg(margin)
	
	old.par <- par(xpd=TRUE)								# stop clipping to plotting region
	
	usr <- par("usr")										# user-coordinates of the plotting region ("plt")
	fig <- par("fig")										# figure-coordinates
	plt <- par("plt")										# plotting-region coordinates on the scale of "usr"
	
	dxp <- plt[2] - plt[1]									# delta_x in plt-coordinates
	dyp <- plt[4] - plt[3]									# delta_y in plt-coordinates
	dxu <- usr[2] - usr[1]									# delta_x in usr-coordinates
	dyu <- usr[4] - usr[3]									# delta_y in usr-coordinates
	
	fuc    <- numeric(4)									# figure-region in user coordinates
	fuc[1] <- usr[2] - dxu * plt[2] / dxp  					# x1, x2, y1, y2
	fuc[2] <- fuc[1] + (usr[2] - fuc[1]) / plt[2]
	fuc[3] <- usr[4] - dyu * plt[4] / dyp  
	fuc[4] <- fuc[3] + (usr[4] - fuc[3]) / plt[4]
	
	if(margin %in% c("top", "bottom"))
	{
		l  <- usr[1] 										# left border
		r  <- usr[2]										# right border
		
		u  <- if(margin == "top")
					fuc[4] - propMargin * diff(fuc[3:4])		# upper border above upper figure region y
				else
					usr[3]									# bottom -> lower figure region y
		
		b  <- if(margin == "top")
					usr[4] 									# bottom border
				else
					fuc[3] + propMargin * diff(fuc[3:4])		
	}
	else
	{
		l  <- if(margin == "left")
					fuc[1] + propMargin * diff(fuc[1:2])		# left border near display region
				else
					usr[2]									# right -> right figure region
		
		r  <- if(margin == "left")
					usr[1] 									# right border
				else
					fuc[2] - propMargin * diff(fuc[1:2])									# left -> left figure region
		
		u  <- usr[4]										# upper border
		b  <- usr[3] 										# bottom border
	}
	
	w  <- abs(diff(c(l, r)))								# table width
	h  <- abs(diff(c(u, b)))								# table height
	
	if(margin %in% c("top", "bottom"))
	{
		rh <- h/nrow(mat)									# row height
		cw <- w/ncol(mat)									# cell width
	}
	else
	{
		cw <- w/nrow(mat)									# row height
		rh <- h/ncol(mat)									# cell width
	}
	
	tmp.Text <- list()
	tmp.Text[names(Text)] <- Text
	Text <- tmp.Text
	
	if(length(Text) > 0)
	{
		for(i in 1:length(Text))							# replicate arguments to a length equal to ncol(mat)
		{
			if(length(Text[[i]]) < ncol(mat))
				Text[[i]] <- rep(Text[[i]], ceiling(ncol(mat) / length(Text[[i]])))
		}
	}
	
	TextSpec <- Text										# keep current state of Text as (Spec)ified
	
	# draw table
	
	tmp.l <- NA
	
	for( i in 1:(nrow(mat)+1) )										# over factor-variables
	{
		if(margin %in% c("top", "bottom"))
			lines(c(l,r), rep(u-(i-1)*rh, 2))						# horizontal lines (top, bottom)
		else
			lines(rep(l+(i-1)*cw, 2), c(b,u))						# vertical line (left, right)																	
		
		for( j in 1:(ncol(mat)+1) )									# over factor-levels, vertical (t,b), horizontal (l,r) lines
		{
			if( i == (nrow(mat) + 1) )
				next
			
			if(length(TextSpec) > 0 && j < ncol(mat) + 1)			# anything specified by the user?
			{
				tmpText <- vector("list", length(TextSpec))			# extract j-th element of argument-vectors in 'TextSpec'
				names(tmpText) <- names(TextSpec)
				
				for(k in 1:length(TextSpec))
					tmpText[[k]] <- TextSpec[[k]][j]
				
				Text <- tmpText
			}
			
			if(j %in%  c(1, ncol(mat)+1) )							# outer lines
			{
				if(margin %in% c("top", "bottom"))
					lines(rep(l+(j-1)*cw, 2), c(u-(i-1)*rh, u-i*rh))
				else
					lines(c(r-(i-1)*cw, r-i*cw), rep(b+(j-1)*rh, 2))
			}
			else																		# inner lines
			{
				if(any(mat[i:nrow(mat),j-1] != mat[i:nrow(mat),j]) || !merge[i])		# draws lines between cells with different content
				{
					if(margin == "top")
						lines(rep(l+(j-1)*cw, 2), c(b+(i-1)*rh, b+i*rh))
					else if(margin == "bottom")
						lines(rep(l+(j-1)*cw, 2), c(u-(i-1)*rh, u-i*rh))
					else if(margin == "left")
						lines(c(r-(i-1)*cw, r-i*cw), rep(b+(j-1)*rh, 2))
					else
						lines(c(l+(i-1)*cw, l+i*cw), rep(b+(j-1)*rh, 2))
				}
			}
			
			if(j < ncol(mat) + 1)									# add text to cells
			{
				if( j == 1 )
				{
					if(margin %in% c("top", "bottom"))
						tmp.l <- l
					else
						tmp.u <- u
				}					
				
				if( merge[i] )
				{
					if( j < ncol(mat) )
					{
						if(any(mat[i:nrow(mat), j] != mat[i:nrow(mat), j+1]))		# cell different from neighboring cell
						{
							if(margin %in% c("top", "bottom"))
							{
								tmp.r <- l + j * cw	
								Text$x <- mean(c(tmp.l, tmp.r))
								if(margin == "bottom")
									Text$y <- mean(c(u-(i-1)*rh, u-i*rh))
								else
									Text$y <- mean(c(b+(i-1)*rh, b+i*rh))
								Text$labels <- mat[i,j]
								Text$adj <- c(.5, .5)
							}
							else
							{
								tmp.b <- u - j * rh
								if(margin == "left")
									Text$x <- mean(c(r-(i-1)*cw, r-i*cw))
								else
									Text$x <- mean(c(l+(i-1)*cw, l+i*cw))
								Text$y <- mean(c(tmp.u, tmp.b))
								Text$labels <- mat[i,j]
								Text$adj <- c(.5, .5)
							}						
							
							do.call("text", Text)
							
							if(margin %in% c("top", "bottom"))
								tmp.l <- tmp.r
							else
								tmp.u <- tmp.b
						}						
					}
					else
					{
						if(margin %in% c("top", "bottom"))
						{
							tmp.r <- l + j * cw	
							
							Text$x <- mean(c(tmp.l, tmp.r))
							if(margin == "bottom")
								Text$y <- mean(c(u-(i-1)*rh, u-i*rh))
							else
								Text$y <- mean(c(b+(i-1)*rh, b+i*rh))
							Text$labels <- mat[i,j]
							Text$adj <- c(.5, .5)
						}
						else
						{
							tmp.b <- u - j * rh
							
							if(margin == "left")
								Text$x <- mean(c(r-(i-1)*cw, r-i*cw))
							else
								Text$x <- mean(c(l+(i-1)*cw, l+i*cw))
							Text$y <- mean(c(tmp.u, tmp.b))
							Text$labels <- mat[i,j]
							Text$adj <- c(.5, .5)
						}
						
						do.call("text", Text)
					}
				}
				else
				{
					if(margin %in% c("top", "bottom"))
					{
						Text$x <- l+(j-1)*cw+cw/2
						if(margin == "bottom")
							Text$y <- mean(c(u-(i-1)*rh, u-i*rh))
						else
							Text$y <- mean(c(b+(i-1)*rh, b+i*rh))
						Text$labels <- mat[i,j]
						Text$adj <- c(.5, .5)
					}
					else
					{
						if(margin == "left")
							Text$x <- r-(i-1)*cw-cw/2
						else
							Text$x <- l+(i-1)*cw+cw/2
						Text$y <- mean(c(u-(j-1)*rh, u-j*rh))
						Text$labels <- mat[i,j]
						Text$adj <- c(.5, .5)
					}
					
					do.call("text", Text)
				}				
			}
			
		}
	}
	
	# add rownames
	
	if(margin %in% c("top", "bottom"))
	{
		if(margin == "bottom")
		{
			tmp.Label <- list(	x = l - .01 * (fuc[2]-fuc[1]),
					y = seq(u-rh/2, b+rh/2, by=-rh),
					labels=rownames(mat),
					adj=c(1, .5))
		}
		else
		{
			tmp.Label <- list(	x = l - .01 * (fuc[2]-fuc[1]),
					y = seq(b+rh/2, u-rh/2, by=rh),
					labels=rownames(mat),
					adj=c(1, .5))
		}
		
		tmp.Label[names(Label)] <- Label
		Label <- tmp.Label	
	}
	else
	{
		if(margin == "left")
		{
			tmp.Label <- list(	x = seq(r-cw/2, l+cw/2, by=-cw),								
					y = u + .01 * (fuc[4]-fuc[3]),
					labels=rownames(mat),
					adj=c(.5, 0))
		}
		else
		{
			tmp.Label <- list(	x = seq(l+cw/2, r-cw/2, by=cw),								
					y = u + .01 * (fuc[4]-fuc[3]),
					labels=rownames(mat),
					adj=c(.5, 0))
		}
		tmp.Label[names(Label)] <- Label
		Label <- tmp.Label
	}
	
	do.call("text", Label)
	
	par(old.par)
}




#' Construct Matrix from Formula and Names.
#' 
#' Function is a helper function for adding tables as X-axis labels
#' representing the structure of crossed (nested) factor-levels.
#' 
#' @usage getMatrix(form, labels, split="\\\\.")
#' @param form		(formula) object from which the matrix should be constructed
#' @param labels	(character) the 'names" element of the list returned by e.g.
#'                  \code{\link{BoxPlot}} or \code{\link{boxplot}}
#' @param split		(character) symbol or string specifying the split symbol or
#'                  string separating factor-levels in 'labels'
#' 
#' @author Andre Schuetzenmeister \email{andre.schuetzenmeister@@roche.com}
#' 
#' @examples
#' bp  <- boxplot(mpg~cyl:gear, mtcars, plot=FALSE)
#' mat <- getMatrix(mpg~cyl:gear, bp$names)
#' mat

getMatrix <- function(form, labels, split="\\.")
{
	stopifnot(is.character(labels))
	stopifnot(class(form) == "formula")
	form <- terms(form)
	fac  <- rownames(attr(form, "factors"))
	fac  <- if(attr(form, "response")) fac[-1]
	mat  <- matrix(unlist(strsplit(labels, split)), ncol=length(labels))	
	if(nrow(mat) > length(fac))
		warning("Factor-level combinations include the split-character ", paste("'", split, "'", sep="")," causing this problem!")
	stopifnot(nrow(mat) == length(fac))	
	rownames(mat) <- fac
	return(mat)
}



