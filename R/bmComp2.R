#' Given 2 biomarkers, find the contigency table and efficacy in the insection/unique portion
#'
#' @param data input data; data frame only
#' @param bm1 variable name for biomarker 1
#' @param bm1_pos_level the level of biomarker 1 which is considered positive. A character value will force bm1 into character.
#' @param bm2 variable name for biomarker 2
#' @param bm2_pos_level the level of biomarker 2 which is considered positive
#' @param endpoint a vector of TTE endpoints
#' @param trt.var variable to indicate treatment variable; only 2 arms are allowed
#' @param placebo.code a character value to indicate the reference arm
#' @param active.code a character value to indicate the active arm.
#' @param headerLevel control the heading info for Rmd outoupt, default is 2
#' @param armColors color for each arm
#' @param ...
#'
#' @return the original data frame with addition derived biomarker status variables
#' @export
#'
#' @examples ```{r function,results='asis',warning=F,message=F,fig.width=7,fig.height=7}
#' b1 <- bmComp2(vad,'TC3IC3','TC3 or IC3','BEP263C3','>=50%',c('PFSINV','OS'),trt.var='ARMCD1', placebo.code = 'B', active.code='A')
#'
#' b2 <-  bmComp2(sample.data, 'KRAS.mutant', 'Mutant', 'CD8.ihc','1', 'OS', trt.var='Arm')
#' ```


bmComp2 <- function(data, bm1 = NULL, bm1_pos_level = NULL,
                    bm2 = NULL , bm2_pos_level = NULL,
                    endpoint=NULL, trt.var = NULL,
                    placebo.code=NULL,active.code=NULL,
                    headerLevel = 2,
                    armColors = c('black','red'),
                    ...) {

  # originally from  https://github.roche.com/kime30/imp110_IA_10SEP2018/blob/master/code/bmComp2.R
  ## format event variable label
  for (e in endpoint){
      stopifnot(e %in% colnames(data))

      event_var <-  paste(e,'EVENT', sep='.')
      if(event_var %in% colnames(data)){
      }else{
        event_pos <- grep(paste(e,'.*event', sep=''), colnames(data), perl=T, ignore.case = T)
        stopifnot(length(event_pos) == 1)
        print(paste('copying', colnames(data)[event_pos],'variable into', event_var))
        data[[event_var]] <- data[, event_pos]

      }
  }

  ## Function variable checks
  stopifnot(exprs = {
    class(data) == "data.frame"
    # Check if biomarkers exist in the data
    any(names(data) == bm1)
    any(names(data) == bm2)
    # Check if the positive level names exist within the biomarkers
    all(bm1_pos_level %in% data[,bm1])
    all(bm2_pos_level %in% data[,bm2])
    # Check if endpoint, trt var exists in the data
   # all(endpoint %in% names(data)) # Multiple endpoint case
    #any(names(data) == endpoint)
    # Checks on endpoint data types
    all(sapply(data[,endpoint],is.numeric))
    #is.numeric(data[,endpoint])
    #all(data[,paste0(endpoint,'.EVENT')] %in% c('0','1'))
    all(sapply(data[,paste0(endpoint,'.EVENT')],function(x){all(x %in% c('0','1'))}))
    any(names(data) == trt.var)
    # Check if the trt var has only 2 levels
    length(names(table(data[,trt.var]))) == 2
  } )
  if (is.numeric(headerLevel)) { headerLevel = round(headerLevel) }
  ## Assign factor levels to the Arms to correctly order display in tables and KM plots
  # As trt.var is checked to have only 2 levels, only one of these needs to be specified
  # Therefore, find the placebo code by any means, and reorder levels based on that

  if(any(is.na(data[,trt.var]))) stop( paste('treatment variable', trt.var,'should not have any missing values'))

  tempCode = names(table(data[,trt.var]))

  if(length(tempCode) != 2) stop ( paste( 'expecting 2 arms in the input data but detecting the following:', paste(tempCode, collapse=',') ))

  if(!( is.null(active.code) | missing(active.code) )) stopifnot(active.code %in% tempCode)
  if(!( is.null(placebo.code) | missing(placebo.code) )) stopifnot(placebo.code %in% tempCode)

  if ( any(tempCode == active.code)) {
    placebo.code = which(tempCode!=active.code)
  } else if (any(tempCode == placebo.code)) {
    placebo.code = which(tempCode==placebo.code)
  } else {
    placebo.code = 1
  }

  data[,trt.var] = factor(data[,trt.var],levels=tempCode[list(1:2,2:1)[[placebo.code]]])

  if( is.character(bm1_pos_level) ) data[[bm1]] <- as.character(data[[bm1]])
  if( is.character(bm2_pos_level) ) data[[bm2]] <- as.character(data[[bm2]])

  # Convert original biomarkers for easy internal use, as well as massage out empty fields
  df = data %>% mutate(thisBM1 = ifelse(!!as.name(bm1)=='',NA,!!as.name(bm1)),
                       thisBM2 = ifelse(!!as.name(bm2)=='',NA,!!as.name(bm2)),
                       # Make new BEP variable to mark subjects with data in both biomarkers
                       thisBEP = ifelse( is.na(thisBM1) | is.na(thisBM2),0,1) ) %>%
    # subset to doubly BEP by bm1 and bm2: exclude records with missing values from either bm1 or bm2,
    filter(thisBEP==1) %>%
    # Assign all variables that match positive levels as positive, and the rest as negative
    mutate(binBM1 = factor(ifelse(thisBM1 %in% bm1_pos_level,paste0(bm1,'+'),paste0(bm1,'-')),
                           levels=c(paste0(bm1,'+'),paste0(bm1,'-'))),
           binBM2 = factor(ifelse(thisBM2 %in% bm2_pos_level,paste0(bm2,'+'),paste0(bm2,'-')),
                           levels=c(paste0(bm2,'+'),paste0(bm2,'-'))) ) %>%
    # Make a new row with all combinations of bm1 and bm2 for tabulation in logranktab
    mutate(combos = factor(paste0(binBM1,',',binBM2),
                           levels=c( paste0(bm1,'+,',bm2,'+'),
                                     paste0(bm1,'+,',bm2,'-'),
                                     paste0(bm1,'-,',bm2,'+'),
                                     paste0(bm1,'-,',bm2,'-')) ) )

  # Secondary variable checks after filtering
  stopifnot(exprs = {
    # Check if positive levels do not occupy all levels
    !all(df[,bm1] %in% bm1_pos_level)
    !all(df[,bm2] %in% bm2_pos_level)

  } )
  # report BEP size
  print(paste0(nrow(df),'/',nrow(data),' subjects have complete records for ',bm1,' AND ',bm2))
  cat('\n\n')
  # report bm positive levels
  print(paste( bm1,'positive is defined as', bm1_pos_level,';',  bm2, 'positive is defined as', bm2_pos_level))
  # print out the 2x2 table (make sure it shows the row variable (bm1) and column variable (bm2) name
  print(kable(table(df$binBM1,df$binBM2,useNA='ifany'),row.names=T,
              caption = paste0(bm1,'(',bm1_pos_level,') vs ',bm2,'(',bm2_pos_level,')')))
  cat('\n')
  # Outer loop for mutlitple endpoint types
  for (thisEndpoint in endpoint) {
    cat('\\newpage')
    cat(paste0(cat(rep('#',headerLevel),sep=''),' ',thisEndpoint,'\n\n'))
    # Logrank and KM plots for each subpopulation in the above table
    for (comparison in levels(df$combos)) {
      cat(paste0(cat(rep('#',headerLevel+1),sep=''),' ',comparison,'\n\n'))
      temp = df %>% filter(combos == comparison)
      if (nrow(temp)== 0) {
        print('There are no subjects in this category')
      } else {
        #par(mfrow=c(1,2))
        print(kable(LogRankTab( data = temp, tte = thisEndpoint, cens = paste0(thisEndpoint,'.EVENT'),
                                var=trt.var),caption=paste0(thisEndpoint,' in ',comparison)))
        cat('\n')
        print(PlotKM(temp,tte=thisEndpoint,cens=paste0(thisEndpoint,'.EVENT'),trt=trt.var,
                     #main=paste0(endNames[i],' for T22c3_3grp ',t22c3levels[j]),
                     col=armColors,return.data = F,
                     main=paste0(thisEndpoint,' in ',comparison)) )
        #par.param=list(mar=c(6,5,3,2)),cex.nrisk=.6))
        cat('\\newpage')
      }
      cat('\n\n')
    }
  }
  #print(kable())
  return(df)
}
