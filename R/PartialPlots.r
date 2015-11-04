
#' Partial effects plots for any model type, based on the provided predict function (predFnx).  Partial
#' effects are calculated by specifying the 'type' argument to be either using either (1) type='median': the median 
#' of numeric covariates and most common level of factor covariates; or (2) type='all': the approach in 
#' randomForest::partialPlot where the supplied dataset is used to calculate a mean prediction for any given 
#' variable value. Note if using type='all', the method will run slow for large datasets and large
#' ensembles of trees, but will give a more accurate representation, particularly for tree-based methods and/or
#' lots of dummy variables.
#' 
#' Note: in randomForest::partialPlot, they scale classificiation probabilities to the log scale, and set any prob of 0 
#' equal to .Machine$double.eps prior to logging.  Subjective choice of what to assign to a prob of 0 will effect 
#' the shape of the resulting partial effect plot, so results form randomForest::partialPlot should be interpreted 
#' cautiously for datasets where classification probabilities contain a substantial number of estimated 0 values.  The
#' approach used here is to plot whatever is passed through the predFnx, so if pass probabilities, it'll show the 
#' partial effect on the probability scale directly.      
#' 
#' Here the key is to pass an approriate user-defined predFnx, where the function must return
#' a data frame: a single column for single-value response (e.g., regression), a single
#' column for binary classification, or multiple columns for multiclass classification.  If only
#' certain classes are desired from multiclass classification, just have the predFnx return columns
#' for those predicted probabilities.    
#' 
#' @param mod model object
#' @param predFnx a prediction function with arguments 'function(mod, newdata)' - must return numeric values. 
#' If CIOn=TRUE, column names for returned data.frame should be [colNm]_lower and [colNm]_upper, e.g.,
#' c('pred', 'pred_low', 'pred_high').
#' @param dat the dataset (or sample dataset) to get median values (type='median') or make predictions directly on (type='all')
#' @param colNms names of the predictors to get the values from
#' @param type prediction dataset for each variable value: median=use median/most common value for covariates; 
#' all=use full datasaet for covariates
#' @param colNms names of the predictors to get the values from
#' @param CIOn whether or not to plot confidence bounds, if so assumes that predFnx passes this in 
#' @param totPerPage Total number of figures per page (default=9)
#' @param pdfFile The file path/name to save to.
#' @export
splotPartialEffs <- function(mod, dat, predFnx, colNms, type="all", dWeights=rep(1,dim(dat)[1]), 
		CIOn=FALSE, totPerPage=12, pdfFile=NULL) {
	require(dplyr)
	require(grid)
	require(gridExtra)
	
	dat= data.frame(dat)
	
	
	## Create a long dataset for ggplot
	pdat = NULL
	levs =c()
	
	for (colNm in colNms) {
		
		if (is.factor(dat[,colNm]) | is.character(dat[,colNm])) vals = unique(dat[,colNm])
		else vals = seq(min(dat[,colNm]), max(dat[,colNm]), length.out=min(length(unique(dat[,colNm])),51))
		
		if (type=="all") {
			
			## Need to get predictions for each le
			getPred <- function (val) {
				d=dat
				d[,colNm]=val
				preds = predFnx(mod, d)	# get the predictions
				preds = apply(preds, 2, function(x) { weighted.mean(x, dWeights, na.rm=T)})  # get the weighted mean per column
			}
			
			preds = as.data.frame(t(sapply(vals, getPred)))
			
		} else if (type == "median"){
			## Set up a function to get median value if num, or most common value if fac/char
			medFnx = function(x) {
				if (is.factor(x) | is.character(x)) names(sort(table(x),decreasing=TRUE)[1])
				else median(x, na.rm=T)
			}
			
			## Construct get the newdata dataset
			othDat = dat %>% 
					dplyr::select(-one_of(colNm)) %>% 
					dplyr::summarize_each(funs(medFnx)) %>% 
					data.frame()
			
			newdata = othDat[rep(1, length(vals)),]
			newdata = data.frame(vals, newdata)
			names(newdata)[1]=colNm
			
			## Get the preds
			preds = predFnx(mod, newdata)
			
		} else {
			cat("'type' argument not supported.  Use either 'median' or 'all', or leave blank for default 'all'\n")
			return(NULL)
		}
		
		
		## Remove the columns representing CI/ribbon to plot
		predNms = names(preds)
		predNms = predNms[grep("_lower|_upper", predNms, invert=T)]
		
		
		## Go through all possible predictions and add to the long dataset
		for (predNm in predNms) {
			if (is.factor(dat[,colNm]) | is.character(dat[,colNm])) {
				tdat = data.frame(x=rep(NA, length(vals)), x_fac=vals, fit=preds[,predNm])
			} else {
				tdat = data.frame(x=vals, x_fac=NA, fit=preds[,predNm])
			}
			if (CIOn){
				tdat$lower = preds[,paste(predNm,"_lower",sep="")] 
				tdat$upper = preds[,paste(predNm,"_upper",sep="")]  
			}
			
			if (length(predNms)>1) {
				tdat$var = paste(colNm,predNm,sep=" on ")
				levs=c(levs, paste(colNm,predNm,sep=" on "))
			} else {
				tdat$var = colNm
				levs=c(levs, colNm)
			}
			
			if (is.factor(dat[,colNm]) | is.character(dat[,colNm])) tdat$type = "factor"
			else tdat$type = "numeric"
			
			if (is.null(pdat)) pdat=tdat
			else pdat=rbind(pdat, tdat)
			
			
		}			
		
	}
	
	
	## Get variables to plot per page
	unVars = unique(pdat$var)
	pages = ceiling(length(unVars)/totPerPage)
	groups = sort(rep(1:pages, totPerPage))[1:length(unVars)]
	splits = split(unVars, groups)
	##	splits = split(unVars, sort(rank(unVars) %% ceiling(length(unVars)/totPerPage)))	
	
	require(ggplot2)
	if (is.null(pdfFile)) {
		windows(record=T)
	} else {
		pdf(pdfFile,width=8.5, height=11)	
	}
	
	
	
	makePlot <- function(colVar){
		
		# Get the column name by itself if combined
		colNm = strsplit(colVar, " on ")[[1]][1]
		if (is.factor(dat[,colNm]) | is.character(dat[,colNm]))	{
			
			tdat = pdat %>% dplyr::filter(var==colVar)
			g= ggplot(data = tdat, aes(x=x_fac, y=fit)) 
			
			if (CIOn) g= g + geom_linerange(aes(x = x_fac, ymin = lower, ymax=upper),lwd = 3, alpha=.25)
			
			g = g + geom_point(shape=21, size=6, fill="black") +
					geom_point(shape=21, fill="white", colour="white", size=3) +
					ggtitle(colVar) +
					ylab("Partial Effect") +
					theme_bw() +
					theme(plot.title = element_text(lineheight=.8, size=10, face="bold"),
							axis.title.x =element_blank(),
							axis.title.y =element_text(size=10, face = "bold", colour = "black"),
							axis.text.x= element_text(size=10, angle=45, hjust = 1),
							axis.text.y= element_text(size=10, angle=45, hjust=1))
			

			# Bump the limits by 7.5% so geom_point won't be cut off
			if (!CIOn) {
				val=tdat$fit
				rng=diff(range(val))
				adj=rng*.075
				g = g + expand_limits(y = c(min(val)-sign(min(val))*adj, max(val)+sign(max(val))*adj))
			}
			
			
		} else  {
			
			g= ggplot(data=pdat %>% dplyr::filter(var==colVar), aes(x=x, y=fit)) +
					geom_line() +
					ggtitle(colVar) +
					ylab("Partial Effect") +
					theme_bw() +
					theme(plot.title = element_text(lineheight=.8, size=10, face="bold"),
							axis.title.x =element_blank(),
							axis.title.y =element_text(size=10, face = "bold", colour = "black"),
							axis.text.x= element_text(size=10, angle=45, hjust = 1),
							axis.text.y= element_text(size=10, angle=45, hjust=1))
			
			if (CIOn) g = g + geom_ribbon(aes(ymax=upper, ymin=lower), alpha=.25) 
			
		}
		g
	}
	
	## Make the plots for each page and arrange via grid.arrange
	for (i in 1:length(splits)) {
		splColNms=splits[[i]]
		tempDat = pdat %>% dplyr::filter(var %in% splColNms)
		all.ggplots = lapply(splColNms, makePlot)
		do.call(grid.arrange,c(all.ggplots)) 
	}
	
	if (!is.null(pdfFile)) dev.off()
	
}