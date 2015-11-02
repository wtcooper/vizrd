
#' Plots a heatmap representation fo the data.  
#' 
#' @param dat dataset
#' @param colNms a vector of column names you want to include (all factor or character columns removed)
#' @param numObs The top n observations to include in the plot
#' @export
splotDataHeatmap <- function(dat, colNms, numObs=75) {
	require(dplyr)
	require(tidyr)
	require(ggplot2)	
	require(scales)
	
	if (!("data.table" %in% class(dat))) dat = dat %>% data.table()
	
	## get just the good columns
	dat = dat %>% select(one_of(colNms))
	
	## remove character and factor columns
	temp = data.frame(head(dat))
	gdNms = names(temp[, !sapply(temp, function(x) is.character(x) | is.factor(x) ), drop=FALSE])
	dat = dat %>% select(one_of(gdNms))
	
	## get just the top columns
	dat = head(dat, numObs)
	
	## get the length of the table
	tblLen = length(dat %>% select(one_of(names(dat)[1])) %>% unlist())
	
	## rescale data to 0-1
	for (colNm in gdNms){
		idx=which(names(dat) == colNm)
		set(dat, i=NULL, j=idx, value=rescale(dat[[idx]]))
	}
	
	
	## set the observation # as the y-axis	
	dat$obs = factor(tblLen:1, levels=as.character(tblLen:1))
	
	## convert from wide to long format for ggplot	
	plotDatLong = dat %>% gather(Variable, Value, -obs) %>% filter(!is.na(Value))
	
	
	## do the plot	
	base_size <- 12
	g <- ggplot(plotDatLong, aes(x=Variable, y=obs, fill=Value)) + 
			geom_tile(colour = "white") + 
			scale_fill_gradient(low = "azure", high = "steelblue") +
			theme_gray(base_size = base_size) + 
			labs(x = "",y = "") + 
			scale_x_discrete(expand = c(0, 0)) +
			scale_y_discrete(expand = c(0, 0)) + 
			theme(
					axis.ticks = element_blank(), 
					axis.text.x = element_text(size = base_size , angle = 45, hjust = 1,vjust=1, colour = "grey50"))
	print(g)
}


#' Plots raw data points (y-axis = values, x-axis = row #).  
#' 
#' @param dat dataset
#' @param colNms the column(s) you want to plot
#' @param numObs the top n observations to include in the plot
#' @param byCol by group column name by which to facet the plots
#' @export
splotDataPoints <- function(dat, colNms, numObs=NULL, byCol=NULL) {
	require(tidyr)
	require(dplyr)
	require(ggplot2)	
	require(data.table)
	
	
	if (length(colNms)>1 & !is.null(byCol))	{
		warning("Can't facet multiple variables and a by group at the same time....removing by group\n")
		byCol=NULL
	}
	
	if (!("data.table" %in% class(dat))) dat = dat %>% data.table()
	
	## get just the good columns
	dat = dat %>% select(one_of(colNms, byCol))
	
	## remove character and factor columns
	temp = data.frame(head(dat))
	gdNms = names(temp[, !sapply(temp, function(x) is.character(x) | is.factor(x) ), drop=FALSE])
	dat = dat %>% select(one_of(gdNms, byCol))
	
	
	## get just the top columns
	if (!is.null(numObs)) dat = head(dat, numObs)
	
	## get the length of the table
	tblLen = length(dat %>% select(one_of(names(dat)[1])) %>% unlist())
	
	
	if (!is.null(byCol)) {
		setnames(dat, byCol, "facet")
		
		## set the observation # as the y-axis	
		dat[, obs := sequence(.N), by=facet]
		
		## convert from wide to long format for ggplot	
		plotDatLong = dat %>% gather(Variable, Value, -obs, -facet) %>% filter(!is.na(Value))
	} else {
		dat$obs = 1:tblLen
		plotDatLong = dat %>% gather(Variable, Value, -obs) %>% filter(!is.na(Value))
	}
	
	
	## do the std deviations
	plotDatLong = plotDatLong %>% group_by(Variable) %>% 
			mutate(StdDeviations=abs(scale(Value)[,1])) %>%
			mutate(StdDeviations = ifelse(is.nan(StdDeviations) | is.na(StdDeviations),0,StdDeviations)) %>% 
			mutate(StdDeviations = ifelse(StdDeviations>5,5,StdDeviations))
	
	
	
	g = ggplot(data=plotDatLong, aes(x=obs, y=Value, fill=StdDeviations)) +
			geom_point(shape=21, size=4, colour=NA, alpha=.75) +
			scale_fill_gradient(low = "darkblue", high = "orangered") +
			xlab("Observation") +
			theme_bw() +
			theme(strip.text= element_text(face="bold"), axis.title = element_text(face="bold"))
	
	## set up the facets - if by group
	if (is.null(byCol)) {
		g = g + facet_wrap(~Variable,scales ="free_y" )  
	} else {			  
		g = g + facet_wrap(~facet, scales = "free_x")
		g = g + ylab(colNms) 
	}
	
	
	print(g)
}




#' Plots a histogram for a column.  
#' 
#' @param dat dataset
#' @param colNm the column you want to plot
#' @param numObs The top n observations to include in the plot
#' @param binSize The bin size (default=1)
#' @param minVal The minimum value to include
#' @param maxVal The maximum value to include
#' @param byCol by group column name by which to facet the plots
#' @export
splotDataHist <- function(dat, colNm, numObs=NULL, binSize=1, minVal=NULL, maxVal=NULL, byCol=NULL) {
	require(dplyr)
	require(ggplot2)	
	
	if (!("data.table" %in% class(dat))) dat = dat %>% data.table()
	
	## get just the good columns
	dat = dat %>% select(one_of(c(colNm, byCol)))
	
	## get just the top columns
	if (!is.null(numObs)) dat = head(dat, numObs)
	
	## get the length of the table
	tblLen = length(dat %>% select(one_of(names(dat)[1])) %>% unlist())
	
	## set the observation # as the y-axis	
	dat$obs = 1:tblLen
	
	## do the std deviations
	setnames(dat, colNm, "Value")
	
	if (!is.null(minVal)) dat = dat %>% filter(Value>=minVal)
	if (!is.null(maxVal)) dat = dat %>% filter(Value<=maxVal)
	
	
	g = ggplot(dat, aes(x=Value)) + 
			geom_histogram(aes(y=..density..), binwidth=binSize, color="black", fill="black", alpha=.7) +  
			geom_density(alpha=.2, fill="#FF6666")  +
			ylab("Density") +
			xlab(colNm)+
			theme_bw() +
			theme(axis.title = element_text(face="bold"), strip.text=element_text(face="bold"))
	
	if (!is.null(byCol)) g = g + facet_wrap(as.formula(paste("~", byCol)))
	
	print(g)
}




#' Plots the distribution for a given column(s) or for a column by group.  
#' 
#' @param dat dataset
#' @param colNms the column(s) you want to plot
#' @param numObs the top n observations to include in the plot
#' @param byCol by group column name by which to facet the plots
#' @export
splotDataDist <- function(dat, colNms, byCol=NULL, baseCol="gray65") {
	require(tidyr)
	require(dplyr)
	require(ggplot2)	
	require(data.table)
	
	
	if (!("data.table" %in% class(dat))) dat = dat %>% data.table()
	
	## get just the good columns
	dat = dat %>% select(one_of(colNms, byCol))
	
	## remove character and factor columns
	temp = data.frame(head(dat))
	gdNms = names(temp[, !sapply(temp, function(x) is.character(x) | is.factor(x) ), drop=FALSE])
	dat = dat %>% select(one_of(gdNms, byCol))
	
	
	if (!is.null(byCol)) {

		setnames(dat, byCol, "byCol")
		plotDatLong = dat %>% gather(Variable, Value, -byCol) %>% filter(!is.na(Value))
		
		g = ggplot(data=plotDatLong, aes(x=byCol, y=Value)) +
				geom_violin(fill=baseCol, colour=NA) + 
				geom_boxplot(width=.1, size=1) + 
				theme_bw() +
				facet_wrap(~Variable, scales = "free") + 
				theme(axis.text.x = element_text(face="bold", angle = 45, hjust = 1,vjust=1), 
						axis.title.x=element_blank(), 
						axis.title.y = element_blank(),
						strip.text = element_text(face="bold")
						)
		
		
		
	} else {
		plotDatLong = dat %>% gather(Variable, Value) %>% filter(!is.na(Value))
		
		g = ggplot(data=plotDatLong, aes(x=1, y=Value)) +
				geom_violin(fill=baseCol, colour=NA) + 
				geom_boxplot(width=.1, size=1) + 
				theme_bw() +
				facet_wrap(~Variable, scales = "free") + 
				theme(axis.text.x = element_blank(), 
						axis.title.x=element_blank(), 
						axis.title.y = element_blank(),
						axis.ticks.x=element_blank(),
						strip.text = element_text(face="bold"))
		
	}
	

	
	print(g)
}




#' Exports the raw data point plots (splotDataPoints()) to a PDF
#' document.  Note: this can be a very large document depending on
#' the number of observations and variables since it'll save the PDF
#' as vector graphics so each point will be rendered.  
#' 
#' @param dat dataset
#' @param colNm the column you want to plot
#' @param numObs The top n observations to include in the plot
#' @param totPerPage Total number of figures per page (default=9)
#' @param pdffile The file path/name to save to.
#' @export
splotPointsToPDF <- function(dat, colNms=NULL, numObs=NULL, totPerPage=9, pdffile) {
	
	if (!is.null(colNms)) dat = dat %>% select(one_of(colNms))
	
	## remove character and factor columns
	temp = data.frame(head(dat))
	gdNms = names(temp[, !sapply(temp, function(x) is.character(x) | is.factor(x) )])
	gdNms = names(temp[, sapply(temp, is.numeric)])
	dat = dat %>% select(one_of(gdNms))
	
	numPages=ceiling(length(gdNms)/totPerPage)
	
	splits=split(gdNms,sort(rank(gdNms) %% numPages))
	
	pdf(pdffile,width=8.5, height=11)
	
	
	for (i in 1:length(splits)) {
		colNmTemp=splits[[i]]
		splotDataPoints(dat, colNms=colNmTemp)
		
	}
	dev.off()
}






#' Exports the raw data point plots (splotDataPoints()) to a PDF
#' document.  Note: this can be a very large document depending on
#' the number of observations and variables since it'll save the PDF
#' as vector graphics so each point will be rendered.  
#' 
#' @param dat dataset
#' @param colNm the column you want to plot
#' @param numObs The top n observations to include in the plot
#' @param totPerPage Total number of figures per page (default=9)
#' @param pdffile The file path/name to save to.
#' @export
splotDistToPDF <- function(dat, colNms=NULL, byCol=NULL, baseCol="gray65", totPerPage=9, pdffile) {
	
	if (!is.null(colNms)) dat = dat %>% select(one_of(colNms, byCol))
	
	## remove character and factor columns
	temp = data.frame(head(dat))
	gdNms = names(temp[, !sapply(temp, function(x) is.character(x) | is.factor(x) )])
	gdNms = names(temp[, sapply(temp, is.numeric)])
	dat = dat %>% select(one_of(gdNms, byCol))
	
	numPages=ceiling(length(gdNms)/totPerPage)
	
	splits=split(gdNms,sort(rank(gdNms) %% numPages))
	
	pdf(pdffile,width=8.5, height=11)
	
	
	for (i in 1:length(splits)) {
		colNmTemp=splits[[i]]
		splotDataDist(dat, colNms=colNmTemp, byCol=byCol, baseCol=baseCol)
	}
	dev.off()
}






#' Wrapper for ggsave() with high resolution defaults for suitable 
#' raster-based graphics. 
#' 
#' @param filename what you want to call it (default: plot.png)
#' @param units units of measure (default: inches)
#' @param height height of plot in units above (default: 4 in)
#' @param width width of plot in units above (default: 6 in)
#' @param dpi resolution (default: 300 dpi)
#' @export
saveDefPlot <- function(filename="plot.png", units="in", height=4, width=6, dpi=300) {
	ggsave(filename, units="in", height=height, width=width, dpi=dpi)
}

