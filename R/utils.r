# function to get a list of all data frames in the current environment (will get dplyr and data.table too)
# from David L. Reiner post (https://stat.ethz.ch/pipermail/r-help/2011-October/291439.html)
my.ls <- function(pos=1, sorted=FALSE, mode, class){
	.result <- sapply(ls(pos=pos, all.names=TRUE),
			function(..x)object.size(eval(as.symbol(..x))))
	if (sorted){
		.result <- rev(sort(.result))
	}
	.ls <- as.data.frame(rbind(as.matrix(.result),"**Total"=sum(.result)))
	names(.ls) <- "Size"
	.ls$Size <- formatC(.ls$Size, big.mark=',', digits=0, format='f')
	.ls$Mode <- c(unlist(lapply(rownames(.ls)[-nrow(.ls)],
							function(x)base::mode(eval(as.symbol(x))))), '-------')
	if (!missing(mode)) {
		.ls <- .ls[sapply(.ls$Mode, function(x) any(unlist(strsplit(x, ", "))  %in% mode)), , drop=FALSE]
	}
	.ls$Class <- c(sapply(rownames(.ls)[-nrow(.ls)],
					function(x) paste(unlist(base::class(eval(as.symbol(x)))), collapse=", ")), '-------')
	if (!missing(class)) {
		.ls <- .ls[sapply(.ls$Class, function(x) any(unlist(strsplit(x, ", "))  %in% class)), , drop=FALSE]
	}
	.ls
}