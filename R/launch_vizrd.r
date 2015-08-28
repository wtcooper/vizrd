
#' Open an interactive browser window to explore
#' the dataset through shiny.
#' 
#' @param dfs A vector of dataset names to use.
#' @examples
#' explore_data(c("iris","mtcars"))
#' @export
explore_data <- function(dfs) {
	
	require(shiny)
	require(Hmisc)
	
	## Could set this up to pass a list of 
	
	dfLs <<- mget(dfs, envir=globalenv())
		
	if (interactive()) shiny::runApp(system.file("R", package = "vizrd"))
	
	
}


#' Open an interactive browser window to explore
#' all datasets (data.frame and inheritors) in the global
#' environment.  
#' 
#' @examples
#' explore_all_data()
#' @export
explore_all_data <- function() {
	
	require(shiny)
	require(Hmisc)
	
	# Get all datasets
	dfs = my.ls(class="data.frame")
	dfLs <<- mget(rownames(my.ls(class="data.frame")), envir=globalenv())
	
	if (interactive()) shiny::runApp(system.file("ShinyFiles", package = "vizrd"))
	
	
}

