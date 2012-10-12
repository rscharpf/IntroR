sd2 <- function(x){  ## a function with a single argument
	sumsOfSquares <- sum((x-mean(x))^2)
	df <- length(x)-1
	result <- sqrt(sumsOfSquares/df)
	return(result)
}

sdMatrix <- function(x, MAR=c(1:2), na.rm=TRUE){
	valid.x <- is(x, "matrix") && is(x[,1], "numeric")
	if(!valid.x) stop("x must be a numeric")
	if(length(MAR) != 1 | !MAR %in% 1:2)
		stop("MAR must be '1' or '2'")
	msg <- switch(paste("margin", MAR, sep=""),
		      margin1="row-wise",
		      margin2="column-wise")
	message("Calculating the ", msg, " standard deviations")
	if(any(is.na(x)) && na.rm){
		warning("Missing values are ignored")
	}
	apply(x, MAR, sd, na.rm=na.rm)
}
