myfunction <- function(x) {
	y <- norm(x)
	mean(y)
}

second <- function(x) {
	x + rnorm(length(x))
}
