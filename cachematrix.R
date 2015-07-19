## These functions create cached copies of the inverse of a given matrix, so no need
## for further calculations is needed when done once. The functions will access the
## cached version of the inverse, if exists, and will calculate the inverse from scratch
## if not.

## As in the assignment description, upon taking a matrix X a list of four functions
## is created: set - for setting up the value of the matrix-, get - to get the value
## of the matrix-, setInverse -to set the value of the inverse matrix-, 
## and getInverse -to get the value of the inverse matrix-.
makeCacheMatrix <- function(X=matrix()) {
	inverse_matrix<-NULL
	set <- function(Y) {
		X<<-Y
		inverse_matrix <<- NULL
	}
	get <- function() X
	setInverse <-function(inverse) inverse_matrix<<-inverse
	getInverse <- function() inverse_matrix
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## The following function calculates the mean of the special matrix
## created with the above function. 
## It first checks to see if the inverse has already been calculated. If so, it gets the ## inverse from the cache and skips the computation. Otherwise, it calculates the mean of ## the data and sets the value of the inverse in the cache via the set(Inverse) function.

cacheInverse <- function (X,...){
	inverse_matrix<-X$getInverse()
	if(!is.null(inverse_matrix)){
		message("getting cached data")
		return(inverse_matrix)
	}
	data<-X$get()
	inverse_matrix<-solve(data,...)
	X$setInverse(inverse_matrix)
	inverse_matrix
}