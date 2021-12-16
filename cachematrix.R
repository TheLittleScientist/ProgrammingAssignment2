## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function makes and stores a matrix. By using get you
## get the matrix. 
## The <<- can modify variables on the parent level whereas <- only works on 
## the current level

makeCacheMatrix <- function(x = matrix()){
      inverse <- NULL
      set <- function(z){
             x <<- z
             inverse <<-NULL
      }
      get <- function(){x}
      setInverse <- function(inverse) {inverse <<- inverse}
      getInverse <- function() {inverse}
      list(set = set, get = get,
           setInverse = setInverse, getInverse = getInverse)

}
 

## Write a short comment describing this function
## This function gives the inverse of the matrix created before.
## The x$getinverse is what inverses the matrix and it gets assigned to inverse.
## If the inverse has already been calculated the function will skip this part 
## and use the cache data and show the message "Getting cached data".
## setInverse is used to set the value of the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inverse <- x$getInverse()
      if(!is.null(inverse)){
             message("Getting cached data")
             return(inverse)
      }
      calculation <- x$get()
      inverse <- solve(calculation, ...)
      x$setInverse(inverse)
      inverse
}
