## set function creates cached value of x
## get function fetches the value of x
## setmat caches the output of solve function

## getinv calls invmat function which inverses
#  the matrix and caches the output
#  if new data comes in the value will be set to NULL

makeCacheMatrix <- function(x = matrix()) {
        invMat <- NULL
          set <- function(y){
            x <<- y
            invMat <<- NULL
          }
        get <- function() x
        setmat <- function(solve) invMat <<- solve
        getinv <- function() invMat
        list(set = set, get = get,
             setmat = setmat,
             getinv = getinv)
  }



## Checks if getinv function has any value i.e. if the input hasnot changed
#  and returns the already cached output
## if the input has changed then get function is used to fetch the new data
#  and inverse function (solve) is used to calculate the new result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}