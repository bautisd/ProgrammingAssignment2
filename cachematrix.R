
## Object for storing a cachable square matrix
## helper funcitons for get, set the matrix
## getinv,setinv the inverse of the matrix
## returns a list with the function definitions
makeCacheMatrix <- function(x = matrix()) {

  inv <-NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## calculates the inverse of a square matrix using the "solve" function
## If the inverse is already know from previous execution,
## returns the cached value of the inverse, does not recaculate the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check to see if the inverse is known
  inv <-x$getinv()
  if(!is.null(inv)){
    message("getting from cache")
    return(inv)
  }
  ## inverse is not known, calculate inverse,
  ## store new calculated invers in the cache object
  data<-x$get()
  inv<-solve(data, ...)
  x$setinv(inv)
  inv
}

