## Pair of functions that create a matrix object that can cache its inverse

# creates special "matrix" object that can cache its inverse. makeCacheMatrix, creates list containing
# functions that set value of matrix, get value of matrix, set value of inverse, get value of inverse


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #set value of matrix, sets matrix x from  y
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  #get value of matrix, returns x
  get <- function() x
  #set value of inverse matrix to i
  setinverse <- function(solve) i <<- solve
  #get value of inverse matrix, returns i
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#computes inverse of special matrix returned by makeCacheMatrix above. If inverse has been
#calculated, then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  #retrieves possible cached data
  i <- x$getinverse()
  #checks if inverse is cached, if present, returns data
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  #if not, retrieves matrix and creates and sets inverse to cache, returns data
  mydata <- x$get()
  i <- solve(mydata, ...)
  x$setinverse(i)
  i
}
