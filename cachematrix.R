## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL ##stores the matrix in cache
  }
  get <- function() x ## gets matrix
  setInverse <- function(solve) m <<- solve ## sets inverse
  
  getInverse <- function() m ## gets inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) ##creates list of the functions
}


##cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
##If the inverse has already been calculate, then the cacheSolve function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){ ## If inverse previously calculated, skip computation
    message("getting cached data") # send message to indicate cache
    return(m) ## return cache
  }
  data <- x$get() ## If not previously calculated, get matrix
  m <- solve(data, ...) ## calculate inverse
  x$setInverse ## store in cache using makeCacheMatrix function
}
