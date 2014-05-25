# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                              #  "m" will store the cached inverse matrix
  
  set <- function(y) {                   #  Set matrix
    x <<- y
    m <<- NULL
  }
  
  get <- function() x                    #  Get matrix
  
  setm <- function(inverse) m <<- inverse#  Set inverse matrix
  
  getm <- function() m                   #  Get inverse matrix
  
  #  Return matrix defined functions
  list(set = set, get = get,setm = setm,getm = getm)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getm()
  
  if(!is.null(m)) {                     #  If the inverse is already calculated, return it
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()                       #  The inverse is not yet calculated, so we calculate it
  m <- solve(data, ...)  
  
  x$setm(m)                             #  Cache the inverse
  m                                     #  Return a matrix that is the inverse of 'x'
}

######### Sample:
# > x = rbind(c(1, -1/4), c(-1/4, 1))
# > Mat = makeCacheMatrix(x)
# > Mat$get()
#      [,1]  [,2]
# [1,]  1.00 -0.25
# [2,] -0.25  1.00
######## first run, no cashe
# > cacheSolve(Mat)
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
######## second run, getting cached data!!!
# > cacheSolve(Mat)
# getting cached data
#           [,1]      [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
