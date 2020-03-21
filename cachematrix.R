#Caching inverse of a matrix
#Creates a matrix that can cache it's inverse
#Args:
#x: A matrix
#Returns:
#A matrix with functions to get/set value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
#cached inverse of matrix
  inv <- NULL
#getter/setter for matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
#getter/setter for matrix inverse
  getinv <- function() inv
  setinv <- function(inverse) inv <<- inverse
#return list of functions for matrix
  list(get=get, set=set, getinv=getinv, setinv=setinv)
}
#Computes the inverse of a matrix. If the inverse has already been
#calculated before, the cached inverse is returned
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # return cached matrix inverse if it's been already computed
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  # compute inverse of matrix 
  m <- x$get()
  inv <- solve(m, ...)
  
  # cache inverse
  x$setinv(inv)
  
  # return inverse of matrix
  return(inv)
}
#Example
#Avoid the singulrity
m <- matrix(c(1, 2, 3, 0, 5, 6, 7, 8, 107), nrow = 3, ncol = 3, byrow = TRUE)
m2 <- makeCacheMatrix(m)
cacheSolve(m2)
#[,1]        [,2]         [,3]
#[1,]  1.04506438 -0.40772532 -0.006437768
#[2,]  0.09012876  0.18454936 -0.012875536
#[3,] -0.07510730  0.01287554  0.010729614
cacheSolve(m2)
inverse is cached
#[,1]        [,2]         [,3]
#[1,]  1.04506438 -0.40772532 -0.006437768
#[2,]  0.09012876  0.18454936 -0.012875536
#[3,] -0.07510730  0.01287554  0.010729614