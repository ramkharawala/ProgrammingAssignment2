####################################
### ASSIGNMENT 2 - R PROGRAMMING ###
### DONE BY: RAM KHARAWALA       ###
### R code for cachematrix.R     ###
####################################

rm(list=ls())

## The following makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function(){x}
  setinv <- function(solve){m <<- solve}
  getinv <- function(){m}
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## The following function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache. 
cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

#Example 1: 2*2 matrix
a <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
a$get()
cacheSolve(a)

#Example 2: 3*3 matrix
b <- makeCacheMatrix(matrix(c(-10,19,-30,41,0,-10,-7,8,23), nrow=3, ncol=3))
b$get()
cacheSolve(b)

#Example 3:
c <- makeCacheMatrix(matrix(c(4,13,97,65,52,78,2,0,3,27,43,0,9,1,99,14), nrow=4, ncol=4))
c$get()
cacheSolve(c)
