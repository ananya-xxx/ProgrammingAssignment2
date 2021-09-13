## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 
## library(MASS) is used to calculate inverse for non-squared as well as square matrices. 

library(MASS)
makeCacheMatrix <- function(x = matrix())
{
  inv<-NULL ##initializing inverse as NULL 
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()x ##function to get matrix x 
  setinv<-function(inverse)inv<<-inverse 
  getinv<-function()
  {
    inver<-ginv(x)
    inver%*%x #function to obtain inverse of the matrix
  }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatric above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache. 

cacheSolve<-function(x, ...) ##gets cache data 
{
  inv<-x$getinv()
  if(!is.null(inv)) ##checking whether inverse is NULL 
  {
    message("Getting cached data!")
    return(inv) ##returns inverse value 
  }
  data<-x$get()
  inv<-solve(data, ...) ##calculates inverse value 
  x$setinv(inv)
  inv ##return a matrix that is the inverse of 'x' 
}