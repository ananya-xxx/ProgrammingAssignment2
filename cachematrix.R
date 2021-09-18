## makeCacheMatrix function creates a special "matrix" object that can cache its inverse. 

library(MASS)
makeCacheMatrix <- function(x = matrix())
{
  inv<-NULL 
  ## here the variable for inverse (inv) has been initialized as NULL 
  set<-function(y)
  {
    x<<-y
    inv<<-NULL
  }
  get<-function()x 
  ## here the function to get matrix x has been defined 
  setinv<-function(inverse)inv<<-inverse 
  getinv<-function()
  {
    inver<-ginv(x)
    inver%*%x 
    ## this is the function that is being used to obtain the inverse of the matrix x as required 
  }
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatric above. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache. 

cacheSolve<-function(x, ...) 
## the function defined here retrieves the cache data 
{
  inv<-x$getinv()
  if(!is.null(inv)) 
  ## here it is being checked whether or not the variable inv (for inverse) is NULL 
  {
    message("Getting cached data!")
    return(inv) 
    ## this function returns inverse value from the cache data retrieved 
  }
  data<-x$get()
  inv<-solve(data, ...) 
  ## this function is used to calculate the inverse value if it is NULL in the cache data retrieved 
  x$setinv(inv)
  inv 
  ## this functions displays the inverse matrix of the matrix x given earlier 
}