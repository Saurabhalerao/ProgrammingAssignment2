## Put comments here that give an overall description of what your
## functions do
##Caching the inverse of a matrix
##Matrix inversion is usually a costly computation
## There are benefits to caching inverse of matrix rather computing repeatedly
## Below two functions written to cache the inverse of a matrix

## Write a short comment describing this function
## Below function creates a matrix object that caches inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y)  {
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
##Below function computes the inverse of a matrix
## created by above function 
## If inverse is already computed then it retrives the inverse from cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
