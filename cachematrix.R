## Put comments here that give an overall description of what your
## functions do

## ******************************************************************************* ## 
## Matrix inversion is usually a costly computation and there may be some benefit  ##
## to caching the inverse of a matrix rather than compute it repeatedly. THis R    ## 
## program consists of a pair of functions that cache the inverse of a matrix.     ##
## ******************************************************************************* ##


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix encompasses the following main functionalities:
## 1. Set matrix value
## 2. Get matrix value
## 3. Set inverse matrix value
## 4. Get inverse matrix value
makeCachedMatrix<-function(x=matrix()){
  x_inv<-NULL
  
  set<-function(y){
    x<<-y
    x_inv<<-NULL
  }
  
  get<-function() x
  setInv<-function(inv) x_inv<<-inv
  getInv<-function() x_inv
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
  
}

## ***************************************************************************************************
## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. if the inverse has already been calculated (and the matrix has not changed),
## then the cacheSolve should retrieve the inverse from the cache.
## The matrix is assumed to be invertible. Otherwise, ginv() might be used instead of solve(). 
cacheSolve<-function(x,...){
  
  m<-x$getInv()
  
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  mat<-x$get()
  m<-solve(mat)
  x$setInv(m)
  m
  
}
