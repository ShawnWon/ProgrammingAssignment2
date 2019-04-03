## In this function, makeCacheMatrix creates a "matrix", which is 
## a list containing a function to:

##1,set the value of the matrix
##2,get the value of the matrix
##3,set the value of the inversed matrix
##4,get the value of the inversed matrix

## 

makeCacheMatrix <- function(x = matrix()) {
  
  #Define the variable "cache", set value to "NULL"
  cache<-NULL
  
  #store a matrix
  setMatrix<-function(new){
    x<<-new
    cache<<-NULL
  }
  
  #get the value of stored matrix
  getMatrix<-function(){
    x
  }
  
  #cache the argument
  cacheInv<-function(solve){
    cache<<-solve
  }
  
  #get the cached value
  getInv<-function(){
    cache
  }
  
  #return a list, which contains the 4 function names.
  list(setMatrix=setMatrix,getMatrix=getMatrix,cacheInv=cacheInv,getInv=getInv)

}


## This function calculates the inverse of the matrix that created by above function.

cacheSolve <- function(y, ...) {
  #get the cached value
  inv<-y$getInv()
  
  #if a cached value exists, then return it directly
  if(!is.null(inv)){
    message("cached data")
    return(inv)
  }
  
  #if no cached value,caculate and store in cache
  data<-y$getMatrix()
  inv<-solve(data)
  y$cacheInv(inv)
  
  #return the inverse result
  inv
}
