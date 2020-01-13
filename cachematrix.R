## The purpose of this function is to demonstrate the 
## operation of lexical scoping. 
## It covers two sub-functions.  
## The first sub-function (makeCacheMatrix) 
## creates a matrix object that can cache its own inversion. 
## The second sub-function cacheInv then computes the 
## inverse of the object returned by sub-function one. 

## This is the first sub-function that can cache its own inversion. 

makeCacheMatrix <- function(x = matrix()) {
  ##Defining makeCacheMatrix as a function of x which
  ##is to a matrix. For the purpose of this exercise, it is 
  ##assumed that all matrices are inversible. (BIG ASSUMPTION)
  a <- NULL
  ## sestting variable a to NULL for use later in the code. 
  set <- function(b){
    x<<-b
    a<<-NULL
  }
  ##set is a nested function within makeCacheMatrix.
  ##it is a mutator as it sets data value to symbols. 
  ##operator <<- assigns the values on the right 
  ##handside to objects defined in the parent environment 
  ## (i.e. makeCacheMatrix environment) on the left. 
  
  get<-function()x
  ## retriving x from the parent environment 
  ## as it is not defined in the get function. 
  
  setInv<-function(solve)a<<-solve
  ## assigning input argument of solve (inversing a matrix) function 
  ## to object a (a matrix)
  ## in the parent environment so that it can accessed later
  
  getInv<-function()a
  ## accessor for object a which is not defined here
  ## so it is retrieved from the parent environment
  ## i.e. the inversed matrix
  
  list(set=set,get=get,
       setInv=setInv,
       getInv=getInv)
  ## naming the list of elements so that 
  ## we can use $ to extract what is required 
  ## in downstream codes. 
}

cacheInv <- function(x,...) {
  ## defining cacheInv as a function of x and anonymous function
  ## to allow for additional in put. 
  a<- x$getInv()
  ## attempting to retrieve an output from makeCacheMatrix
  ## to see if result is NULL. 
  if(!is.null(a)) {
    message ("getting cached data")
    return(a)
    ## if result if not NULL, then return that result
    ## an a messege. 
  }
  data<-x$get()
  a<- solve(data,...)
  x$setInv(a)
  a
  ## if result is NULL, then compute an inversed matrix and display. 
}

b <- makeCacheMatrix(matrix(c(1,4,6,8,9,3,5,4,3),3,3))
cacheInv(b)
