## The functions makeCacheMatrix and cacheSolve enable a user to 
## "save" computation bandwidth by caching the results of a matrix
## rather than having to repeatedly compute it.

## makeCacheMatrix creates a special "matrix" object that can cache
## its inverse by setting the value of the matrix, getting the
## value of the matrix, setting the value of the inverse, and
## getting the value of the inverse

makeCacheMatrix <- function (x = numeric()){
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <-function(solve)  m<<-solve
  getinverse <- function() m
  list(set=set,get=get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve returns a matrix that is the inverse of 'x'
cacheSolve <- function(x,...){
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-mean(data,...)
  x$setinverse(m)
  m
} 
