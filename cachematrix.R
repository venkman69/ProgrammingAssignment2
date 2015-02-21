## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# A custom matrix object with a solved (inverted) matrix
# held such that it does not need recomputation
makeCacheMatrix <- function(mat = matrix()) {
  invmat<-NULL
  set <-function(newmat){
    mat<<-newmat
    invmat<<-NULL
  }
  get<-function() mat
  setinv <- function(inv) invmat<<-inv
  getinv <- function() invmat
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- try(solve(data, ...))
  if (class(inv)=="try-error"){
    message("**** Solve failed.")
    return (NULL)
    
  }
  x$setinv(inv)
  inv
}
