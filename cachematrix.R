## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Comments here
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL # this is where the result of inversion is stored
  set<-function(y){
    x<<-y
    inv<<-NULL # it also initialises xinv to null
  }
  get<-function()x # return the input matrix
  setInverse<-function(inverse) inv<<-inverse # set the inversed matrix
  getInverse<-function() inv # return the inversed matrix
  list(set=set, get=get, setInverse=setInverse,
       getInverse=getInverse) # return a list that contains these functions
  # to change matrix, to get the setted matrix and the inversed matrix, and
  # to set the inversed matrix
}


## Write a short comment describing this function
##Comments
cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse() # get the inversed matrix from object x
  if(!is.null(inv)){message("getting cached data") # if the inversion result is 
    # there message (getting cached data)
    return(inv)} # return the calculated inversion
  mat<-x$get() # if not we do x$get to get the matrix object
  inv<-solve(mat, ...) # solve it
  x$setInverse(inv) # then set it to the object
  inv # return the solved result
}
