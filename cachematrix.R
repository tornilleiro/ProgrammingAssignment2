## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  getter an setter functions to save data values of matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



## Write a short comment describing this function
#  1. if the X object has inverse. I return the value.
#  2. If the X object x has not inverse calculated, I check following cases
#     2.1 I check if the matrix is square.
#     2.2 I check if the determinant of matrix is not 0.
#  3. If two steps (2.1 and 2.1) before are ok I use the funcion solve  to calculate the inverse

cacheSolve <- function(x) {
    
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting inverse matrix cached data")
        return(m)
    }

    data <- x$get()
    di<-dim(data)
    if(di[1]!=di[2]) {
        message("The matrix is not square") 
        return(-1)
    }
    d<-det(data)
    if (d==0){
      message("This is a singular matrix, can not be inverted")
      return(-1)
    }
    
     m <- solve(data)
    x$setinverse(m)
    m
  
}
