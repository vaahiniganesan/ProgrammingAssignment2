## The two functions written below either calculate the inverse of a square invertible matrix
## or return the inverse value from cache depending on if the inverse for that matirx
## has already been calculated. Thus, these functions help in saving the cost of
## computing matrix inverse. 

## The makeCacheMatrix function creates a special matrix which is originally a list of the folliwng functions:
## 1. Set the value of matrix.
## 2. Get the value of matrix.
## 3. Set the matrix inverse using solve(x) function.
## 4. Get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m_inv <- NULL
  
  ## The matrix from the user is stored in the variable x for operation. 
  ## The matrix inverse variable is empty.
  set <- function(y) {
    x <<- y
    m_inv <<- NULL
  }
  
  ## The matrix assigned to x is returned.
  get <- function() x
  
  ## The matrix inverse is calculated using the solve(x) function.
  ## This assumes the matrix to be square and invertible.
  setinv <- function(minv) m_inv <<- solve(x)
  
  ## The inverse of the matrix is returned.
  getinv <- function() m_inv
  
  ## This forms the list of functions in the special matrix created by the
  ## makeCacheMatrix function.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The cacheSolve function returns the inverse of a matrix given as input by the user.
## This function saves computational cost for the inverse calculation by first checking
## if the matrix inverse has already been calculated.
## If yes, then the inverse is simply returned from cache. If not, i.e. if it is a
## new matrix (whose inverse has not previously been caclulated), then it computes
## the inverse using solve(x) function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Get the value of the m_inv variable from makeCacheMatrix function and check
  ## if it already contains the inverse for that matrix. If it does, then notify the
  ## user that the inverse is getting returned from cache and return the inverse value.
  m_inv <- x$getinv()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  
  ## If the m_inv variable did not contain the inverse for that matrix, then
  ## the inverse is calculated using the solve(x) function.
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
  
}
