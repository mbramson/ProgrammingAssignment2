## makeCacheMatrix returns a vector of functions (set, get, setinversematrix, and getinversematrix).
## It's input is a matrix (presumed to be invertible as no code exists to check this). If no matrix is supplied
## then the associated matrix will default to a 1x1 matrix with an NA value.

## The set function allows you to set the value of the matrix associated with makeCacheMatrix.

## The get function returns x, which is the previous matrix which was associated with makeCacheMatrix either with the
## set function or while first running the function.

## The setinversematrix function takes the inverse of a supplied matrix and stores it in m.

## the getinversematrix function returns m which is the inverse of the matrix x (or Null if setiversematrix has never
## been called.)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) m <<- solve
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## cacheSolve returns the inverse matrix of the one supplied previously to the makeCacheMatrix function (or supplied
## through the set function of makeCacheMatrix.)

## It first checks if m is Null. If m is not null, then it knows that the inverse has previously been computed and stored,
## and simply returns that cached value. If m is null, then it has to compute the inverse which it does.

## The end result of this is that once the inverse of a specific matrix has been solved, it is stored in memory.
## When that value is called again, it does not have to recalculate, it can simply query the memory 
## (saving time on larger matrices).

cacheSolve <- function(x, ...) {
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}
