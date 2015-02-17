## The makeCacheMatrix stores the matrix and the inverse
## The cacheSolve functions uses makeCacheMatrix to cache the inverse of the matrix

## makeCacheMatrix creates a function object that can be used to cache the
##                 result of the matrix solve() function
## 
## set : Sets the matrix data
## get : Retrieves the matrix data
## store : Sets the inverse matrix data
## retrieve : Retrieves the matrix data
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  
  get <- function() x
  store <- function(invertable) m <<- invertable
  retrieve <- function() m
  list (set = set, get = get, store = store, retrieve = retrieve)
}


## cacheSolve uses makeCacheMatrix to cache the inverse of a matrix on first use
##            and there after to use the cached result
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$retrieve()
  if (!is.null(m)) {
    message("Returning cached matrix")
    return(m)
  }
  
  data <- x$get()
  m<-solve(data)
  x$store(m)
}


