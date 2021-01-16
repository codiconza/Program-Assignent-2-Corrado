## First function creates a special "matrix" object that can cache its inverse.

makeVector <- function(x = numeric()) {
      m <- NULL
      
      ## Set the value of the matrix 
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      ##Get the value of the matrix
      get <- function() x
     
      ##Set the value of the inverse
      setmean <- function(mean) m <<- mean
      ##Get the value of the inverse
      getmean <- function() m
      list(set = set, get = get, setmean = setmean, getmean = getmean)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cachemean <- function(x, ...) {
      m <- x$getmean()
      
      ## If inverse has already been calculated then get previous calculation and return "getting cached data"
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## Otherwise compute inverse of of matrix
      data <- x$get()
      m <- mean(data, ...)
      x$setmean(m)
      m
}
