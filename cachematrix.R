## Contains a function that creates a matrix object that can
## cache its matrix inverse result and a function that gets
## the matrix inverse of such a created matrix object.

## Creates a matrix object that can cache a result for future use.
## Creates a list of functions associated with that matrix object
## to do the necessary setting and getting of the matrix object
## and the setting and getting of the matrix inverse result.

makeCacheMatrix <- function( x = matrix() ) {
  inverse <- NULL
  
  ## Set the value of the matrix object.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## Get the value of the matrix object.
  get <- function() x
  
  ## Set the matrix inverse result.
  setinverse <- function(inv) inverse <<- inv
  
  ## Get the matrix inverse result.
  getinverse <- function() inverse
  list( set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse ) 
}


## Return a matrix that is the inverse of 'x'.

cacheSolve <- function( x, ... ) {
  ## Call the getinverse function to see if there's a cached result.
  ## If there is, we're done, return the cached result.
  inverse <- x$getinverse()
  if ( !is.null( inverse ) ) {
    message( "getting cached data" )
    return( inverse )
  }
  
  ## Otherwise, get the matrix anc calculate the inverse.
  data <- x$get()
  inverse <- solve( data, ... )
  x$setinverse(inverse)
  inverse
}
