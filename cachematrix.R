## The two following functions together compute an inverse matrix operation. 
## If A is the matrix that you want the inverse of, call the first function
##
##   a <- makeCacheMatrix( A )
##
## which stores "helper" functions for the computation in the list "a."
##
## Then call the second function
##
##   B <- cacheSolve( a )
##
## which returns B, the inverse matrix of A. This can be checked by doing
##
##   I <- A %*% B
##
## where I will be the "identity" matrix, a matrix the same size as A and B,
## filled with ones along the main diagonal and zeros elsewhere. Due to
## numerical rounding issues the zeros will, in all likelihood, not be
## exactly zero but be extremely close to zero: e.g. 3.053113e-16

## This first function creates the "helper" functions.
makeCacheMatrix <- function( x = matrix() ) {

  m_inv <- NULL
  set <- function( y ) {
    x <<- y
    m_inv <<- NULL
  } 
  get <- function() x
  set_inverse_matrix <- function( inverse_matrix ) m_inv <<- inverse_matrix
  get_inverse_matrix <- function() m_inv
  list( set = set , get = get ,
        set_inverse_matrix = set_inverse_matrix ,
        get_inverse_matrix = get_inverse_matrix )
  
} # end of makeCacheMatrix function

## This second function actually returns the inverse matrix.
cacheSolve <- function( x , ... ) {  
## Return a matrix that is the inverse of 'x'

# retrieve the cached inverse matrix, m_inv .
  m_inv <- x$get_inverse_matrix()

# which will be either NULL, i.e. not calculated yet, or not NULL,
# i.e. it has been previously calculated and cached.

# if m_inv is not NULL, just return the cached inverse matrix  
  if( !is.null( m_inv ) ) {
  message( "getting cached inverse matrix" )
  return( m_inv )
  }
  
# if m_inv is NULL, compute, cache and return the inverse matrix m_inv 
  data <- x$get()               # get the input matrix "A"
  m_inv <- solve( data )        # calculate the inverse matrix
  x$set_inverse_matrix( m_inv ) # cache the inverse matrix
  m_inv                         # return the inverse matrix
  
  } # end of cacheSolve function
