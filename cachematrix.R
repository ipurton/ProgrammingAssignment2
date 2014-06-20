## This pair of functions creates and caches an inverse of a matrix, x, which
## can be used to avoid the need to repeadetly use the solve function.

## Stores a list of functions (set, get, setinverse, & getinverse) into a
## variable as defined by user. Allows for the functionality of cacheSolve by 
## offering a blank matrix, x, and the aforementioned child functions.

## Usage: store makeCacheMatrix in a variable, such as 'z'
## Define a matrix using z$set(matrix(...))
## Verify matrix's storage using z$get

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
          ##establishes variable 'inv' for later functions
     set <- function(y) {
          x <<- y
               ##stores matrix 'y' in the matrix 'x' from parent function
          inv <<- NULL
               ##REMOVES any previously cached inverse matrix
     }
     get <- function() x
          ##prints matrix 'x'
     setinverse <- function(inverse) inv <<- inverse
          ##'inv' from cacheSolve is stored in the 'inv' variable of THIS FUN
     getinverse <- function() inv
          ##prints the inverse matrix stored in 'inv'
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
               ##creates a list of these variables; allows them to be called via
               ##z$set, z$get, etc
}

## Will either use solve() on the matrix x to print the inverse of it, or will
## return the cached copy of it.

## Usage: cacheSolve(z) will either print the inverse of x or a cached copy

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
          ##stores getInverse from prior function into 'inv'
     if(!is.null(inv)) {
               ##if 'inv' is NOT null...
          message("fetching cached inverse matrix")
               ##print this message...
          return(inv)
               ##and print the variable as is.
     }
     data <- x$get()
          ##Else, store the matrix 'x' from above into 'data
     inv <- solve(data, ...)
          ##solves matrix 'x' (creates inverse matrix) and stores it into 'inv'
     x$setinverse(inv)
          ##see above; this will set the 'inv' used in getinverse to the 'inv'
          ##from THIS function. This provides the cached copy.
     inv
          ##prints 'inv', the inverse matrix
}
