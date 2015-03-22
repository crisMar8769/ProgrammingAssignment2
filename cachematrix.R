## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {                          ## set the value of the matrix
                 x <<- y
                 inv <<- NULL
         }
         get <- function() x                           ## get the value of the matrix
         setinv <- function(inverse) inv <<- inverse   ## set the value of the inverse to the cache
         getinv <- function() inv                      ## get the value of the inverse from the cache
         list(set = set, get = get,                    ## returns a list with the 4 objects created
              setinv = setinv,
              getinv = getinv)

}


## The following function calculates the inverse of the matrix. 
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the result in the cache via the setinv function

cacheSolve <- function(x, ...) {
         inv <- x$getinv()                       ## 
         if(!is.null(inv)) {                     ## check if the inverse has already been calculated
                 message("getting cached data")
                 return(inv)                     ## the inverse previously calculated y returned
         }
         data <- x$get()                         ## the matrix is stored in the object called 'data'
         inv <- solve(data, ...)                 ## the inverse is calculated and stored in the object 'inv'
         x$setinv(inv)                           ## the inverse is set in the cache via the setinv function
         inv                                     ## the inverse is returned

}
