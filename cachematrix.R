## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function is responsible for set, get, setinverse and getinverse operations
makeCacheMatrix <- function(x = matrix()) {
   inverse_matrix <- NULL
   
   set <- function(mat){
     x <<- mat
     inverse_matrix <<- NULL
   }
   
   get <- function() x  
   
   setinverse <- function(im) inverse_matrix <<- im 
     
   getinverse <- function() inverse_matrix
   
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

## Write a short comment describing this function
# This function checks whether the inverse matrix is already present or not. If present, it returns it otherwise
# it solves for inverse matrix and then returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
      message("getting cached data")
      return(inverse_matrix)
    }
    data <- x$get()
    inverse_matrix <- solve(data, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}

## Sample Example for Testing
myMatrix <- makeCacheMatrix(matrix(c(1,2,1,3,4,5,2,1,3), nrow = 3, byrow = TRUE))

cat("Original Matrix:\n")
print(myMatrix$get())

invMatrix <- cacheSolve(myMatrix)
cat("\nInverse Matrix:\n")
print(invMatrix)

cachedInvMatrix <- cacheSolve(myMatrix)
cat("\nCached Inverse Matrix:\n")
print(cachedInvMatrix)





