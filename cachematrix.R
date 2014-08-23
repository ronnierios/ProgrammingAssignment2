##Two functions that are used to create a special object
## that stores a matrix and cache's its inverse

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ##Set Inverse Matrix variable to default value
    InverseMatrix <- NULL
    
    ##This function set new matrix value and reset Inverse Matrix value
    set <- function(y){
        x <<- y
        InverseMatrix <<- NULL
    }
    
    ##This function get matrix value
    get <- function() x
    
    ##This function get Inverse matrix value
    getInverseMatrix <- function() InverseMatrix
    
    ##This function set Inverse Matrix value
    setInverseMatrix <- function(im)  InverseMatrix <<- im
       
    ##create and return a list with functions to inverse a matrix
    list(set =set,
         get=get, 
         getInverseMatrix=getInverseMatrix,
         setInverseMatrix=setInverseMatrix)
        
}


##This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    
    #get Inverse matrix value
    InverseMatrix <- x$getInverseMatrix()
    
    ##Validates if the matrix has been inverted before
    if (!is.null(InverseMatrix))
    {
        message("getting cached inverse matrix")
        return (InverseMatrix)
    }
    ##Otherwie get the matrix value
    matrix <- x$get()
    ##Calculate and Inverse matrix
    invmatrix <- solve(matrix, ...)
    ##Set inverse matrix value to retrieve the inverse from the cache
    x$setInverseMatrix(invmatrix)
    ## Return a matrix that is the inverse of 'x'
    return (invmatrix)
       
}
