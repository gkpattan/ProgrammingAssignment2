## Usage example of "makeCacheMatrix" and "cacheSolve" together:
##  1a. my_matrix <- makeCacheMatrix(matrix(1:4,2,2))
##  1b. cacheSolve(my_matrix) (Executing the command once calculates the inverse of the matrix)
##  1c. cacheSolve(my_matrix) (Executing the command second time displays the message "getting cached data" and gets the cached inverted matrix)

## ASSUMPTION: For this assignment, assumption is that the matrix supplied is always invertible or else the following error is encountered
## Error in solve.default(data, ...) : Lapack routine dgesv: system is exactly singular


## makeCacheMatrix
## makeCacheMatrix has 4 functions
## 1. set function: sets a new matrix to x and resets the variable m
##      matrix can be set through either of following 2 ways:
##  1a. my_mat <- matrix(1:4,2,2)
##  1b. makeCacheMatrix(my_mat)
##                 OR
##  1x. my_matrix <- makeCacheMatrix()
##  1y. my_matrix$set <-matrix(1:4,2,2)

## 2. get function: gets the matrix .this can be used to see the matrix which was set using set function
##      example usage: my_matrix$get()

## 3. setInverse function: Inverse matrix can also be set directly. sets the inverse of the matrix to variable m 
##      example usage: my_matrix$setInverse(solve(matrix(1:4,2,2)))

## 4. getInverse function: gets the inverse matrix
##      example usage: my_matrix$getInverse()
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(my_inverted_matrix) m <<- my_inverted_matrix
    getInverse <- function() m
    
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve
##  if the inverse of the matrix is already calculated then
##      the message "getting cached data" is displayed and the inverse of the matrix is returned
##  else 
##      gets the matrix using the makeCachMatrx$get function, 
##      calculates the inverse, 
##      sets the inverse 
##      returns the inverse of the matrix
##  endif
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}

