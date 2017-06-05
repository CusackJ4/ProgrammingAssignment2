##' The following function is used to compute the inverse of a matrix
##' and then cache the result. Matrix inversions are a costly computation
##' and it may be beneficial to store the computed value in a cache
##' rather than having to continuously compute the value. 



#' makeCacheMatrix creates a list that contains a function to
#' 1. Set the value of the matrix
#' 2. Get the value of the matrix
#' 3. Set the value of the inverse of the matrix
#' 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#' This function is used to solve the inverse of the matrix. It first
#' checks to see if the inverse has been computed and stored. 
#' If it has, the function returns the cached value along with a message 
#' stating this, and then exits the function. If not, it computes the
#' inverse and then uses the setinverse function to set the value in
#' the cache. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


#Test-a
a <- makeCacheMatrix(matrix(1:4, 2, 2)) #assigns a matrix to an object "a", of 
a$get() #prints the value of the matrix
cacheSolve(a) #prints the inverse of the matrix
# running cacheSolve(a) a second time provides "getting cached data" message,
# confirming data is retrieved from the cache

a$set(matrix(4:7, 2, 2)) #provides a new matrix value to object "a" of type
# makeCacheMatrix()
cacheSolve(a) #prints the inverse of the new matrix
# running again provides "getting cached data" message, confirming data is
#'retrieved from the cache



#Test-b
vectorByThree <- makeCacheMatrix(matrix(c(3, 0, 2, 2, 0, -2, 0, 1, 1), nrow = 3,
                                        ncol = 3, byrow = TRUE)) #assigns a 
# matrix to object "vectorByThree"
vectorByThree$get() #prints the value of the matrix
cacheSolve(vectorByThree) #prints the inverse of the matrix
# running cacheSolve(vectorByThree) a second time provides "getting cached data" 
# message, confirming data is retrieved from the cache

byThreeInv <- cacheSolve(vectorByThree) #stores the value of the inverse matrix
# into byThreeInv, if cacheSolve(vectorByThree) has been run, it will
# retrieve this value from the cache, along with "getting cached data" message
byThreeInv #printing byThreeInv will not retrieve the data from the cache, as 
# the data is stored directly in byThreeInv as a matrix object.


#Example Run: Test-a
#> a <- makeCacheMatrix(matrix(1:4, 2, 2))
#> a$get()
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

# cacheSolve(a) 
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
#> cacheSolve(a)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#> a$set(matrix(4:7, 2, 2))
#> cacheSolve(a)
#[,1] [,2]
#[1,] -3.5    3
#[2,]  2.5   -2
#> cacheSolve(a)
#getting cached data
#[,1] [,2]
#[1,] -3.5    3
#[2,]  2.5   -2
#> 




