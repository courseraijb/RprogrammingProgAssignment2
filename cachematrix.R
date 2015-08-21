## Author: Ian Burchett - 2015-08-21

##Description:
## This R package demonstrates the caching function of R, via manipulating a 
## given invertable, square, matrix. Two function are provided: makeCacheMatrix,
## and cacheSolve, as described below:

##makeCacheMatrix:
## This function accepts a given square invertable matrix as its single argument
## and returns an R element with functions, storing the given matrix as an
## attribute. The functions include a getter and setter for the matrix, which
## implement R's caching functionality.

makeCacheMatrix <- function(input_matrix = matrix()) {
        #set up the inverse_matrix attribute to store the inverse
        inverse_matrix <- NULL
        
        #setup the set function, which sets both input and inverse matricies
        set <- function(set_value) {
                input_matrix <<- set_value
                inverse_matrix <<- NULL
        }
        
        #setup the get function which returns the input matrix
        get <- function() input_matrix
        
        #setup the set_inverse_matrix function sets the inverse matrix
        set_inverse_matrix <- function(matrix) inverse_matrix <<- matrix
        
        #setup the get_inverse_matrix function which returns the inverse matrix
        get_inverse_matrix <- function() inverse_matrix
        
        #list the available functions within the CacheMatrix object
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix, 
             get_inverse_matrix = get_inverse_matrix)
}


##cacheSolve:
## This function demonstrates R's caching functionality by accepting a 
## cachingmatrix object described by makeCacheMatrix, and then checks the cache
## for the cached inverse (it was already calculated previously). If the cache
## does not contain the inverse, the function performs the solve(matrix) call,
## which is the built in "solve for inverse of a square matrix" function within
## the R language. The inverse of the input matrix is then returned, whether 
## from cache or calculated manually. Via utilization of the set_inverse_matrix function
## the caching is achieved, so the next time the very same call is made, the
## data will be pulled from cache instead of calculated.

cacheSolve <- function(input_matrix, ...) {
        ## Return a matrix that is the inverse of 'input_matrix'
        inverse_matrix <- input_matrix$get_inverse_matrix()
        
        #check if data is already calculated, residing in the cache
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                
                #return the cached data
                return(inverse_matrix)
        }
        
        #since the data wasn't cached, calculate the inverse matrix
        data <- input_matrix$get()
        inverse_matrix <- solve(data)
        input_matrix$set_inverse_matrix(inverse_matrix)
        
        #returning the inverse matrix
        inverse_matrix
}