
## makes an object into a matrix that can cache the inverse of square matrices
makeCacheMatrix <- function( m = matrix()) {
        # create inverse variable 
        i <- NULL
        
        # set the matrix method
        set <- function(matrix){
                m <<- matrix
                i <<- NULL
        }
        
        # get the matrix method
        get <- function() {
                m
        }
        
        # set the inverse method
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        # get the inverse method
        getInverse <- function() {
                i
        }
        
        #display list of methods created
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Takes the output of the above function and calculates the inverse
## unless it has already been calculated and stored in the cache, then
## this function will retrieve the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        # return inverse if already stored in cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # get the matrix
        data <- x$get()
        
        # find the inverse of the matrix with multiplication
        m <- solve(data) %*% data
        
        # set the inverse as the matrix
        x$setInverse(m)
        
        # return the inverse matrix
        m
}
