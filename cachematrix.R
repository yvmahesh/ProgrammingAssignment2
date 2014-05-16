## Below function will cache the inverse of the matrix,
## when user tries to calculate matrix inversion using cachesolve method
## system will check the cache first will return the data from cache
## if its not available will calculate matric inversion and add the result to cache
## so that succesive calls will get data from cache for same input

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	## sets new matrix value
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
		## sets new matrix inverse value
        setinverse <- function(inverse) m <<- inverse
		## gets new matrix inverse value
        getinverse <- function() m
		## return list with get ,set ,setinverse,getinversemethods
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	## get inverse from cache matrix
	m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
		## in case of null caculate matrix inverse using solve
        m <- solve(data, ...)
		## set cacluate inverse to cachematrix
        x$setinverse(m)
		## Return a matrix that is the inverse of 'x'
        m
        
}
