## This assignment reduces computational load by caching the inversion of a
## given invertible matrix. It will check to see if the given matrix inversion 
##exists before calculating another inversion of the same square

## makeCacheMatrix creates an 8x8, length 64 invertible square matrix
## of random normal variables. matrix(1:n, ncol=x, ncol=y) could have
## been used to create a progressive sequence of a square matrix but
## the rnorm was more fun to work with.

makeCacheMatrix <- function() {
    x <- matrix(rnorm(64), ncol = 8, nrow = 8)
    return(x)
}

## Creates a list of four functions that are used for evaluating
## the presence of a cached inverted matrix and then setting or
## getting the new (or genesis) matrix vua passing to/from
## cacheInversion

makeMatrix <- function(x) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinversion <- function(solve) m <<- solve
    getinversion <- function() m
    list(set = set, get = get,
         setinversion = setinversion,
         getinversion = getinversion)
}

## cacheInversion is a function with arguments x that perform either returns
## the previously cached inverted matrix, thereby reducing computational load
## via 'set' or if the inversion is.null then via the 'get' it will store the
## inversion that it receives from x of cacheSolve. Additionally, another stud-
## ent recommended a Sys.time function to show how quickly the calculations
## were occurring so it seemed a prudent add on and good exercise to explore
## some other functions within R.

cacheInversion <- function(x, ...) { # argument x from makeCacheMatrix
    start_time <- Sys.time() #sets the System Time at the onset of action
    m <- x$getinversion() # stores the inverted matrix from cacheSolve
    if(!is.null(m)) { #if m is not null (prev cache in) then return same
        message("getting cached data") #user notice
        return(m) #returned value of m, which is the inversion
    }
    data <- x$get #gets the matrix from makeCacheMatrix
    m <- solve(data, ...) #performs the inversion and binds it to m
    x$setinversion (m) #sets m
    end_time <- Sys.time() #finishes system time calc from above
    time_duration <- end_time - start_time #performs a delta
    sprintf("the time to calculate was %s", time_duration) #prints the time
    
    m #returns the value of m, either cached or new
}
