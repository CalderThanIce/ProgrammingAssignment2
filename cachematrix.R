## -------------------------------------------------------------------------------------------------
## Together these functions that are able to cache potentially time-consuming computations.
## For example, taking the inverse of a matrix can be a fast operation, using the solve() function.
## However, for a very large matrix, it may take too long to compute the inverse,
## especially if it has to be computed repeatedly (e.g.in a loop). 
## If the contents of a matrix are not changing, it may make sense to cache the value of the inverse
## so that when we need it again, it can be looked up in the cache rather than recomputed.
## We take advantage of the scoping rules of the R language and how they can be 
## manipulated to preserve state inside of an R object.



## -------------------------------------------------------------------------------------------------
## makeCacheMatrix is a function that builds a`makeVector` creates a special "vector", which is
## really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of a matrix
## 4.  get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    ## set up a variable inv and assign it a NULL value
    inv <- NULL
    
    ## define a function "set" which globally assigns it's input to the variable x
    ## and globally assigns NULL to the variable inv
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## define funtion "get" which returns x
    get <- function() x
    
    ## define funtion "setinverse" which globally assigns it's input to the variable inv
    setinverse <- function(inverse) inv <<- inverse
    
    ## define funtion "getinverse" which returns inv
    getinverse <- function() inv
    
    ## Output is a list of these funtions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## -------------------------------------------------------------------------------------------------
## cacheSolve is a function that takes the output of makeCachematrix.
## It calculates the inverse of the special "matrix" created with the above function.
##  However, it first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the `setinverse` function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # First get any cached data, if it exists (otherwise we will reutrn NULL)
    inv <- x$getinverse()
    
    ## Then check if what we got previously is NULL or not
    if(!is.null(inv)) {
        message("getting cached data")
        
        ## if the inverse is already stored (i.e. inv is not NULL) we can simply output inv
        ## and exit the function
        return(inv)
    }
    
    ## If inv is NULL, we will have to get the data and calculate the inverse
    ## This gets the original matrix we input to makeCachematrix
    data <- x$get()
    
    ## This calculates thi inverse
    inv <- solve(data, ...)
    
    ## Now we cache our inverted matrix, for the next time this funtion is run 
    ## (i.e. in the future inv <- x$getinverse() will no longer be NULL!)
    x$setinverse(inv)
    
    ## Return inverted matrix
    inv
    
}
