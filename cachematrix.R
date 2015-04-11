## The purposes of these functions:
##  - to set initial matrix,
##  - to print initial matrix,
##  - to count inversed matrix,
##  - to keep inversed matrix in the cache as long as initial matrix did not changed.

## The 'makeCacheMatrix' function:
##  - creates the blank matrix by calling function without arguments,
##  - method 'set(data)' puts data to the initial matrix,
##  - method 'get()' returns an initial matrix,
##  - method 'getInversed()':
##  - - returns an inversed matrix,
##  - - informs user whether inversed matrix was extracted from cache or newly counted
##  - - informs user in case initial matrix wasn't set yet

makeCacheMatrix<- function(x = matrix()) {

    solveIsActual<- FALSE
    inversedMatrix<- matrix()
    
    set<- function(originalMatrix) {
        x<<- originalMatrix
        solveIsActual<<- FALSE
    }
    
    get<- function() x

    getInversed<- function() {
        if (solveIsActual) {
            message("getting cached data")
            return(inversedMatrix)
        }
        else {
            if (is.na(x[1,1])) message("seems like original matrix wasn't set yet")
            else {
                message("counting new data and placing into cache")
                inversedMatrix<<- solve(x)
                solveIsActual<<- TRUE
                return(inversedMatrix)
            }
        }
    }
    
    list(set = set,
         get = get,
         getInversed = getInversed)
}

## The 'cacheSolve' function.
## Returns a matrix that is the inverse of 'x'.

## It's actually useless function in this case.
## Placing all the functionality to one function 'makeCacheMatrix' reduced the number of variables
## and number of cross-calls between the separate functions.
## But I have kept this function to provide compatibility.

cacheSolve<- function(x, ...) {
    x$getInversed()
}