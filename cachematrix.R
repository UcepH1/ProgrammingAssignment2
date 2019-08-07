### ----------------- WEEK 3 Assignement ----------------- ### 
### by UcepH ###

# Here we created two functions to help us avoid unecessary calculations by caching  the inverse of a given square
# matrix M and reusing it when needed

# NB : an important assumption made is that matrix M in invertible !

# First function here creates a special list of four functions associated to matrix M: 
# - a set function to initialize value of Matrix
# - a get function to retrieve value of Matrix
# - a setsolve function to calculate inverse of Matrix 
# - a getsolve function to retrieve inverse of Matrix from previous calculation
# Resulting output of this function will give you acces to the value of input Matrix by using "output_list$get()"
makeCacheMatrix <- function(M = matrix()) {
        s <- NULL
        set <- function(y) {
                M <<- y
                s <<- NULL
        }
        get <- function() M
        setsolve <- function(M) s <<- solve(M)
        getsolve <- function() s
        list(set = set, get = get, # output list
              setsolve = setsolve,
              getsolve = getsolve)
}


# Second function here uses our special list of four elements i.e. the special_M to: 
# - calculate inverse of Matrix if no previous calculation were made
# - retrieve inverse of Matrix from previous calculation
# Indeed, after applying this function for the first time to an output list created by makeCacheMatrix, 
# the output list will contain the inverse of the input Matrix
cacheSolve <- function(special_M) {
        s_cache <- special_M$getsolve() # get the cache stored value of inverse
        if(!is.null(s_cache)) { # if already calculated we indicate so
                print("getting cached data")
                return(s_cache)
        }# otherwise we calculate inverse 
        data <- special_M$get()
        special_M$setsolve(data) # and store it within object
        special_M$getsolve()
}

