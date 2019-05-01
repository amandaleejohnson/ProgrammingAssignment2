#Programming Assignment 2: Lexical Scoping 

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
#         
#         makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#         cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#               If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#               retrieve the inverse from the cache.
# 
# Computing the inverse of a square matrix can be done with the solve function in R. 
# For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#This function creates a special "matrix" object that can cache its inverse.
#This should be similar to makeVector, but use an input matrix instead of a vector
makeCacheMatrix <- function(x = matrix()) {
        
        #Step 1 - initialize objects x and m
        #x is initialized as a function argument, default is an empty matrix
        #If no default is specified, you'll get an error message when data=x$get()
        
        m = NULL #m is set to NULL as an object within makeCacheMatrix
        
        #Step 2 - define "getters" and "setters"
        #set() - where the magic happens!    
        
        set = function(y) { #y is an argument and is assumed to be a numeric vector
                x <<- y #assign the input argument to x
                m <<- NULL #assign NULL to m, clears any previous value of m!
        }
        
        get = function() x #the value of x is pulled from the parent enviro makeCacheMatrix()
        
        setinverse = function(solve) m <<- solve #the value of m is pulled from the parent enviro
        #BUT we need to acceess it after setinverse() completes, the code uses
        #"<<-" to assign the input argument to m in the parent environment
        
        getinverse = function() m
        
        #Step 3 - create a new object by returning a list()
        #assigns each function as an element of list and returns it to the parent enviro
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        #The function returns a fully formed object of type makeCacheMatrix() to be used by R code.
        #each element in the list is NAMED, which allows us to use "$" to access functions by name
        #rather than "[["
}


#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#               If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#               retrieve the inverse from the cache.
#This should be similar to cachemean, but calculates the matrix inverse instead of mean
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        #USE THE "solve" function!
        
        m = x$getinverse() #get the inverse of matrix "x"
        
        #Checks to see if getinverse is NULL. 
        #makeCacheMatrix() sets the cached inverse to NULL
        #whenever a NEW VECTOR is set into the object
        
        #If the result of getinverse IS NOT NULL, that means we have a cached inverse!
        #and we can return the cached inverse to the parent environment:
        
        #If m is NOT NULL:
        if(!is.null(m)) { 
                message("getting cached data") 
                return(m) #pull the cached inverse and return it to the parent environment
        }
        #IF m IS NULL:
        data = x$get() #pull the vector from the input object
        m = solve(data, ...) #calculate the inverse, store it in m
        x$setinverse(m) #set the inverse IN the input object
        m #return the value of the inverse to the parent environment
}
        

