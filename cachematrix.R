# this function works like a class, it creates a list
# that contains 4 member functions: set, get, setinv
# and getinv. it uses <<- assignment operator so that
# these internal variables are not exposed to the
# outside environment. 



makeCacheMatrix <- function(x = matrix()) {

       I <- NULL								# this is where the result of inversion is stored, "I" stands for "stored inverse matrix" as NULL which is an empty identifier set up to contain the 												 #inversion.  
        set <- function(y) {					 # 'y' in this case is the numeric arg passed
                                   			  # into 'makeCacheMatrix'
        	
                x <<- y							# Set 'x' for the function enviromnent to 'y'
                I <<- NULL						# it initialises "I" to NULL value in the parent environment.
        }
        get <- function() x						# return the input matrix 
        setinv <- function(solve) I <<- solve   # setup the solve function for the inverse of the returned matrix
        getinv <- function() I					# return the inversed matrix "I"
        list(set = set, get = get, 				
             setinv = setinv,		
             getinv = getinv)
}        										## return a list that contains these 4 member functions, the workflow is like the following
     						    			  # makeCacheMatrix object 
     						  		       # x <- makeCacheMatrix(testmatrix)
    						     	            # x$set(newmatrix) # to change matrix
      					 		       # x$get # to get the set matrix
    								       # x$setInv # to set the inversed matrix
     						     		       # x$getInv # to get the inversed matrix




## Write a short comment describing this function


        cacheSolve <- function(x, ...) {
        I <- x$getinv()							## get the inversed matrix from object x.
        								  		# it will be null if uncalculated, referring to the first line "I <- NULL" in the first function
        if(!is.null(I)) {					    # if "I" is not NULL, which means if the inversion result exists
                message("getting cached data")
                return(I)						# then displays the message "Getting cached data" and return the calculated inversion
        }
        data <- x$get()							# otherwise, it runs x$get to get the matrix object
        I <- solve(data, ...)					# and solve it
        x$setinv(I)								# then it sets the result to the object "I"
        I										# print the result
}





