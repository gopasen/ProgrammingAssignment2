## The functiona below cab be used to compute, cache and return the  inverse of a matrix. If the inverse of a matrix has already been computed earlier, then it retrieves the inverse from the cache.



## makeCacheMatrix function creates a special "matrix" object that contains functions to cache and retrive inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
		inverseM <- matrix()
		
		## setmatrix function saves new matrix value from y argument and resets the previous inverse matrix to NULL 
		
		setMatrix <- function(y) {
				x <<- y
				inverseM <<- NULL
		}
		
		
		## Gets the matrix value 
		
		getMatrix <- function() {
			x
		} 
		
		
		##Sets the inverse matrix value to argument value of ïnvmat"
		
		setInvMatrix <- function(invmat){
			inverseM <<- invmat
		}
		
		## Retireves the inverse matrix value
		
		getInvMatrix <- function(){
			inverseM 
		}



		## Returns list of functions for caching and retrieving Matrix and Inverse matrix value
		
		mf <- list(setM=setMatrix, getM=getMatrix, setIM=setInvMatrix, getIM=getInvMatrix)
		
		return(mf)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
	
        ## Return an inverse matrix if availble in cache
        im <- x$getIM()
        if(!all(is.na(im))){
        	message("getting cached inverse matrix")
        	return(im)
        }
        
        ## Inverse matrix not found in cache, hence computes the inverse matrix and stores in cache
        matrix <- x$getM()
        im <- solve(matrix, ...)
        x$setIM(im)
        return(im)   
         
}
