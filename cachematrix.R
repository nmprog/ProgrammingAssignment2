


## The two functions, makeCacheMatrix and cacheSolve, take an invertible matrix named mat, 
## solve for the inverse of mat (invmat) and then maintain the values associated with mat and invmat
## in a cache. Subsequent calls for the value of the inverse of the matrix mat then simply 
## return the cached value without the necessity of repeating the full calculation of that inverse.



## The function makeCacheMatrix takes as its argument an invertible matrix, mat. It generates four functions,
## set, get, setinvmat, getinvmat and returns them in a list.
  
## Each of the four functions is associated with the argument mat.  The created function "set" uses the deep 
## assignment operator <<- to place both mat and invmat in an environment outside the function environment so
## that the values of mat and invmat persist and can be read after creating function is executed.
 
## The created "get" function returns the value of the matrix mat. The created function "setinvmat" sets the 
## value of the inverse of mat in invmat replacing the value NULL established by the function "set". The created
## function "getinvmat" returns the current value of invmat.  



makeCacheMatrix <- function(mat = matrix(), ...) {
        invmat <- NULL
        
        # Establish pointers to the values of mat and invmat outside the function environment
        # using the superassignment operator "<<-"
        set <- function(y) {
                mat <<- y
                invmat <<- NULL
        }
        
        get <- function() {mat}
        setinvmat <- function(inverse)  {invmat <<- inverse}
        getinvmat <- function()  {invmat}
        
        # Associate list names with the function names
        list(set = set, get = get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)         
}


## The function "cacheSolve" operates on the output from the function "makeCacheMatrix" which output can be
## thought of as data (a matrix) with functions.
## Each of the functions created by  makeCacheMatrix can be invoked using the $ operator e.g. x$setinvmat()
## The function"cacheSolve" first invokes the "getinvmat" function to retrieve the cached value of invmat.
## The variable invmat is tested to see if it is NULL or has been assigned a value.  If invmat is NULL, "cacheSolve" computes
## the inverse of the matrix passed as an argument from "makeCacheMatrix".  Otherwise, it  returns the cached value 
## of the inverse invmat without having to use the solve() function.


cacheSolve <- function(x = matrix(), ...) {
        invmat<- x$getinvmat()
        # Test to see if the inverse of x has already been computed
        # If invmat has not been computed, do so using solve()
        if(!is.null(invmat)) {
                message("getting cached data")
                return(invmat)
        }
        data <- x$get()
        invmat <- solve(data)
        x$setinvmat(invmat)
        invmat
}
                






       
 