## @netmanchris code for Coursera Intro to R course. Jan 2015
##Assignment to understand lexical scoping. How to move and access data between
##differnt environments within R. See Other resources for understanding Lexical Scoping
##document
## 

## makeCacheMatrix
##This function will take the input of a matrix stored in variable X, compute the inverse
##of the matrix and store it in variable m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  

}


## cacheSolve This function will first check to make sure that the contents of m are
##not null ( !isnull(m)). If they are not null, the function will retrieve the inverse
##of the matrix which was cached in the variable m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



#Tests taken directly from https://class.coursera.org/rprog-010/forum/thread?thread_id=288


## first script: test cacheSolve repeatedly
x <- matrix(rnorm(160000),400,400)   #creates matrix
x_ <- makeCacheMatrix(x)      #caches inverse of matrix
for (i in 1:1000) {         #runs the cacheSolve function 1000 times 
  x__ <- cacheSolve(x_)
}

## second script: test solve repeatedly
###This takes much longer since m has not been cached.

x <- matrix(rnorm(160000),400,400)   #creates matrix
for (i in 1:1000) {            #runs the solve function 1000 time. 
  x_ <- solve(x)
}

## third script: verify equality
x <- matrix(rnorm(160000),400,400)  #creates matrix
x_ <- makeCacheMatrix(x)            #caches inverse of matrix
cacheSolve(x_)                      #runs solve function
print(identical(x_$getinverse(),solve(x)))     #prints "TRUE" if identical
