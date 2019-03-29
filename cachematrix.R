## The following function will creates a matrix object
## that can cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The code below computes the inverse of matrix returned
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
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


#### Testing the functions above 

Test <- matrix(c(1,2,3,4),2,2)

T1 <- makeCacheMatrix(Test)

cacheSolve(T1)

#### Solution
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

cacheSolve(T1)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5





