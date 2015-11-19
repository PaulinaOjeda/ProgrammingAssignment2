## This function returns a list object which support to store a inverted matrix as cache. 2 objects are saved
## The matrix and the inverted matrix. This support to be initilized on first call of after using $set
## If original matrix is modified ($set) the inverted matrix will be cleaned to force to recalculate it

makeCacheMatrix <- function(x = matrix()) {

  i<-NULL #assign NULL to i. this variable will hold the inverted matrix
  
  #this function, allows save the original matrix (non inverted matrix)
  set <- function(y){
  
   #When original matrix is uptdated, the cache is cleaned, and inverted matrix in removed
   x <<-y     # the original matrix is saved in x variable
   i <<- NULL # The i variable, which hold the inverted matrix, is cleaned, so the cached matrix is removed 
 }
  ## define a get function, which returns the orignal matrix
  get <- function() x
  
  #this function allows save/replace the inverted matrix
  setinverse <-function(inve) i<<- inve
  
  ## this function returns the inverted matrix already saved.
  getinverse <-function() i
  
  ## this list is returned, which is made of 4 functions, set, get, setinverse and getinverse
  list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## cacheSolve returns the inverse of matrix x (given as parameter)
## but tries get an inverse matrix from cache of x before calculate it
## if there is not a cached inverted matrix (x$getinverse) the inverted will be calculated and it will be set 
## into cache
cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  li <-x$getinverse()

  ## Check if already exist an inverted matrix.
  if(!is.null(li)) 
    {
    message("getting cached inverted matrix")
    return(li) ## If li is not null, return it and exit from cacheSolve function
   }
  
  ## if li is null, previous return will not be executed, so we continue here
  ## There is not an inverted matrix cached, so we need to calculate and save it
  
  ## 1) Get the orginal matrix
  data <-x$get()
  
  
  ## 2) Returns its inverse and save it into li variable
  li <- solve(data,...)
  
  ## 3) We call to setinverse of x to save their inverted matrix, passing li as the inverted matrix
  x$setinverse(li)
  
  
  ## 4 and finally, we return li, which is the inverted matrix
  li
  }

  
  