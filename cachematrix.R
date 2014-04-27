## a set of functions to compute the inverse of a matrix 
## in an efficient way using caching making use of the <<-
## operator


## create a cacheable matrix object
makeCacheMatrix <- function(x = matrix()) {
  # 
  i <- NULL
  # additional variable to reflect changes of the matrix
  changed <- FALSE
  # set a matrix 
  set <- function(y) {
    x <<- y
    i <<- NULL
    changed <<- TRUE
  }
  # get a matrix 
  get <- function() x
  # set the inverse of a matrix 
  setInverse <- function(inverse) {
    i <<- inverse
    changed <<- FALSE
  }
  # get the inverse of a matrix
  getInverse <- function() i
  # get boolean variable changed 
  getChanged <- function() changed
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse, getChanged = getChanged)
}



## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x) { 
  i <- x$getInverse()
  # check if x has been changed in the mean time
  if(!x$getChanged()) {
    message("getting cached data")
    return(i)
  }
  ## retrieve the matrix
  data <- x$get()
  ## using R's solve function to compute the inverse of a matrix
  i <- solve(data)
  ## set the inverse to preserve it
  x$setInverse(i)
  i
}

# A test case follows which was executed to convince me that the code
# is correct. I have found the material in the forums and have extended
# it on my own. I attach a trace of the test case execution to convince the
# reviewer, too. THis trace is conatined in trace.txt

##########################################################################
## Test case to check cached inverse matrix
##########################################################################
##
## PREPARE ##########################
##provide a 3x3 matrix
m <- matrix(c(1,2,3,6,0,4,7,8,9),3,3)
#
#create a "cache"
cache <- makeCacheMatrix()
#
#set the matrix value of the cache
cache$set(m)
##
## CROSSCHECK #######################
##
#crosscheck 1: same matrix in cache than m?
mCache<-cache$get()
if(!identical(m, mCache)) {
  stop("Matrix is not the same")
}
#
#crosscheck 2: at this point the cached inverse must be null
iCache<-cache$getInverse()
if(!is.null(iCache)) {
  stop("Inverse must be null")
}
##
## CHECK SOLVE ###########################
##
# now solve first time
s1 <- cacheSolve(cache)
#
#solve second time
s2 <- cacheSolve(cache)
#
#check: s1 and s2 should be identical
if(!identical(s1,s2)) {
  stop("Both inverse computations must be the same")
}
#
#now cacheHits should be 1
##provide another 3x3 matrix
mNew <- matrix(c(1,2,3,6,0,4,7,6,8),3,3)
data <- cache$set(mNew)
# NULL is expected here
cache$getInverse()
# TRUE is expected here
cache$getChanged()
s1 <- cacheSolve(cache)
cache$getChanged()
s2 <- cacheSolve(cache)
if(!identical(s1,s2)) {
  stop("Both inverse computations must be the same")
}
