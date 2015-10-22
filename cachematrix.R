> ## Assignment: Caching the Inverse of a Matrix
> ## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse 
> 
> makeCacheMatrix <- function(x = matrix()) {
+   mat <- NULL #Null matrix 
+   
+   get <- function() x  # get function returns x - input matrix object 
+   setsmat <- function(solmat) mat <<- solmat # setsmat function assigns solved matrix to mat 
+   getsmat <- function() mat # getsmat function returns mat - solved matrix 
+   
+   ##makeCacheMatrix returning list object of above defined functions  
+   list( get = get, setsmat = setsmat, getsmat = getsmat) 
+ }
> 
> ## Inverse of matrix returned by makeCacheMatrix
> cacheSolve <- function(x, ...) {
+   mat <- x$getsmat() 
+   if(!is.null(mat)) {
+     message("getting cached data")
+     return(mat)
+   }
+   data <- x$get()
+   smat <- solve(data, ...)
+   x$setsmat(smat)
+   smat
+ }
> 
> 
> ## Call makeCacheMatrix function by passing invertible matrix and aasign to IM
> IM <- makeCacheMatrix ( matrix(c(4,3,2,4) , 2,2 ) )
> 
> ## First run of cacheSolve: Call cacheSolve function by passing IM as input 
> cacheSolve (IM)
     [,1] [,2]
[1,]  0.4 -0.2
[2,] -0.3  0.4
> 
> ## In the first run cacheSolve have input as IM list as object from function makeCacheMatrix
> ## mat <- x$getsmat() : returns mat NULL at this point no value in mat    
> ## R pointer evaluates if condition which is FALSE and jumps to next 
> ## data <- x$get() : returns "x" which is matrix as input value
> ## smat <- solve(data, ...) : get the inverse of matrix and store solved matrix in to smat 
> ## x$setsmat(smat) : smat value passed to function setsmat returns with an assignment statement
> ## mat <<- smat. Now inside smat we have solved matrix
> ## In the end cacheSolve returns a object smat which is solved matrix 
> 
> ## Second run of cacheSolve with same input as in first run 
> cacheSolve (IM)
getting cached data
     [,1] [,2]
[1,]  0.4 -0.2
[2,] -0.3  0.4
> 
> ## Second run : we have same input as first run IM list as object from function makeCacheMatrix
> ## now - mat <- x$getsmat() : expression returns mat with solved matrix value in it (lexical scoping)
> ## ****we had set during first run using x$setsmat(smat)***
> ## R pointer evaluates if condition which is TRUE, which prints message "getting cached data"
> ## in the next step cacheSolve function returns mat solved matrix object in to console 
> ## and exits due to return() function call
