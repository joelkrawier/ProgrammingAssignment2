## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#' Title
#'
#' 
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<- function(mat) inv <<- mat
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function


#' Title
#'
#' la fonction cacheSolve va utiliser le resultat 
#' de makecacheMatrix pour calculer l'inverse de la matrice contenue 
#' dans la liste de résultat de makecacheMatrix
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}