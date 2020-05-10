## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix())
{     
  inv<-NULL
  set<-function(y)
  {
                  
    x <<- y                           
    inv <<- NULL                    
  }
  get <- function() x               
  set_i<-function(inverse)inv<<-inverse
  get_i<-function()inv

  list(set = set, get = get, set_i = set_i, get_i = get_i)   
}

  #calculate inverse of special metrix return by cache natrix 
  #else retreive inverse fron cache
 cachesolve<-function(x,...)
 {
   ##return a matrix that is the inverse of "x"
   inv<-x$get_i()
   if (!is.null(inv)){
     massage("getting catch data")
  
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set_i(inv)
  inv
}
        

