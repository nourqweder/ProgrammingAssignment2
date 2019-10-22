## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function



makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

m <- function(x = matrix()) {
    #1 Initialize cached inverse matrix
    inv <- NULL
     #flag to check if matrix has updated or not
    updated <- TRUE
    
    # Set a matrix in the data structure
    set_m <- function(x) {
        m <<- x
        #init status for matrix
        mat_inv <<- NULL
        #flag to check if matrix has updated or not
        updated <<- TRUE
    }
    
    # Get the currently set matrix
    get_m <- function() m
    
    # Set calculate the inverse matrix, 
    # if matrix has not updated -> false
    set_inverse_matrix <- function(inverse) {
        updated <<- FALSE
        mat_inv <<- inverse
    }


## cache inverse m
 #  inverse matrix
    get_inverse_matrix <- function() inv
    
    #detected updated 
    is_updated <- function() updated
    #all functions
    list(set_m = set_m, 
         get_m = get_m,
         set_inverse_matrix = set_inverse_matrix,
         get_inverse_matrix = inverse_matrix,
         is_updated = is_updated)
}


## cacheSolve function calculate the inverse "matrix" returned by m from previous func
#if inverse is available in chch but matrix has not upddated so it retrived from cach

cacheSolve <- function(my_matrix, ...) {
    # cached inverse matrix
    inv <- my_matrix$get_inverse_matrix()
    #check if matrix has been updated 

    updated <- my_matrix$is_updated()
    
    m <- my_matrix$get_m()
    
    # Calculate the inverse of matrix
    inv <- solve(m, ...)
    # Set the calculated matrix that's inversed
    my_matrix$set_inverse_matrix(inv)
    
    message(" inverse of matrix is DONE!")
    inv
}
