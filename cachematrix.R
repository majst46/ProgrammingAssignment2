##     The function makeCacheMatrix takes an object x and, so long as that object is a square 
##   matrix or a single number, returns a list with two elements: 
##      + The matrix x itself (or a one by one matrix containing x, if x was a single number)
##      + A null object where the inverse may be inserted. 

makeCacheMatrix <- function(x = matrix()) {

        if(!is.matrix(x) & (!is.numeric(x) | length(x) != 1)){
                message("Error: object input to makeCacheMatrix is not a matrix") 
                return(x)
        }

        if(is.matrix(x)){ 
                if(dim(x)[1] != dim(x)[2]){
                        message("Error: matrix input to makeCacheMatrix is not square") 
                        return(x)
                }
        }

        if(!is.matrix(x) & is.numeric(x) & length(x) == 1){
                message("Warning: Number coerced to 1 by 1 matrix") 
                x<-matrix(x)
        }

        i <- NULL

        list(matrix=x, inverse = i)
}



##     The function cacheSolve takes an object of the form output by makeCacheMatrix.  It returns 
##   errors if the input object has the incorrect form, or the form returned by makeCacheMatrix when
##   it does not receive a square matrix.  
##     It checks the second component of the list; if this is not null, then the function does nothing. 
##   Otherwise, it checks to see if the input matrix is invertible, and if it is, it write the inverse of 
##   that matrix into the second element of the list.  It does not repeat the checks made by 
##   makeCacheMatrix to ensure that the first element of the input object is indeed a square matrix.


cacheSolve <- function(x){

        if(is.list(x)){
                if(length(x)!=2 | !is.matrix(x[[1]])){
                        message("Error: List input to cacheSolve has the wrong form")
                        return(x)
                }
        }else{
                message("Error: Object input to cacheSolve is not a list")
                return(x)
        }

        if(!is.null(x[[2]])){
                message("Using cached value of inverse")
                return(x)
        }else{
                if(det(x[[1]])==0){
                        message("Warning: Matrix is not invertible")
                        x[[2]]<-"Inverse does not exist"
                 }else{
                         x[[2]]<-solve(x[[1]])

                 }
         }

         return(x)
}


