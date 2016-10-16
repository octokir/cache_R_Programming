makeCacheMatrix <- function(x=matrix()){
	m <- x
	m_i <- NULL
	
	set_matrix <- function(amatrix) m <<- amatrix # this might not be needed, really depending on how the problem is interpreted. i believe the teachers instruction clears it up but check 													#w/ the solution first. Almost the same as set_inverse_matrix.  
	
	get_matrix <- function()
	{
		m_i
	}
	
	get_inverse_matrix <- function() m_i
	{
		if(m_i != NULL){
			get_matrix()
		}else{
			set_inverse_matrix()
		}
	} 
	
	set_inverse_matrix <- function() 
	{
		if(m_i == NULL)
		{
			m_i <- solve(m)
			
		}	
	}
	
	cache_inverse_matrix <- function(){
		if(m_i != NULL)
		{
			m_i <- set_inverse_matrix()	
		}
	} 

	list(get_matrix= get_matrix, set_matrix=set_matrix, get_inverse_matrix=get_inverse_matrix, set_inverse_matrix=set_inverse_matrix)
}

cacheSolve <- function(x, ...){
	#This one takes the matrix from the machachematrix and computes its inverse. if the inverse has already been calculated, than it retrieves it from the cache. 
	m <- x$get_matrix()
	
	if(!is.null(m)){
		m <- x$get_matrix()
		return(m)
	}else
		x$set_inverse_matrix()
		
}