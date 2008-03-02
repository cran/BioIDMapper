## bio.select.R
## Function: data analysis module - choose selected id from result matrix
## Jan 29, 2008

`bio.select` <-
function(result_matrix, colno, myid)
{
    if (any(missing(result_matrix), missing(colno), missing(myid)))
    {
        processMessage("Usage:   bio.select(result matrix, col_no, myid)")
        processMessage("         return matrix: the result from \"bio.convert\" function")
        processMessage("         col_no: \"myid\" column numer in the \"result_matrix\" ")
        return(processMessage("Done."))    
    }

    if(!is.matrix(result_matrix))
    {
        result_matrix<-as.matrix(result_matrix)
    }
    
    return(subset(result_matrix,result_matrix[,colno]==myid))
 
}

