## validate.R
## Function: return unique ids with "matrix" format
## non-access to user
## Jan.29, 2008

`validate` <-
function(id_list)
{   
    id_list<-as.matrix(id_list)
    id_list<-unique(id_list)
    id_list<-id_list[!is.na(id_list)]
    
    return(id_list)
}

