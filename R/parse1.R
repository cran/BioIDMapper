## parse1.R
## Function: parse data from NCBI
## non-access to user
## Jan. 29, 2008

`parse1` <-
function(uri)
{
    root_level<-"LinkSet"
    from_level_1<-"IdList"
    from_level_2<-"Id"
    to_level_0<-"LinkSetDb"
    to_level_1<-"Link"
    to_level_2<-"Id"

    parseXML<-xmlTreeParse(uri)
    myData<-parseXML$doc$children[[1]]
    childSet<-xmlChildren(myData)
    
    if(!is.null(childSet[[root_level]][[to_level_0]][[to_level_1]][[to_level_2]]))
    {
        from_col<-NULL
        to_col<-NULL
        for (i in 1: length(childSet))
        {
            from_id<-xmlValue(childSet[[i]][[from_level_1]][[from_level_2]])
            if (!is.null(childSet[[i]][[to_level_0]][[to_level_1]][[to_level_2]]) )
            {
                for (j in 3:length(xmlChildren(childSet[[i]][[to_level_0]])))
                {
                     to_id<-xmlValue(childSet[[i]][[to_level_0]][[j]][[to_level_2]])
                     from_col<-rbind(from_col, from_id)
                     to_col<-rbind(to_col, to_id)
                }
            }
        }
        return_matrix<-cbind(from_col, to_col)
    }
    else
    {
        return_matrix<-NULL
    }

    return(return_matrix)
}

