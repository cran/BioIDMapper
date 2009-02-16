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
    
    #if(!is.null(childSet[[root_level]][[to_level_0]][[to_level_1]][[to_level_2]]))
    #{
    if ( length(grep("^Link$", unlist(childSet))) > 0 )
    {
        from_col <- NULL
        to_col <- NULL
        for (i in 1: length(childSet))
        {
            from_id <- xmlValue(childSet[[i]][[from_level_1]][[from_level_2]])
            #if (!is.null(childSet[[i]][[to_level_0]][[to_level_1]][[to_level_2]]) )
            #{
            if (  length(grep("^Link$", unlist(childSet[[i]]))) > 0 )
            {

               myset <- unlist(childSet[[i]])
               idPos <- grep("^Id$", myset)
               idPos <- idPos[-1] # get rid of "from" ID
               matchPos <- idPos + 2
               matchID <- myset[matchPos]
               names(matchID) <- NULL
               to_col <- c(to_col, matchID)
               from_col <- c(from_col, rep(from_id, length(matchID)))
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

