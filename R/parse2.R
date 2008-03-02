## parse2.R
## Function: parse query result from UniProt
## non-access to user
## Jan.29, 2008

`parse2` <-
function(returnHtml)
{  
    totalIDPair<-noquote(strsplit(returnHtml, "\n")[[1]])

    if (length(totalIDPair)>=1)
    {
        from_col<-noquote(strsplit(totalIDPair[[1]],"\t")[[1]])[[1]]
        to_col<-noquote(strsplit(totalIDPair[[1]],"\t")[[1]])[[2]]
    }
    else
    {
        return(-1)
    }
    if (length(totalIDPair)>=2)
    { 
        for (i in 2:length(totalIDPair))
        {
            from_col<-rbind(from_col, noquote(strsplit(totalIDPair[[i]],"\t")[[1]])[[1]])
            to_col<-rbind(to_col, noquote(strsplit(totalIDPair[[i]],"\t")[[1]])[[2]])    
        }
       
    }

    # combine
    final_matrix<-cbind(from_col, to_col)
    
    return(final_matrix)
}

