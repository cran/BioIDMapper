## parse2.R
## Function: parse query result from UniProt
## non-access to user
## Jan.29, 2008
## Feb., 2009 Update
`trans.parse2`<-function(onerow)
{
    f<-NULL
    t<-NULL
    oneresult<-list(from=NA, to=NA)
    if (strsplit(onerow[[2]], "\r") == "")
    {
        return(oneresult)
    }

    len<-strsplit(onerow[[2]], ";")[[1]]
    for ( i in 1:length(len))
    {
       if (len[i] != "\r")
       {
          t<-rbind(t, len[i])
          f<-rbind(f, onerow[[1]])  
       }
       else
       {
          break
       }
        
    }
    oneresult$from<-f
    oneresult$to<-t
    return(oneresult)
}

`parse2` <-
function(returnHtml)
{   
    from_col<-NULL
    to_col<-NULL
    returnHtml<-gsub(" ", "", returnHtml)
    totalIDPair<-noquote(strsplit(returnHtml, "\n")[[1]])
    for (i in 1: length(totalIDPair))
    {
        if (strsplit(totalIDPair[[i]], "\r") == "")
        {
            next
        }
        
        tmp<-strsplit(totalIDPair[[i]],"\t")[[1]]
        if (length(tmp) == 1)
        {
            next
        }
        
        part.row<-trans.parse2(noquote(tmp))
       
        if (is.na(part.row$from[1]))
        {
            next
        }
        from_col<-rbind(from_col, part.row$from)
        to_col<-rbind(to_col, part.row$to)
    }

    # combine
    if (length(from_col) == 0)
    {
        return(-1)
    }
    if (is.na(from_col[1]))
    {
        return(-1)
    }
    final_matrix<-cbind(from_col, to_col)
    
    return(final_matrix)
}

