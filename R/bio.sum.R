## bio.sum.R
## Function: data analysis module - summarize mapping results
## Jan 29, 2008


`bio.sum` <-
function(final_matrix, start_matrix, option)
{
    if (missing(final_matrix))
    {
        processMessage("Usage:   bio.sum(return matrix)")
        processMessage("         return matrix: the result from \"bio.convert\" function")
        processMessage("Example: bio.convert(c(\"6456604\", \"23396823\"), 1, 5)->myMatrix")
        processMessage("         bio.sum(myMatrix)")
        return(processMessage("Done."))    
    }
     
    if (!is.matrix(final_matrix))
    {
        final_matrix<-as.matrix(final_matrix)
    }
    
    if (nrow(final_matrix)==1 || ncol(final_matrix)==1)
    {
        processMessage("Only one row/column, no need to summary.")
        return(NULL)
    }
    
    ## summary
    colseq<-seq(1,ncol(final_matrix))
    result<-matrix(seq(1:ncol(final_matrix)),nrow=1)
   
    mappingNo<-sapply(colseq, function(colseq)
                          { 
                            final_matrix[,colseq]->dtemp
                            dtemp[!is.na(dtemp)]->dtemp
                            dtemp<-unique(dtemp)
                            result[,colseq]<-length(dtemp)
                          }
                     )
   
    result<-matrix(mappingNo, nrow=1)
   
    colnames(result)<-colnames(final_matrix)
    rownames(result)<-c("maping_Total")

    ## more summary
    if (!missing(start_matrix))
    {       
        total<-nrow(start_matrix)  
        
        
        final_col<-seq(1, ncol(final_matrix))
        tlength<-seq(1, ncol(final_matrix))
        tlength<-sapply(final_col, function(final_col)
                          {
                              ptemp<-final_matrix[,final_col]
                              ptemp<-ptemp[!is.na(ptemp)]
                              tlength[final_col]<-length(unique(ptemp))   
                          }
                        )
      
        percent<-sapply(final_col, function(final_col)      
                            { 

                              if (final_col==1)
                              {
                                sprintf("NNNNNN")
                              }
                              else
                              { 
                                ptemp<-final_matrix[,(final_col-1):final_col]
                                ptemp<-ptemp[!is.na(ptemp[,2]),]
                                sprintf("%.2f%s", length(unique(ptemp[,1]))/tlength[final_col-1]*100, "%")
                              }
                          }
                     ) 

        result_withp<-rbind(result, percent)
        rownames(result_withp)<-c("mapping_Total", "mapping_Percentage")     
    }
    
    final_col<-seq(1,ncol(final_matrix))  
    myList<-list()
    
    if (!missing(option))
    {
        if (is.logical(option))
        {
          if (option)
          {
              for(i in 1:ncol(final_matrix))
              {
                 myList[[i]]<-table(final_matrix[,i])
              }
              
              names(myList)<-paste("Unique Mapping for ", colnames(final_matrix), sep="")
          }
        }
        else
          processMessage("The option is not correct logical character. Please check manual.")
        
    }         
    if (!missing(start_matrix))
    {
        myList[[length(myList)+1]]<-result_withp                 
    }
    else
    {
        myList[[length(myList)+1]]<-result
    }
    names(myList)[length(myList)]<-"Summary for result"
    
    #output mapping schema
    cat("------------------------------------------------------------------\n\n")
    cat("MAPPING SCHEMA:\n")
    for (i in 1:(ncol(result)-1))
    {
        cat(result[,i], colnames(result)[i]," are mapped to ",result[,i+1], colnames(result)[i+1], "\n") 
    }

    if (!missing(start_matrix))
    {
        cat("\nMAPPING PERCENTAGE:\n")
        percent<-matrix(percent, nrow=1)
        
        for (i in 2: ncol(percent))
        {
            cat(percent[,i], colnames(result)[i-1], " are mapped to ", colnames(result)[i], "\n")
        }
    }
    
    cat("------------------------------------------------------------------\n\n")
    
    return(myList);
}


