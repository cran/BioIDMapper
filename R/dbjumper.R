## dbjumper.R
## Function: mapping between different gene level
## non-access to user
## Jan. 29, 2008

`dbjumper` <-
function(id_list, from, to, pathMatrix)
{ 
  pathway<-as.matrix(pathMatrix)
  
  id_list<-validate(id_list)
  
  if (nrow(pathway)<2)
  {
      stop("Need to update the configuration file!") 
  }

  ## setup para
  from_type<-from
  to_type<-to
  merge_matrix<-NULL

  for (i in 1:nrow(pathway))
  {     
      if (i==1)
      {
          from_type<-from
          to_type<-pathway[i,3]
      }
      else 
      {   
          if (i==nrow(pathway))
          {
              to_type<-to
          }
          else
          {
              to_type<-pathway[i,3]
          }
          from_type<-pathway[i,2]
      }
  
      if (from_type != to_type )
      {    
          if (pathway[i,1]=="g")
          {
              processMessage("Parsing data from NCBI")
              temp_matrix<-geneMap(id_list, from_type, to_type)
              temp_matrix[which(temp_matrix=="no_match")]<-NA
          }
          else
          {
              if (pathway[i,1]=="p")
              {  
                  processMessage("Parsing data from UniProt")
                  temp_matrix<-proteinMap(id_list, from_type, to_type)
                  
                  temp_matrix[which(temp_matrix=="no_match")]<-NA
              }
              else
              {
                  ## for function
              }
              
          }
      
          ## if all are no_match
          if (length(temp_matrix)!=0 && any(!is.na(temp_matrix[,2])))
          {
              
            if (is.null(merge_matrix))
              {
                  merge_matrix<-temp_matrix
              }
              else
              {    
                  mycol<-c(colnames(merge_matrix),colnames(temp_matrix)[2]) 
                  merge_matrix<-merge(merge_matrix, temp_matrix, by.x = pathway[i-1,3], by.y = pathway[i,2], all = TRUE, sort=F) 
                
                  merge_matrix<-merge_matrix[mycol]           
              }
              
              ## update id_list
              id_list<-temp_matrix[,2]
              id_list<-validate(id_list)
          }
          else
          {
              if (is.null(merge_matrix))
              {
                  merge_matrix<-temp_matrix
              }
              
              if (length(merge_matrix)!=0)
              {
                  null_to_col<-as.matrix(rep(NA, nrow(merge_matrix)), ncol=1)
                  colnames(null_to_col)<-to
                  merge_matrix<-cbind(merge_matrix, null_to_col)
              }
              return(merge_matrix)
              
          }
      }      
  }
  return(merge_matrix)
}

