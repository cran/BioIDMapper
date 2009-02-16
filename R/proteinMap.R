## proteinMap.R
## Function: mapping between protein ids
## non-access to user
## Jan.29, 2008

`proteinMap` <-
function(my_list, from_type, to_type)    # my_list is matrix
{
  pPara<-.exp$getpPara()    
  
  rootURL<-pPara[which(pPara=="rootURL")+1]

  from_sign<-pPara[which(pPara=="from_sign")+1]
  to_sign<-pPara[which(pPara=="to_sign")+1]
  id_sign<-pPara[which(pPara=="id_sign")+1]
  other_para<-pPara[which(pPara=="other_para")+1]
  nolimit<-as.numeric(pPara[which(pPara=="queryLimit")+1])
  
  from<-paste(from_sign, from_type, sep="")
  to<-paste(to_sign, to_type, sep="")
  
  linepattern <- "\n"
  tabpattern<-"\t"

  time<-ceiling(length(my_list)/nolimit)  
  final_matrix<-NULL
  for (j in 1:time)
  {  
      #Uniprot requirement
      if (j>1) 
      {
          Sys.sleep(3)
      }
          
      if (time==1)
      {
          ui_list<-my_list
      }
      else 
      {
          if (j==time)
          {
             ui_list<-my_list[((j-1)*nolimit+1):length(my_list)] 
          }
          else 
          {
              ui_list<-my_list[((j-1)*nolimit+1):(j*nolimit)]
          }
      }  

      ui_all<-paste(ui_list, sep="", collapse="+")
      id<-paste(id_sign, ui_all, sep="")

      link<-paste(from, to, sep="")
      id<-paste(link,id, sep="")
      
      rootURL<-paste(rootURL, id, sep="")
     
      ## need to break it when rootURL > 8000
      ## pairs separated
      rootURL<-paste(rootURL, other_para, sep="")
      returnResult<-try(getURL(rootURL))  
      if (inherits(returnResult, "try-error"))           
          stop("\n-------------------------\nPlease check internet connection!\n-------------------------\n\n")

      section_matrix<- parse2(returnResult)
 
      if(is.matrix(section_matrix))
      {
          final_matrix<-rbind(final_matrix, section_matrix)
      }     
      processMessage(paste(length(ui_list), " IDs have been processed")) 
      
      # return back to original URL, and then loop to add more IDs
      rootURL<-pPara[which(pPara=="rootURL")+1]     
  }
  
  if (is.null(final_matrix))
  {
      processMessage(" No ID found in database. 0 IDs have been processed")
  }
  else
  {
      ## parse result
      colnames(final_matrix)<-c(from_type, to_type)  
      if (nrow(final_matrix) >= 1)
      {
          rownames(final_matrix)<-c(1:nrow(final_matrix))
      }
      
  }

  return(final_matrix)
}

