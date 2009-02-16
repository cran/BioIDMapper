## geneMap.R
## Function: mapping in "gene" level
## non-access to user
## Jan. 29, 2008

`geneMap` <-
function(my_list, from_type, to_type)   # my_list --> matrix
{
  gPara<-.exp$getgPara()
 
  rootURL<-gPara[which(gPara=="rootURL")+1]
  from_sign<-gPara[which(gPara=="from_sign")+1]
  to_sign<-gPara[which(gPara=="to_sign")+1]
  id_sign<-gPara[which(gPara=="id_sign")+1]
  nolimit<-as.numeric(gPara[which(gPara=="queryLimit")+1])
  tool<-gPara[which(gPara=="other_para")+1]
  
  from<-paste(from_sign, from_type, sep="")
  to<-paste(to_sign, to_type, sep="")
  
  rootURL<-paste(rootURL, from, sep="")
  rootURL<-paste(rootURL, to, sep="")
  rootURL<-paste(rootURL, tool, sep="")

  time<-ceiling(length(my_list)/nolimit)
  final_matrix<-NULL
  for (j in 1:time)
  {
      #NCBI requirement
      if (j>1) 
      {
          Sys.sleep(3)
      }
          
      if (time==1)
      {
          part_list<-my_list
      }
      else 
      {
          if (j==time)
          {
             part_list<-my_list[((j-1)*nolimit+1):length(my_list)] 
          }
          else 
          {
              part_list<-my_list[((j-1)*nolimit+1):(j*nolimit)]
          }
      }

      #collapse all ids to one string
      all_list<-paste(id_sign, part_list, sep = "", collapse="")
       
      startURL<-paste(rootURL, all_list, sep="") 
  
      uri<-try(getURL(startURL))
      if (inherits(uri, "try-error"))
          stop("\n-------------------------\nPlease check internet connection!\n-------------------------\n\n")
      
      section_matrix<- parse1(uri)
     
      if(!is.null(section_matrix))
      {  
          final_matrix<-rbind(final_matrix, section_matrix)
      }
      processMessage(paste(length(part_list), " IDs have been processed"))
  }

  if (is.null(final_matrix))
  {
      processMessage(" No ID found in database. 0 IDs have been processed")
  }
  else
  {
      colnames(final_matrix)<-c(from_type,to_type)
      if (nrow(final_matrix) >= 1)
      {
          rownames(final_matrix)<-c(1:nrow(final_matrix))
      }
  }
  return(final_matrix)
}

