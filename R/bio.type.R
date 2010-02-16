## bio.type.R
## Function: display mapping schema for BioIDMapper
## Jan. 29, 2008

`bio.type` <-
function(type_to_id)
{
  id2term<-.exp$gettype()
  if (missing(type_to_id))
  {

     detail<-data.frame(id2term[,2])
     sepa<-which(detail=="NNNNNN")
     rname<-rep(c("NCBI", "Boundary", "UniProt"), c(sepa-1, 1, nrow(detail)-sepa))
     detail<-cbind(seq(1, nrow(detail)),detail,rname)
     colnames(detail)<-c("Biokey number", "BioIDs", "Sources")
     return(data.frame(detail))

  }

  if (is.numeric(type_to_id)) return(id2term[type_to_id,2])
  if (is.character(type_to_id)) return(which(id2term[,2]==type_to_id))

  return(processMessage("It is not in the id list."))

}

