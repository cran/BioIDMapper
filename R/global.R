## zzz.R
## Function: environment setup
## non-access to user
## Jan. 29, 2008

myFile <- function(...)
{
	system.file(..., package= "BioIDMapper")
}


.exp<-local({  
  
  #biourl<-"http://www.public.iastate.edu/~sunx1/BioIDMapper.xml"
  #allPara<-updateTerm(biourl)
  
  allPara<-updateTerm(myFile("setup","BioIDMapper.xml"))
  
  .id2term<-as.matrix(allPara$term[,1])
  .type<-as.matrix(allPara$term[,c(1,2)])
  .link2site<-as.matrix(allPara$term[,3])
  .keymap<-as.matrix(allPara$key_map)
  
  ## read in gene para
  Para<-as.matrix(allPara$para)
  pgSep<- which(Para=="proteinMap")
  pfSep<-which(Para=="functionMap")
  .gPara<-Para[1:(pgSep-1)]
  .pPara<-Para[(pgSep+1): (pfSep-1)]
  
  list(
    getallPara = function() allPara,
    getid2term = function() .id2term,
    gettype = function() .type,
    getlink2site = function() .link2site,
    getkeyMap= function() .keymap,
    getgPara =  function() .gPara, 
    getpPara =  function() .pPara   
  )

})



