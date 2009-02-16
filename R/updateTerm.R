## updateTerm.R
## Function: when loading package, automatically get parameters from author's webpage
## non-access to user
## Jan.29, 2008

`updateTerm` <-
function(biourl)
{
  require(XML)
  require(RCurl)
   if(missing(biourl) || is.null(biourl))
   {
      biourl<-"http://www.public.iastate.edu/~sunx1/BioIDMapper.xml"
      returnDoc<-try(getURL(biourl))
      if (inherits(returnDoc, "try-error"))  
      { 
       stop("\n-------------------------\nPlease check internet connection!\n-------------------------\n\n")
      }         
   }
   else
   {  #return a XML file
      returnDoc<-biourl
   }


 
   
   paraXML<-xmlTreeParse(returnDoc)

   ## update term data
   gterms_xml<-paraXML$doc$children[[1]][["gene"]][["allDBKey"]]
   gterms_no<- length(xmlChildren(gterms_xml))
   terms_list<-NULL
   site<-NULL
   detail<-NULL
   for (i in 1:gterms_no)
   {
      terms_list<-rbind(terms_list, xmlValue(gterms_xml[[i]]))
      if (!is.null(xmlGetAttr(gterms_xml[[i]], "site")))
        site<-rbind(site, xmlGetAttr(gterms_xml[[i]], "site"))
      else
        site<-rbind(site, "NNNNNN")
        
      if (!is.null(xmlGetAttr(gterms_xml[[i]], "detail")))
        detail<-rbind(detail, xmlGetAttr(gterms_xml[[i]], "detail"))
      else
        detail<-rbind(detail, "NNNNNN")
   }

   ## separate gene and protein
   terms_list<-rbind(terms_list, "SEP")
   site<-rbind(site, "NNNNNN")
   detail<-rbind(detail, "NNNNNN")

   pterms_xml<-paraXML$doc$children[[1]][["protein"]][["allDBKey"]]
   pterms_no<- length(xmlChildren(pterms_xml))
   for (i in 1:pterms_no)
   {
      terms_list<-rbind(terms_list, xmlValue(pterms_xml[[i]]))
      if (!is.null(xmlGetAttr(pterms_xml[[i]], "site")))
        site<-rbind(site, xmlGetAttr(pterms_xml[[i]], "site"))
      else
        site<-rbind(site, "NNNNNN")
      
      if (!is.null(xmlGetAttr(pterms_xml[[i]], "detail")))
        detail<-rbind(detail, xmlGetAttr(pterms_xml[[i]], "detail"))
      else
        detail<-rbind(detail, "NNNNNN")
   }

   terms_list<-cbind(terms_list, detail)
   terms_list<-cbind(terms_list, site)

   ## update para data
   para<-"geneMap"
   gPara_xml<-paraXML$doc$children[[1]][["gene"]][["parameter"]]
   gPara_no<- length(xmlChildren(gPara_xml))
   for (i in 1: gPara_no)
   {
       para<-c(para, xmlGetAttr(gPara_xml[[i]], "label"))
       para<-c(para, xmlValue(gPara_xml[[i]]))
   }

   para<-c(para, "proteinMap")
   pPara_xml<-paraXML$doc$children[[1]][["protein"]][["parameter"]]
   pPara_no<- length(xmlChildren(pPara_xml))
   for (i in 1: pPara_no)
   {
       para<-c(para, xmlGetAttr(pPara_xml[[i]], "label"))
       para<-c(para, xmlValue(pPara_xml[[i]]))
   }
   para<-c(para, "functionMap")

   ## update gene2protein data
   g2p_xml<-paraXML$doc$children[[1]][["mapping"]][["gene2protein"]]
   tsite<-NULL
   from<-NULL
   to<-NULL
   for (i in 1:length(xmlChildren(g2p_xml)))
   {
      tsite<-rbind(tsite, xmlValue(g2p_xml[[i]]))
      from<-rbind(from, xmlGetAttr(g2p_xml[[i]], "from"))
      to<-rbind(to, xmlGetAttr(g2p_xml[[i]], "to"))
   }

   gene2protein<-data.frame(tfunction=tsite, from_type=from, to_type=to)

   ## update protein2gene data
   p2g_xml<-paraXML$doc$children[[1]][["mapping"]][["protein2gene"]]
   tsite<-NULL
   from<-NULL
   to<-NULL
   for (i in 1:length(xmlChildren(p2g_xml)))
   {
      tsite<-rbind(tsite, xmlValue(p2g_xml[[i]]))
      from<-rbind(from, xmlGetAttr(p2g_xml[[i]], "from"))
      to<-rbind(to, xmlGetAttr(p2g_xml[[i]], "to"))
   }
   protein2gene<-data.frame(tfunction=tsite, from_type=from, to_type=to)
   
   ## update protein2protein data
   p2p_xml<-paraXML$doc$children[[1]][["mapping"]][["protein2protein"]]
   tsite<-NULL
   from<-NULL
   to<-NULL
   for (i in 1:length(xmlChildren(p2p_xml)))
   {
      tsite<-rbind(tsite, xmlValue(p2p_xml[[i]]))
      from<-rbind(from, xmlGetAttr(p2p_xml[[i]], "from"))
      to<-rbind(to, xmlGetAttr(p2p_xml[[i]], "to"))
   }
   protein2protein<-data.frame(tfunction=tsite, from_type=from, to_type=to)
   
   ## update mapping-keys data
   key_xml<-paraXML$doc$children[[1]][["mapping"]][["mappingKey"]]
   keyDetail<-NULL
   gkey<-NULL
   pkey<-NULL
   gno<-NULL
   pno<-NULL
   for (i in 1:length(xmlChildren(key_xml)))
   {
      keyDetail<-rbind(keyDetail, xmlValue(key_xml[[i]]))
      gkey<-rbind(gkey, xmlGetAttr(key_xml[[i]], "gname"))
      gno<-rbind(gno, which(xmlGetAttr(key_xml[[i]], "gname")==terms_list[,1]))

      pkey<-rbind(pkey, xmlGetAttr(key_xml[[i]], "pname"))
      pno<-rbind(pno, which(xmlGetAttr(key_xml[[i]], "pname")==terms_list[,1]))
   }
   keyMap<-data.frame(key_detail=keyDetail, gene_key=gkey, gene_list_no=gno, protein_key=pkey, protein_list_no=pno)

   return(list("term"=terms_list, "key_map"=keyMap, "para"=para, "g2p"=gene2protein, "p2g"=protein2gene, "p2p"=protein2protein))
}

