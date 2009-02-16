## bio.convert.R
## function: main interface for BioIDMapper
## Jan 29, 2007

`bio.convert` <-
function(id_list = "character",
                  from = "numeric",
                  to = "numeric")
{
    require(RCurl)
    require(XML)
    id2term<-.exp$getid2term()
    allPara<-.exp$getallPara()
 
    if(any((missing(id_list)),missing(from),missing(to))) 
    {
        processMessage("Usage:   convert(ids, from, to)")
        processMessage("     ids:      vector of known ids")
        processMessage("     from:     types of ids. check bio.type()")
        processMessage("     to:       types of ids. check bio.type()")
        processMessage("Example 1: bio.convert(c(\"6456604\", \"23396823\"), 1, 5)")
        processMessage("Example 2: bio.convert(glist, 1, 5)")
        return(processMessage("Done."))
    }

    if (missing(id_list))
    {
        stop("Please specify the data to be translated!")
    }

    if (missing(from))
    {
        stop("Please specify type for start ids!")
    }

    if (missing(to))
    {
        stop("Please specify type for end ids!")
    }
    
    ## validate from/to type
    if (!is.numeric(from) || !is.numeric(to))
    {
        stop("From/To should be number. Check help with convert()!")
    }

    if (from==to)
    {
        stop("From/To are same type.")
    }
    
    ## key between NCBI and Uniprot
    if (((from==1) && (to==18)) ||((from==18) && (to==1))) 
    {
        stop("From/To are same type.")
    }
    
    
    if (!from %in% c(1:length(id2term)) || !to %in% c(1:length(id2term)))
    {
         stop("From/To are not in the term list")
    }
    
    if (from==which(id2term=="SEP") || to==which(id2term=="SEP"))
    {
        stop("It is boundary between gene and protein. Not Valid!")
    }
        
    id_list<-  validate(id_list)

    from_type<-id2term[from]
    to_type<-id2term[to]
    
    no_boundaryType<-which(id2term=="SEP")
    no_totaltypes<-nrow(id2term)-1

    g2pFile<-allPara$g2p
    p2gFile<-allPara$p2g
    p2pFile<-allPara$p2p
   
    ## start mapping
    if (from <= no_boundaryType && to <= no_boundaryType)
    {
        processMessage("Parsing data from NCBI")
        final_matrix<-geneMap(id_list, from_type, to_type)
        final_matrix[which(final_matrix=="no_match")]<-NA
        
    }
    else
    {
        if (from > no_boundaryType && to > no_boundaryType)
        {  
            ## UniProt can only transfer between ACC/ID and other ids 
            if (from_type=="ACC" || to_type=="ACC")
            {
                final_matrix<-proteinMap(id_list, from_type, to_type)

                ## avoid length(id_list)==1
                if(is.null(nrow(final_matrix))) return(final_matrix)
                final_matrix[which(final_matrix=="no_match")]<-NA  
              
                if (all(is.na(final_matrix[,2])))
                {
                    final_matrix<- final_matrix[-c(1:nrow(final_matrix)),]
                }
            }
            else
            {
                final_matrix<-dbjumper(id_list, from_type, to_type, p2pFile)  
            }
           
        }
        else
        {       
            if (from < no_boundaryType)
            {   
                final_matrix<-dbjumper(id_list, from_type, to_type, g2pFile)
            }
            else
            {  
                final_matrix<-dbjumper(id_list, from_type, to_type, p2gFile)
                
            }
        }
    }
    # fix 'protein' colnames
    if (length(final_matrix)!=0)
    {
      nf<-colnames(final_matrix)
      nf[nf=='protein']<-'GI number'
      colnames(final_matrix)<-nf
    }

    processMessage("Done...")
    return(final_matrix)
    
}

