## bio.link.R
## Function: link module - link to specific websites
## Jan 29, 2008

`bio.link` <-
function(id, to)
{
    id2term<-.exp$getid2term()
    link2site<-.exp$getlink2site()
 
    if(any(missing(id),missing(to)))
    {
        processMessage("Usage:   biolink(id, to)")
        processMessage("   --id:  known id")
        processMessage("   --to: type for the known id")
        processMessage("Example: biolink(\"200529\", 1)")
        return(processMessage("Done."))
    }
    
    if (missing(to))
    {
        processMessage("Please input the type of website you want to link.")
        return(processMessage("Done."))
    }
    
    if (!to %in% c(1:length(id2term)))
    {
         stop("Input Paramter is not in the term list")
    }
    
    if (!is.na(link2site[to]) && link2site[to]!="NNNNNN")
      browseURL(paste(link2site[to], id, sep=""))
    else
      processMessage("Currently no link to this external database.")
    return(processMessage("Done."))

}

