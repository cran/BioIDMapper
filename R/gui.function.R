################################################################################
## BioIDMapper
## Author: Xiaoyong Sun
## Date: Nov.20, 2009
## Goal: GUI
## File: gui.function.R
################################################################################

# for addRightTable
getLabel <- function(mylabel)
{

    known.label <- names(.exp$getAllIDSets())
    check.label <- grep(mylabel, known.label)
    if (length(check.label)==0) return(mylabel)
    else return(paste(mylabel, length(check.label)+1, sep="_"))
}

addRightTable <- function(done, mylabel, notsave)
{
           ptable=gtable(done, multiple=TRUE, expand=TRUE)
           size(ptable) <- c(Width*0.5, Height*0.7)
           part.pane2=gpanedgroup(ptable,horizontal=FALSE)

           add(BIOID$bio.dialog.notebook2, part.pane2, label = mylabel,
                  override.closebutton = TRUE
               )
           
           if (missing(notsave))
           {

               .exp$setNoteConvertType("data")
           }
           else if (notsave==1)
           {
               .exp$setNoteConvertType("Merge")
           }

           mylabel <- getLabel(mylabel)
           .exp$setIDSets(done, mylabel)

}


checkDataType <- function(target.notebookNo)
{
    typeNo <- .exp$getNoteConvertType()
    return(typeNo[target.notebookNo])
}



ErrorMessage <- function(mymessage)
{
    gmessage(mymessage, icon="warning", title="warning")
}


# labelMessage1 <- "Please move data for diagnostics from left TABLE to right TABLE."
# labelMessage2 <- "After choosing data, click to interactive diagnostics."
# winTitle <- "Configure datasets"
# statusMessage <- "Data is ready for interactive diagnostics."
# menuOption: 1 for model comparison; 2 for interactive graphics

selectDataDialog <- function(winTitle, labelMessage1, labelMessage2, statusMessage, menuOption)
{
   # check data exist
    if(all(is.na(BIOID$pk.dir.left[])))
    {
        ErrorMessage("No data is available! Please open ID list first.")
        return(invisible(NULL))
    }

        ggobi.gwin <- gwindow(title=winTitle)
        ggobi.group <- ggroup(cont=ggobi.gwin, horizontal=FALSE)
        g0 <- glabel(text=labelMessage1, cont=ggobi.group)
        g1 <- gframe(cont=ggobi.group)
            size(g1) <- c(Width*0.5, Height*0.5)

        id2term <- .exp$gettype()
        alldata.name <- id2term[,2]
        fromid <- .exp$getFromID()
        fromind <- which(alldata.name %in% fromid)
        alldata.name <- alldata.name[-fromind]

        alldata.name <- data.frame(alldata.name, stringsAsFactors=FALSE)
    
    tbl1 = gtable(data.frame(AllIDs = alldata.name), cont = g1, expand=TRUE)
    size(tbl1) <- c(Width*0.2, Height*0.4)

    arrowButton = gbutton(">", cont = g1);
    enabled(arrowButton) <- FALSE

    tbl2 = gtable(data.frame(TargetIDs = alldata.name), cont=g1, expand = TRUE)
    tbl2[] <- c() ## clear out initialized values. Can't start gtable empty.
    size(tbl2) <- c(Width*0.2, Height*0.4)

    g3 <- glabel(text=labelMessage2, cont=ggobi.group)
    confirmButton = gbutton("Go to next step", cont= ggobi.group)


    addHandlerClicked(tbl1, function(h,...)
    {
      enabled(arrowButton) <- TRUE
      svalue(arrowButton) <- ">"
      ## no means to clear all selections
    })
    addHandlerClicked(tbl2, function(h,...)
    {
      enabled(arrowButton) <- TRUE
      svalue(arrowButton) <- "<"
    })

    addHandlerClicked(arrowButton, function(h,...)
    {
      if(svalue(arrowButton) == ">") {
        curVal = svalue(tbl1)
        tbl1[] <- setdiff(tbl1[], curVal)
        if(any(is.na(tbl2[])))
          tbl2[] <- curVal
        else
          tbl2[] <- unique(c(tbl2[],curVal))          # adjust for initial  NA
      } else {
        curVal = svalue(tbl2)
        tbl2[] <- setdiff(tbl2[], curVal)
        tbl1[] <- sort(unique(c(tbl1[],curVal)))
      }
      enabled(arrowButton) <- FALSE
    })

    addHandlerClicked(confirmButton, function(h,...)
    {
       it.data <- tbl2[]
       if(length(it.data)==0)
       {
          ErrorMessage("Please use button in the middle to choose data from left column")
          return(invisible(NULL))
       }
       .exp$cleanMapSets()
       sapply(1:length(it.data), function(i) .exp$setMapSets(it.data[i], it.data[i]))
       svalue(BIOID$bio.statusBar) <- statusMessage
       dispose(ggobi.gwin)


    })

}

################################################################################
# PUBLIC FUNCTIONS
################################################################################

## handler to open data, including file, load, data varible
gui.open <- function()
{
## initialize
    BIOID$startName <- NULL

    gtmp.win = gwindow("Select Biological ID files", horizontal=FALSE)

    gtgroup1 = ggroup(cont=gtmp.win, horizontal=FALSE)

    gf1 <- gframe(text = "Configure", markup = FALSE, pos = 0, horizontal=TRUE, container = gtgroup1)
    tbl <- glayout(cont=gf1)

    tbl[1,1, anchor=c(-1,-1)] = glabel("Read data from R environment: ")
    data.env = gedit("")
    tbl[1,2] = data.env
    
    tbl[2,1, anchor=c(-1,-1)] = glabel("Target ID type:")
    fromID = gdroplist(items = .exp$gettype()[,2])
    tbl[2,2] = fromID

    gb1 = gbutton(text="Click to open", horizontal=FALSE )
    tbl[3,2] = gb1
    
    addhandlerclicked(gb1, function(h,...)
      {
          if (svalue(data.env) != "")
          {
              my.name <- as.name(svalue(data.env))
              old.data <- eval(my.name)
              my.data <- old.data

              if (is.data.frame(my.data))
              {
                  if (ncol(my.data) > 1)
                  {
                      BIOID$startName <- data.frame(my.data[,c(1,2)])
                      my.data <- data.frame(my.data[,1])
                      colnames(my.data) <- svalue(fromID)

                  }
              }
              else if (is.list(my.data))
              {
                  uxx <- unlist(my.data)
                  uxx <- uxx[!is.na(uxx)]
                  BIOID$startName <- data.frame(ID=uxx, name=names(uxx), stringsAsFactors = FALSE )
                  colnames(BIOID$startName)[2] <- svalue(fromID)
                  
                  my.data <- data.frame(uxx, stringsAsFactors = FALSE )
                  colnames(my.data) <- svalue(fromID)
              }
              else
              {
                  ErrorMessage("ID list should be in data.frame or list format!")
                  return(invisible(NULL))
              }
              

              if (nrow(my.data) < 1 || ncol(my.data) > 1)
              {
                  mymessage <- paste("The data has", nrow(my.data), "row(s) and", ncol(my.data), "column(s)", sep=" ")
                  mymessage <- paste(mymessage, "BioIDMapper only read one column of IDs.", sep=" ")
                  ErrorMessage(mymessage)
                  return(invisible(NULL))
              }

              if (!is.na(BIOID$pk.dir.left[])) BIOID$pk.dir.left[] <- c()
              BIOID$pk.dir.left[] <- validate(my.data)

              if (!is.na(BIOID$pk.dir.right[])) BIOID$pk.dir.right[] <- c()

              if (is.null(BIOID$startName)) BIOID$pk.dir.right[] <- validate(my.data)
              else BIOID$pk.dir.right[] <- validate(BIOID$startName[,2])

              svalue(BIOID$bio.statusBar) <- "Data is loaded successfully."

              ## save data set
              savedata <- data.frame(validate(my.data))
              colnames(savedata) <- svalue(fromID)
              .exp$setIDSets(savedata, svalue(fromID))
              .exp$setNoteConvertType("data")
              .exp$setFromID(svalue(fromID))
              dispose(gtmp.win)
          }
          else
          {
              ErrorMessage("Please enter data name!")
              return(invisible(NULL))

          }

      })


}

gui.options <- function()
{
  labelMessage1 <- "Please move target dataset from left TABLE to right TABLE."
  labelMessage2 <- "After choosing data, click to map."
  winTitle <- "Preference"
  statusMessage <- "Data is configured for ID conversion."

  selectDataDialog(winTitle, labelMessage1, labelMessage2, statusMessage)
}

gui.convert <- function()
{
   # check data exist
    if(all(is.na(BIOID$pk.dir.left[])))
    {
        ErrorMessage("No data is available! Please open ID list first.")
        return(invisible(NULL))
    }
    
       ## TODO: detail
       id2term<-.exp$gettype()
       
       mydata <- .exp$getIDSets(.exp$getFromID())
       to.list <- .exp$getMapSets()
       toind <- which(id2term[,2] %in% names(to.list))

       fromid <- .exp$getFromID()
       fromind <- which(id2term[,2]==fromid)

       ## Process Bar
      convertW = gwindow(title="Process of conversion", parent=c(150,200), height=Height*0.05, width=Width*0.5, horizontal=FALSE)

      convert.group =ggroup(horizontal=FALSE, spacing=0, expand=TRUE)
      convert.bar <- gtkProgressBar()

      add(convert.group, convert.bar)
      add(convertW, convert.group)

      # start with something
      check <- 0.2
      gtkProgressBarSetFraction(convert.bar, check)

      sapply(1:length(toind), function(i)
      {
           done <- bio.convert(mydata, fromind[1], toind[i])
           addRightTable(done, id2term[,2][toind[i]])

           check <- i/length(toind)
           gtkProgressBarSetFraction(convert.bar, check)

           if (i > 1) Sys.sleep(5)
       })
      
      dispose(convertW)
      svalue(BIOID$bio.statusBar) <- "IDs are converted successfully."

}

gui.subset <- function()
{
   # check data exist
    if(all(is.na(BIOID$pk.dir.left[])))
    {
        ErrorMessage("No data is available! Please open ID list first.")
        return(invisible(NULL))
    }
    
    ## check data type
    noteid <- as.numeric(svalue(BIOID$bio.dialog.notebook2))
    datatype <- checkDataType(noteid)
    if (datatype != "data")
    {
        ErrorMessage(paste("This data can NOT be subsetted since it is ",datatype, " data", sep="" ))
        return(invisible(NULL))
    }

    target.data <- data.frame(.exp$getIDSets(noteid))
    
    subset1.win = gwindow("Subset", horizontal=FALSE)
    gtgroup1 = ggroup(cont=subset1.win, horizontal=FALSE)

    gf1 <- gframe(text = "Interested ID for subset", markup = FALSE, pos = 0, horizontal=TRUE, container = gtgroup1)
    tbl <- glayout(cont=gf1)

    tbl[1,1, anchor=c(-1,-1)] = glabel("ID: ")

    id.list = gdroplist(items = colnames(target.data))
    tbl[1,2] = id.list

    mybutton = gbutton(text="Continue", horizontal=FALSE)
    tbl[1,3] = mybutton
    
    addhandlerclicked(mybutton, function(h,...)
        {
           gui.subset.2(svalue(id.list))
           dispose(subset1.win)
        })
}

gui.subset.2 <- function(keyid)
{
    # data sets
    noteid <- as.numeric(svalue(BIOID$bio.dialog.notebook2))
    target.data <- data.frame(.exp$getIDSets(noteid))
    
    tmp.data <- unique(target.data[[keyid]])
    tmp.data <- tmp.data[!is.na(tmp.data)]
    table.data <- data.frame(tmp.data)
    colnames(table.data) <- keyid

    subsetW = gwindow(title="Subset", parent=c(100,50), height=Height, width=Width*0.5, horizontal=FALSE)

    subset.group =ggroup(horizontal=FALSE, spacing=0, expand=TRUE)
    subset.frame1 = gframe(text = "Subset data", markup = FALSE, pos = 0, cont=subset.group, horizontal=TRUE)

    ## frame 1
    ## TODO: check data is all NA
    if (ncol(target.data) < 1)
    {
        ErrorMessage("You don't have data for subsetting!")
        return(invisible(NULL))
    }
    

         subset.table = gtable(table.data, multiple = TRUE, sort.columns = 1:2,
                              expand=TRUE)
         size(subset.table) <- c(Width*0.5, Height*0.6)
         add(subset.frame1, subset.table)

    ## frame 3
    choice <- names(.exp$getAllIDSets())
    choice <- choice[-c(1, noteid)]

        deleteMerge.ind <- grep("Merge", choice)
        if (length(deleteMerge.ind)>0)  choice <- choice[- deleteMerge.ind]
        deleteSummary.ind <- grep("Summary", choice)
        if (length(deleteSummary.ind)>0)  choice <- choice[- deleteSummary.ind]
    
    ## if no other data sources for link OR the first original data is chosen
    if (length(choice) == 0 || noteid == 1) choice <- "No available choice"

        subset.frame3 = gframe(text = "Link additional IDs", markup = FALSE, pos = 0, cont=subset.group, horizontal=TRUE)
        id.choice <- gcheckboxgroup(choice, cont=subset.frame3, horizontal=TRUE)

    if (choice[1] == "No available choice")  enabled(id.choice) <- FALSE

    subset.button = gbutton(text="Subset", horizontal=FALSE, cont=subset.group)
 
    
    add(subsetW, subset.group)
      
    addhandlerclicked(subset.button, handler= function(h,...)
    {
               choice.value <- svalue(subset.table)
               if(length(choice) > 0) choice.idlist <- svalue(id.choice)

               if (length(choice.value)==0)
               {
                  ErrorMessage("Please choose DATA first!")
                  return(invisible(NULL))
               }

               target.ind <- which(target.data[[keyid]] %in% choice.value)
               target.value <- unique(target.data[target.ind,])

               if (length(choice.idlist)!=0)
               {
                   ## merge data
                   ##TODO: need a processBar here too!
                   sapply(1:length(choice.idlist), function(i)
                          {
                              x.data <- target.value
                              y.data <- data.frame(.exp$getIDSets(choice.idlist[i]))

                              target.value <<- merge(x.data, y.data, by.x=colnames(target.value)[1],
                                                    by.y=colnames(y.data)[1], all.x=TRUE)

                          })
              }


               BIOID$mergeNo <- BIOID$mergeNo + 1
               mylabel <- paste("MergeData", BIOID$mergeNo, sep="_")

               # 1: merge data type
               addRightTable(target.value, mylabel, 1)
               dispose(subsetW)
               svalue(BIOID$bio.statusBar) <- "Data are subsetted successfully."

    })
    
    
}

gui.link <- function()
{
   # check data exist
    if(all(is.na(BIOID$pk.dir.left[])))
    {
        ErrorMessage("No data is available! Please open ID list first.")
        return(invisible(NULL))
    }
    
    ## check data type
    noteid <- as.numeric(svalue(BIOID$bio.dialog.notebook2))
    datatype <- checkDataType(noteid)
    if(datatype != "data" && datatype != "Merge")
    {
        ErrorMessage("Summary data can Not perform this function!")
        return(invisible(NULL))
    }
    
    subset1.win = gwindow("Connect", horizontal=FALSE)

    gtgroup1 = ggroup(cont=subset1.win, horizontal=FALSE)

    gf1 <- gframe(text = "Interested ID for Connecting to external resources", markup = FALSE, pos = 0, horizontal=TRUE, container = gtgroup1)
    tbl <- glayout(cont=gf1)

    tbl[1,1, anchor=c(-1,-1)] = glabel("ID: ")


    target.data <- data.frame(.exp$getIDSets(noteid))

    id.list = gdroplist(items = colnames(target.data))
    tbl[1,2] = id.list

    mybutton = gbutton(text="Continue", horizontal=FALSE)
    tbl[1,3] = mybutton

    addhandlerclicked(mybutton, function(h,...)
        {
           gui.link.2(svalue(id.list))
           dispose(subset1.win)
        })
}

gui.link.2 <- function(keyid)
{
    # data sets
    noteid <- as.numeric(svalue(BIOID$bio.dialog.notebook2))
    target.data <- data.frame(.exp$getIDSets(noteid))

    table.data <- data.frame(unique(target.data[[keyid]]))
    colnames(table.data) <- keyid

    subsetW = gwindow(title="Connect", parent=c(100,50), height=Height, width=Width*0.5, horizontal=FALSE)

    subset.group =ggroup(horizontal=FALSE, spacing=0, expand=TRUE)
    subset.frame1 = gframe(text = "Connect to external resources", markup = FALSE, pos = 0, cont=subset.group, horizontal=TRUE)
    subset.button = gbutton(text="Connect", horizontal=FALSE, cont=subset.group)

    ## frame 1
    ## TODO: check data is all NA
    if (ncol(target.data) < 1)
    {
        ErrorMessage("You don't have data for connection!")
        return(invisible(NULL))
    }


     subset.table = gtable(table.data, multiple = TRUE, sort.columns = 1:2,
                              expand=TRUE)
     size(subset.table) <- c(Width*0.5, Height*0.6)
     add(subset.frame1, subset.table)

     add(subsetW, subset.group)

    addhandlerclicked(subset.button, handler= function(h,...)
    {
               choice.value <- svalue(subset.table)

               if (length(choice.value)==0)
               {
                  ErrorMessage("Please choose DATA first!")
                  return(invisible(NULL))
               }

               id2term<-.exp$gettype()

               keyid <- gsub("\\.", " ", keyid)
               toind <- which(id2term[,1] %in% keyid)
               if (length(toind) < 1)
               {
                  toind <- which(id2term[,2] %in% keyid)
                  if (length(toind) < 1)
                  {
                      ErrorMessage("Thi ID is Not supported for external resources!")
                      return(invisible(NULL))
                  }
               }


               sapply(1:length(choice.value), function(i) bio.link(choice.value[i], toind[1]) )

               dispose(subsetW)
               svalue(BIOID$bio.statusBar) <- "Data are linked to external sources successfully."
    })
    
    addhandlerdoubleclick(subset.table, handler= function(h,...)
    {
               choice.value <- svalue(subset.table)

               if (length(choice.value)==0)
               {
                  ErrorMessage("Please choose DATA first!")
                  return(invisible(NULL))
               }

               id2term<-.exp$gettype()

               toind <- which(id2term[,1] %in% keyid)
               if (length(toind) < 1)
               {
                  ErrorMessage("Thi ID is Not supported for external resources!")
                  return(invisible(NULL))
               }

               bio.link(choice.value, toind)

               dispose(subsetW)
               svalue(BIOID$bio.statusBar) <- "Data are linked to external sources successfully."

    })

}


gui.summary <- function()
{
   # check data exist
    if(all(is.na(BIOID$pk.dir.left[])))
    {
        ErrorMessage("No data is available! Please open ID list first.")
        return(invisible(NULL))
    }

    ## check data type
    noteid <- as.numeric(svalue(BIOID$bio.dialog.notebook2))
    if (noteid == 1)
    {
        ErrorMessage("Only one ID list can Not perform thsi function!")
        return(invisible(NULL))
    }
    datatype <- checkDataType(noteid)
    if(datatype != "data" && datatype != "Merge")
    {
        ErrorMessage("Summary data can Not perform this function!")
        return(invisible(NULL))
    }

    target.data <- .exp$getIDSets(noteid)
    start.data <- .exp$getIDSets(1)
    mylabel <- "Summary"
    
    sink("sinkTmp.txt")
    bio.sum(target.data, start.data)
    sink()

    psum = gtext(text=readLines("sinkTmp.txt", n=-1), width=Width*0.5, height=Height*0.7)
           part.pane2=gpanedgroup(psum,horizontal=FALSE)
    add(BIOID$bio.dialog.notebook2, part.pane2, label = mylabel,
                  override.closebutton = TRUE
                  )

    .exp$setNoteConvertType("Summary")
    .exp$setIDSets(readLines("sinkTmp.txt", n=-1), mylabel)

    svalue(BIOID$bio.statusBar) <- "Data are evaluated successfully."
    on.exit(unlink("sinkTmp.txt"))
}

gui.plot <- function()
{
   # check data exist
    if(all(is.na(BIOID$pk.dir.left[])))
    {
        ErrorMessage("No data is available! Please open ID list first.")
        return(invisible(NULL))
    }

    ## get data
    noteid <- as.numeric(svalue(BIOID$bio.dialog.notebook2))
    if (noteid == 1)
    {
        ErrorMessage("Only one ID list can Not perform thsi function!")
        return(invisible(NULL))
    }
    datatype <- checkDataType(noteid)
    if(datatype != "data" && datatype != "Merge")
    {
        ErrorMessage("Summary data can Not perform this function!")
        return(invisible(NULL))
    }

    target.data <- .exp$getIDSets(noteid)
    start.data <- .exp$getIDSets(1)
    mylabel <- "Plot"

    plot.result <- bio.sum(target.data, start.data, TRUE)

    ## start window
    bar.window <- gwindow(title="Graph summary for mapping", height=Height, width=Width*0.7)

    gim = ggroup(horizontal=TRUE, expand=TRUE) # expand

    bio.dialog.image1 <- gimage("save", dirname="stock", cont=gim, size="large_toolbar")
    gseparator(horizontal=FALSE, cont=gim)

    bio.dialog.image2 <- gimage("close", dirname="stock", cont=gim, size="large_toolbar")

    addhandlerclicked(bio.dialog.image1, handler=function(h,...)
    {
        subid.notebook <- as.numeric(svalue(bio.dialog.notebook))
        graph.boundary <- length(plot.result)-1

        if (subid.notebook <= graph.boundary) myaction <- "write.table"
        else myaction <- graph.form

        gfile("Save", type="save", action=myaction, handler = function(h,...)
        {
              #tmp.data <- do.call(h$action,list(h$file, ))
              if (h$action=="write.table")
              {
                  i <- subid.notebook

                  mydata <- plot.result[[i]]
                  mylabel <- names(plot.result)[i]

                  table.data <- data.frame(mydata)
                  if (nrow(table.data) <=1 )
                  {
                      ErrorMessage("Only one row. No need to continue")
                      return(invisible(NULL))
                  }
                  table.data <- table.data[order(table.data[,2], decreasing=TRUE),]
                  names(table.data) <- c(colnames(target.data)[i], "Frequency")

                  file.name <- paste(h$file, "txt", sep=".")
                  do.call(h$action, list(x=table.data, file=file.name))
              }
              else
              {
                  i <- subid.notebook - graph.boundary
                  mydata <- plot.result[[i]]
                  mylabel <- names(plot.result)[i]

                  graph.label <- paste(mylabel, "graph", sep="_")
                  
                  table.data <- data.frame(mydata)
                  if (nrow(table.data) <=1 )
                  {
                      ErrorMessage("Only one row. No need to continue")
                      return(invisible(NULL))
                  }
                  
                  table.data <- table.data[order(table.data[,2], decreasing=TRUE),]
                  names(table.data) <- c(colnames(target.data)[i], "Frequency")

                  file.name <- paste(h$file, myaction, sep=".")
                  do.call(h$action, list(file=file.name))
                  print(dotplot(table.data[,2]~table.data[,1], xlab=colnames(table.data)[1],
                        ylab=colnames(table.data)[2], main=graph.label))
                  on.exit(dev.off())
              }

        })

    })
    addhandlerclicked(bio.dialog.image2, handler=function(h,...) dispose(bar.window))


    bio.dialog.notebook <- gnotebook(closebuttons = TRUE,dontCloseThese = 1, tearable = FALSE)
    part.pg = gpanedgroup(gim, bio.dialog.notebook, horizontal = FALSE)


    add(bar.window, part.pg)

    ## draw table
    sapply(1:(length(plot.result)-1), function(i)
    {
        mydata <- plot.result[[i]]
        mylabel <- names(plot.result)[i]

        table.data <- data.frame(mydata)
                  if (nrow(table.data) <=1 )
                  {
                      ErrorMessage("Only one row. No need to continue")
                      return(invisible(NULL))
                  }
                  
        table.data <- table.data[order(table.data[,2], decreasing=TRUE),]
        names(table.data) <- c(colnames(target.data)[i], "Frequency")
        
        bgtable = gtable(table.data, sort.columns = 1:2, expand=TRUE)
        table.label <- paste(mylabel, "table", sep="_")
        add(bio.dialog.notebook, bgtable, label = table.label,
              override.closebutton = TRUE)
        
    })
    
    ## draw graph
    sapply(1:(length(plot.result)-1), function(i)
    {
        mydata <- plot.result[[i]]
        mylabel <- names(plot.result)[i]

        graph.label <- paste(mylabel, "graph", sep="_")
        bgraph = ggraphics(ps=6)
        size(bgraph) <- c(Height, Width*0.6)
        add(bio.dialog.notebook, bgraph, label = graph.label,
              override.closebutton = TRUE)

        table.data <- data.frame(mydata)
                  if (nrow(table.data) <=1 )
                  {
                      ErrorMessage("No data is available. No need to continue")
                      return(invisible(NULL))
                  }
        table.data <- table.data[order(table.data[,2], decreasing=TRUE),]
        names(table.data) <- c(colnames(target.data)[i], "Frequency")

        print(dotplot(table.data[,2]~table.data[,1], xlab=colnames(table.data)[1],
              ylab=colnames(table.data)[2], main=graph.label))

    })


}

gui.save <- function()
{
   # check data exist
    if(all(is.na(BIOID$pk.dir.left[])))
    {
        ErrorMessage("No data is available! Please open ID list first.")
        return(invisible(NULL))
    }
    
    save.win = gwindow("Save", horizontal=FALSE)

    gtgroup1 = ggroup(cont=save.win, horizontal=FALSE)

    gf1 <- gframe(text = "Save results", markup = FALSE, pos = 0, horizontal=TRUE, container = gtgroup1)
    tbl <- glayout(cont=gf1)

    tbl[1,1, anchor=c(-1,-1)] = glabel("File name: ")


    id.list = gedit(text="")
    tbl[1,2] = id.list

    mybutton = gbutton(text="Save", horizontal=FALSE)
    tbl[1,3] = mybutton

    addhandlerclicked(mybutton, function(h,...)
        {
           ## check data type
            noteid <- as.numeric(svalue(BIOID$bio.dialog.notebook2))
            datatype <- checkDataType(noteid)
            if (datatype != "Summary" )
            {
                write.table(data.frame(.exp$getIDSets(noteid)) ,file=svalue(id.list))
            }
            else
            {
                zz <- file(svalue(id.list), "w")  # open an output file connection
                writeLines(.exp$getIDSets(noteid), zz)
                close(zz)
                
            }
            
            dispose(save.win)
            svalue(BIOID$bio.statusBar) <- "Data are saved successfully."
        })
}

gui.quit <- function()
{
    dispose(BIOID$BioW)
}