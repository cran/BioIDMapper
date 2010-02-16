################################################################################
## BioIDMapper
## Author: Xiaoyong Sun
## Date: Nov.20, 2009
## Goal: GUI
## File: bio.gui.R
################################################################################

bio.gui <- function()
{
    
    Height <- 600
    Width <- 600*1.6

    assign("BioW", gwindow(title="BioIDMapper GUI", parent=c(100,50), height=Height, width=Width), BIOID)

    tb = gtoolbar(tbl)
    mainGroup = ggroup(horizontal=FALSE, spacing=0, cont=BIOID$BioW, expand=TRUE)

    ## subGUI
    mynotebook1=gnotebook(closebuttons = TRUE,dontCloseThese = 1, # was 1:2 before commands area moved
                                                         tearable = FALSE)
    size(mynotebook1) <- c( Width*0.3, Height*0.8)
    assign("bio.dialog.notebook1", mynotebook1, BIOID)

    mynotebook2=gnotebook(closebuttons = TRUE,dontCloseThese = 1, # was 1:2 before commands area moved
                                                         tearable = FALSE)
    size(mynotebook2) <- c( Width*0.7, Height*0.8)
    assign("bio.dialog.notebook2", mynotebook2, BIOID)

    ###############################
    ## main layout
    ####################################
    add(mainGroup, tb)
    bottomGroup = ggroup(horizontal=TRUE)
    add(mainGroup, bottomGroup, expand=TRUE)

    data(glist)
    from.table <- "AllIDs"
    to.table <- "TargetIDs"
    colnames(glist) <- "TargetIDs"
    pk.dir.right1 = gtable(data.frame(glist), sort.columns = 1:2, expand=TRUE)
    colnames(glist) <- from.table
    pk.dir.left1 = gtable(data.frame(glist), sort.columns = 1:2, expand=TRUE)

    pk.dir.right1[] <- c()
    size(pk.dir.right1) <- c(Width*0.7, Height*0.7)
    assign("pk.dir.right", pk.dir.right1, BIOID)

    part.pane2=gpanedgroup(BIOID$pk.dir.right,horizontal=FALSE)
    add(BIOID$bio.dialog.notebook2, part.pane2, label = "To",
        override.closebutton = TRUE
        )

  rightpane = gpanedgroup(BIOID$bio.dialog.notebook2, horizontal=FALSE)
  
  pk.dir.left1[] <- c()
  size(pk.dir.left1) <- c(Width*0.3, Height*0.7)
  assign("pk.dir.left", pk.dir.left1, BIOID)

  part.pane1=gpanedgroup(BIOID$pk.dir.left,horizontal=FALSE)
  add(BIOID$bio.dialog.notebook1, part.pane1, label = "From",
        override.closebutton = TRUE
        )

  leftpane = gpanedgroup(BIOID$bio.dialog.notebook1, horizontal=FALSE)
  pg = gpanedgroup(leftpane, rightpane)

  add(bottomGroup, pg, expand=TRUE)
  assign("bio.statusBar", gstatusbar("Ready", container=NULL), BIOID)
  add(mainGroup, BIOID$bio.statusBar)

}
