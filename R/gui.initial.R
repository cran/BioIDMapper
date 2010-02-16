################################################################################
## BioIDMapper
## Author: Xiaoyong Sun
## Date: Nov.20, 2009
## Goal: GUI
## File: gui.initial.R
################################################################################

        BIOID <- new.env()
        assign("BioW", NULL, BIOID)
        assign("bio.dialog.notebook1", NULL, BIOID)
        assign("bio.dialog.notebook2", NULL, BIOID)
        assign("bio.statusBar", NULL, BIOID)
        assign("pk.dir.right", NULL, BIOID)
        assign("pk.dir.left", NULL, BIOID)
        assign("mergeNo", 0, BIOID)
        assign("startName", NULL, BIOID)
        
        Height <- 600 #getSubHeight()
        Width <- 600*1.6 #getSubWidth()
        graph.form <- "pdf"  # pdf, jpeg, bmp


    ## Tool bar
        tbl = list(
            open = list(handler=function(h,...) gui.open(), icon="open"),
            options=list(handler=function(h,...) gui.options(), icon="preferences"),
            convert = list(handler=function(h,...) gui.convert(), icon="convert"),
            subset = list(handler=function(h,...) gui.subset(), icon="subset"),
            link = list(handler=function(h,...) gui.link(), icon="connect"),
            summary=list(handler=function(h,...) gui.summary(), icon="evaluate"),
            plot=list(handler=function(h,...) gui.plot(), icon="plot"),
            save = list(handler=function(h,...) gui.save(), icon="save"),
            quit=list(handler=function(h,...) gui.quit(), icon="quit")
            )

