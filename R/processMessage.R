## processMessage.R
## Function: output message
## non-access to user
## Jan.29, 2008

`processMessage` <-
function(message)
{
 if(interactive())
 {
    cat(paste(message, "\n", sep=""))
 }
}

