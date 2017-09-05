#' namew
#'
#' Concacenates clipboard tables from excel to create filenames

namew = function() {

{data <- read.table("clipboard", header = F, stringsAsFactors = F, colClasses = c("character"))

# Function

name.funct = function(bob){
  paste(as.character(bob), collapse = ".")}

#############################

## SCRIPT ##

#############################

data1 <- data.frame(lapply(data, as.character), stringsAsFactors=FALSE)

writeClipboard(apply(data1, FUN=name.funct, MARGIN = 1))
}
}


