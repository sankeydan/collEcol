#' slACC
#'
#' Converts short number into long number specific to the ACC
#'
#' # short - long acc num
#'

slACC = function(){
{accs = read.table("clipboard")
accs = accs[,1]

old.wd = getwd()

setwd("C:/Users/Sankey_Dan/Dropbox/Software/R/Dataframes")
load("short.long.num.rda")

setwd(old.wd)
writeClipboard(as.character(short.long.num[accs,2]))}
}
