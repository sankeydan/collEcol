#' Choose only the files you want
#'
#' other packages used in this function: stringr
#' @param output "numbers" or "filenames". Filenames probably more useful. We'll see.
#' @param folder "GPS" or "ACC"
#' @param year Year 1 or Year 2 of Phd
#' @param group.num c(1,2) etc. or "all"
#' @param g_s group, solo or all = "g", "s" or "all"
#' @param f_u familiar, unfamiliar or all = "f", "u" or "all"
#' @param site Site 1,2,3,4,5,6,7,8,9,10 or "all"
#' @param miles 4, 8 or "all"
#' @param direction N, NE, E, SE, S, SW, W, NW or "all"
#' @param flight.nums 1:12 or "all"
#' @param pigeon.nums or c(32,39,40) etc.
#' @param release.of.day  1,2, 3 , c(2,3) or "all"



files.chooser = function(output = "filenames" , folder = "GPS", year = "all", group.num = "all", g_s = "all", f_u = "all" , site = "all", miles = "all", direction = "all" ,flight.nums = "all", pigeon.nums = "all", release.of.day = "all"){

  # libraries

  library(stringr)

  # set working directory

  setwd( file.path( PROJHOME, "data", folder))

  files1 = list.files()


  #split files to matrix (Wanted Files)
  files2 = str_split_fixed(list.files(), "\\.", 16)
  wf = matrix(NA, nrow =  nrow(files2), ncol = 10) ## wanted files. i.e. Im using which function to see which files I want!

  ## year
  if(year == 1){
    wf[,1] = which(files2[,1] == "P1")
  }
  if(year == 2){
    wf[,1] = which(files2[,1] == "P2")
  }
  if(year == "all"){
    wf[,1] = 1:nrow(files2)
  }

  ## Group number
  if(is.numeric(group.num) == T){
    wf[1:length(which(files2[,2] %in% group.num)),2] = which(files2[,2] %in% group.num)
  }
  if(group.num[1] == "all"){
    wf[,2] = 1:nrow(files2)
  }

  ## group or solo

  if( g_s == "g"){
    wf[1:length(which(files2[,4] == "Gro")),3] = which(files2[,4] == "Gro")
  }
  if( g_s == "s"){
    wf[1:length(which(files2[,4] == "Sol")),3] = which(files2[,4] == "Sol")
  }
  if( g_s == "all"){
    wf[,3] = 1:nrow(files2)
  }

  #Familiar or Unfamiliar
  if( f_u == "f"){
    wf[1:length(which(files2[,5] == "F")),4] = which(files2[,5] == "F")
  }
  if( f_u == "u"){
    wf[1:length(which(files2[,5] == "U")),4] = which(files2[,5] == "U")
  }
  if( f_u == "all"){
    wf[,4] = 1:nrow(files2)
  }

  ## Site
  if(is.numeric(site) == T){
    wf[1:length(which(files2[,6] %in% site)),5] = which(files2[,6] %in% site)
  }
  if(site[1] == "all"){
    wf[,5] = 1:nrow(files2)
  }

  ## Miles
  if(is.numeric(miles) == T){
    wf[1:length(which(files2[,7] %in% miles)),6] = which(files2[,7] %in% miles)
  }
  if(miles[1] == "all"){
    wf[,6] = 1:nrow(files2)
  }

  ## direction

  if(direction == "all"){
    wf[,7] = 1:nrow(files2)
  }
  if(direction != "all"){
    wf[1:length(which(files2[,8] %in% direction)),7] = which(files2[,8] %in% direction)
  }

  ## Flight nums
  if(is.numeric(flight.nums) == T){
    wf[1:length(which(files2[,9] %in% flight.nums)),8] = which(files2[,9] %in% flight.nums)
  }
  if(flight.nums[1] == "all"){
    wf[,8] = 1:nrow(files2)
  }

  ## Pigeon nums
  if(is.numeric(pigeon.nums) == T){
    wf[1:length(which(files2[,11] %in% pigeon.nums)),9] = which(files2[,11] %in% pigeon.nums)
  }
  if(pigeon.nums[1] == "all"){
    wf[,9] = 1:nrow(files2)
  }

  ## Release of day
  if(is.numeric(release.of.day ) == T){
    wf[1:length(which(files2[,14] %in% release.of.day )),10] = which(files2[,14] %in% release.of.day )
  }
  if(release.of.day[1]  == "all"){
    wf[,10] = 1:nrow(files2)
  }

  for(i in 1:ncol(wf)){
    assign(paste("w.",i , sep = ""), na.omit(as.vector(wf[,i])))
  }

  if(output == "numbers"){
    return(Reduce(intersect, list(w.1,w.2,w.3,w.4,w.5,w.6,w.7,w.8,w.9,w.10)))
  }

  if(output == "filenames"){
    return(list.files()[Reduce(intersect, list(w.1,w.2,w.3,w.4,w.5,w.6,w.7,w.8,w.9,w.10))])
  }
}



