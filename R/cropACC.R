#' cropACC
#'

cropACC = function(){

# Libraries

library(stringr)

#############################

## CONTENTS

# P1. Write file names
# P2. Set a working directory for output files

# 1. Manipulate time vector (ELIMINATE MILISECONDS)
# 2. Crop!
# 3. Plot
# 4. Save

#############################

## PRELIMINARY ##

#############################

# P1

{files <-read.table("clipboard") # from Excel document "Pigeon_Protocol" under the columns: "Filename", "start.point" and "end.point"
files2 <- as.character(files$V1)

start.end <- files[,2:3]

# P3

working.d <- "C:/Users/Sankey_Dan/Dropbox/Data_Online/Flights/ACC"

# remember to leave the original files in mydocuments. FOR NOW.

#############################

## SCRIPT ##

#############################


for (i in 1:length(files2)){

  setwd("~/")

  # Load data

  data <- read.csv(paste(files2[i], ".csv" , sep = ""))

  # 1. MANIPULATE TIME VECTOR - to remove miliseconds

  char.times = as.character(data[, 1])
  split.times <- str_split_fixed(char.times, "\\.", 2)

  head(split.times) # Now we have column without miliseconds


  ## 2. CROP


  data1 <- data[which(split.times[,1] == start.end[i,1])[1]:
                  which(split.times[,1]  == start.end[i,2])[1],]

  # 3. PLOT

  #plot(data1[seq(1,nrow(data1),40),2], type ="l")

  # 4. SAVE

  setwd( working.d)

  write.csv(data1 , file= paste(files2[i], ".csv" , sep = ""))

  print( paste ( "done with" , i , "/", length(files2)))

}
}
}
