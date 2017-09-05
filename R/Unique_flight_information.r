# unique flight information

# Finding the start time of all flights.

# rm(list=ls())

# load all filenames and start.points from excel ACC folder.

ufi = function (){

  library(collEcol)
  library(stringr)

  start = read.csv( file.path( PROJHOME, "data" , "metadata" , "Start.times.csv"), header =  T)
  start$Filename = as.character(start$Filename)
  start$Start.point = as.character(substring(start$Start.point, 2 , 20))


  # Find unique flight events

  filename.split = as.data.frame(str_split_fixed(start$Filename, "\\.", 17))
  filename.split$unique = paste(filename.split[,2], filename.split[,13], filename.split[,14], filename.split[,15])


  # Unique flight information

  ufi = data.frame( start.time = rep(NA,length(unique(filename.split$unique))),
                    site = rep(NA,length(unique(filename.split$unique))),
                    flight = rep(NA,length(unique(filename.split$unique))))
  vec = rep(NA, length(unique(filename.split$unique)))

  # change point between group and solo

    cgs  = which(abs(as.numeric(filename.split[2:nrow(filename.split),4]) - as.numeric(filename.split[1:nrow(filename.split)-1,4])) ==  1)[c(1,3,7,9)] +1
    # add one for when solo changes to group. will need to do this for second familiar site too. First is 836. The one after will be skip one from there

     site2 = as.numeric(as.character(filename.split[,6]))

  for( i in cgs){ # WATCH OUT!!!! added 836

    site2[i:length(site2)] =   site2[i:length(site2)] + 1
  }


  for ( i in  1:length(unique(filename.split$unique))){

    ufi$start.time[i] = min(start$Start.point[filename.split$unique == unique(filename.split$unique)[i]])
    ufi$site[i] = filename.split[which(filename.split$unique == unique(filename.split$unique)[i])[1],6]
    ufi$site2[i] = site2[which(filename.split$unique == unique(filename.split$unique)[i])[1]]
    vec [ i] = (which(filename.split$unique == unique(filename.split$unique)[i])[1])
    ufi$filename[i] = paste(as.matrix(filename.split[which(filename.split$unique == unique(filename.split$unique)[i])[1],1:9][1,]), collapse = ".")
    }

  ufi$flight = as.numeric(as.character(filename.split[vec,9]))

  ufi$unique = unique(filename.split$unique)



  ufi$group.num = as.numeric(str_split_fixed(ufi$unique, " ", 4)[,1])

  # day month flightofday
  dmf = matrix(as.numeric(str_split_fixed(ufi$unique, " ", 4)[,2:4]), ncol = 3)

  ufi = ufi[order(dmf[,2], dmf[,1], dmf[,3]),]

  flight.per.site = c(0,12,6,12,6,1,1,1,1,1,1,1,1,1,1,3,3,3,3)


  for(i in 1:nrow(ufi)){

  ufi$study.flight.num[i] = sum(flight.per.site[1:ufi$site2[i]]) + ufi$flight [i]

  }


  # adding flights where no accelerometers were present.
  #

  extra.file = files.chooser( group.num = 1 , g_s = "g", site = 1, flight.nums = 1, output = "filenames")[3]

  data = read.csv( file.path ( PROJHOME, "data" , "GPS", extra.file), header = T)
  nrow(data)
  data2 = data[8000:nrow(data),]
  #plot(data2$LATITUDE)

  start = 1

  ufi2 = rbind(ufi ,c(paste( "2017-07-01", as.character(data2$UTC.TIME[start])), 1, 1, 2, substr(extra.file, 1, 22), "1 1 7 1",1,13 ))

  ufi3 = ufi2[c(1:27, nrow(ufi)+1,28:nrow(ufi)),]



  ufi = ufi3

  ufi$unique = as.character(ufi$unique)



  # convert to UTC



  ufi$start.time =  c(as.POSIXct (ufi$start.time[c(1:27)])-3600,
                      as.POSIXct(ufi$start.time[c(28)]),
                      as.POSIXct(ufi$start.time[c(29:nrow(ufi))])-3600)

  # save


  save(ufi,  file = file.path ( PROJHOME, "smallDFs" , "unique_flight_information.rda"))

}


