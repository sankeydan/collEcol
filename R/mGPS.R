#' mGPS
#'
#' A function which modifies raw GPS files and outputs in the mGPS folder

#' @param lat_lon2numeric necessary when NAs present in data
#' @param correct_lon Multiply the output data by negative 1. Longitude data from my GPS is on the wrong side of zero.
#' @param correct_time Sometimes the GPS saves random additional files. This will get rid of those.
#' @param get_speed Speed in m/s
#' @param get_heading Heading for each individual's trace.
#' @param cut_files Cut the files at a radius from the release site and home
#' @param cut_radius is the cut radius in metres
#'

mGPS = function(lat_lon2numeric = T, correct_time = T, correct_lon = T, get_speed = T, get_heading = T, cut_start = T, cut_end = T, cut_radius = 200, files = files.chooser(output = "filenames"), save = T, return_data = F){

  # lat_lon2numeric = T
  # correct_time = T
  # correct_lon = T
  # get_speed = T
  # get_heading = T
  # cut_start = T
  # cut_end = T
  # cut_radius = 200
  # files = files.chooser(output = "filenames", g_s = "g", site = 14 , flight =  3 , direction = "N")
  # save = T
  # return_data = F

  # Libraries

  library(collEcol)
  library(stringr)
  library(chron)

  library(collEcol)
  library(stringr)
  library(chron)

  # Files



  load( file.path( PROJHOME, "smallDFs" , "unique_flight_information.rda"))
  load( file.path( PROJHOME, "smallDFs" , "metrics.rda"))
  load( file.path( PROJHOME, "smallDFs" , "release.site.coords.rda"))


  # Script


  for ( i in 1:length(files)){


    data = read.csv( file.path (PROJHOME, "data" , "GPS", files[i]), header = T )


    if(is.null(data$link1_href) == F){ # lin1_href is just a random variable output by "my_geoconverter (which I paid for to get the GPX file converted into a CSV)
      data$LATITUDE = data$Y
      data$LONGITUDE = data$X
      data$UTC.DATE = str_sub( data$time, 1,10)
      data$UTC.TIME = str_sub( data$time, 12, 19)
    }

    data = as.data.frame(data [, c("LONGITUDE" , "LATITUDE" , "UTC.DATE" , "UTC.TIME") ])

    if(lat_lon2numeric == T){
      data$LATITUDE = as.numeric(as.character(data$LATITUDE))
      data$LONGITUDE = as.numeric(as.character(data$LONGITUDE))
    }

    if(correct_lon == T){
      data$LONGITUDE = -abs(as.numeric(data$LONGITUDE))
    }

    if(correct_time == T){

      if( length(which( data$UTC.TIME == "UTC TIME")) != 0){
        data$UTC.TIME[data$UTC.TIME == "UTC TIME"] = NA
      }

    }



    if(get_speed == T){
      data$speed = get_dist(data$LONGITUDE, data$LATITUDE, method = "speed", hz = 5)
    }

    if(get_heading == T){
      data$head = get_head(data$LATITUDE, data$LONGITUDE)
    }


    if(cut_start == T){

      if(length(which( times(data$UTC.TIME) == times(str_sub(uff(files[i], output = "start.time"), -8,-1))))[1] > 0){  # This cuts the data at the unique flight minimum time
        data = data[ which( times(data$UTC.TIME) == times(str_sub(uff(files[i], output = "start.time"), -8,-1)))[1]: nrow(data),]
        timefault = F
      }

      if(length(which( times(data$UTC.TIME) == times(str_sub(uff(files[i], output = "start.time"), -8,-1))))[1] == 0){  # This cuts the data at the unique flight minimum time
        print(paste( "issues with time. File = " , files[i]))
        timefault = T
      }

      site = rsc[rsc$site == paste(str_split_fixed(files[i], "\\.", 16)[,8],
                                   str_split_fixed(files[i], "\\.", 16)[,7], sep = ""),]
      home = rsc[rsc$site == "home",]
      data$dist2site = get_dist(data$LONGITUDE, data$LATITUDE, rep(site$lon,nrow(data)), rep(site$lat,nrow(data)), method = "distance" )
      data$dist2home = get_dist(data$LONGITUDE, data$LATITUDE, rep(home$lon,nrow(data)), rep(home$lat,nrow(data)), method = "distance" )


      if(is.na(which(data$dist2site > cut_radius)[1]) == F){
        data = data[which(data$dist2site > cut_radius)[1]:nrow(data),]
      }
    }

    if(cut_end == T){

      if(is.na(which(data$dist2home < cut_radius)[1]) == F){
        data = data[1:which(data$dist2home < cut_radius)[1],]
      }
      if(save == T & timefault == F & is.na(which(data$dist2site > cut_radius)[1]) == F & is.na(which(data$dist2home < cut_radius)[1]) == F){
        save(data, file = file.path( PROJHOME, "data", "mGPS", paste0(substr(files[i],1,nchar(files[i])-4) , ".rda"))) # modified GPS (mGPS)

      }
    }

    if(cut_end == F){

      if(is.na(which(data$dist2home < cut_radius)[1]) == F){
        load( file.path ( PROJHOME , "smallDFs" , "metrics.rda"))
        metrics$cut_end[metrics_row(files[i])] =  which(data$dist2home < cut_radius)[1]
        save( metrics , file = file.path ( PROJHOME , "smallDFs" , "metrics.rda"))
      }
      if(save == T & timefault == F & is.na(which(data$dist2site > cut_radius)[1]) == F & is.na(which(data$dist2home < cut_radius)[1]) == F){
        save(data, file = file.path( PROJHOME, "data", "mgGPS", paste0(substr(files[i],1,nchar(files[i])-4), ".rda"))) # modified GPS for group flights (mgGPS)

      }

    }

    # #plot
    # sq = seq(1,length(data$dist2site),100)
    # plot(data$LATITUDE~data$LONGITUDE,main = i, xlim = c(-0.9,-0.3), ylim = c(51.35, 51.55 ) )
    # points(home$lat ~ home$lon, col = "red", pch = 2)
    # points(site$lat ~ site$lon, col = "blue", pch = 2)









  if( return_data == T){
    return(data)
  }


  print(paste("done with", which(files == files[i]) , "/" , length(files)))

  #plot(data$dist2home)
  if(is.na(which(data$dist2site > cut_radius)[1]) == T | is.na(which(data$dist2home < cut_radius)[1]) == T){

    print(paste("battery ran out or data logger filled before the pigeon got home. Have saved metrics$accuracy as NA.  File =" , files[i] ))
    file = files[i]

    metrics$accuracy[metrics_row(file)] = NA
    save(metrics, file = file.path( PROJHOME, "smallDFs", "metrics.rda"))
  }

  }

print( "please ignore any error messages, NAs are creating this message and are from GPS signal error")

}




