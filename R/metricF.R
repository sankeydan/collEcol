#' metricF
#'
#' A function to produce metrics for each flight. eg. the accuracy of the flight path. Works in the mGPS folder.
#'
#' @param accuracy the accuracy of the path compared to the beeline
#' @param files Choose the files you want to perform functions on. USe files.chooser( g_s = "g") to get groups for example. Do not change folder = "mGPS"
#' @param time.taken The number of rows of data divided by the hz of the logger

metricF = function(accuracy = T, speed = T, time.taken = T, min.speed = 5, hz = 5, files = files.chooser(folder = "mGPS")){
  #

  # accuracy = T
  # speed = T
  # min.speed = 5
  # hz = 5
  # time.taken = T
  # files = files.chooser(folder = "mGPS" , g_s = "g")



  # libraries

  library(stringr)

  # script


  load( file.path( PROJHOME , "smallDFs" , "release.site.coords.rda"))
  load( file.path( PROJHOME , "smallDFs" , "metrics.rda"))

  for( i in 1:length(files)){

    load( file.path ( PROJHOME , "data",  "mGPS" ,  files[i])) # read the data

    #plot
    # sq = seq(1,length(data$dist2site),100)
    # plot(data$LATITUDE~data$LONGITUDE,main = i, xlim = c(-0.9,-0.3), ylim = c(51.35, 51.55 ) )
    # points(home$lat ~ home$lon, col = "red", pch = 2)
    # points(site$lat ~ site$lon, col = "blue", pch = 2)

    v1 = str_split_fixed(files[i], "\\.", 16)

    if(accuracy == T){

      metrics$accuracy[metrics_row(files[i])] =  (rsc$beeline_200[rsc$site == paste(v1[,8], v1[,7], sep = "")]/sum(data$speed))*hz
      metrics$total.dist[metrics_row(files[i])] = sum(data$speed)/hz
    }



    if(speed == T){

      metrics$mean.speed[metrics_row(files[i])] = mean(data$speed[data$speed > min.speed])
      metrics$time.stationary[metrics_row(files[i])] = length(which(data$speed < min.speed))/hz
    }

    if( time.taken == T ){

      metrics$time.taken[metrics_row(files[i])] = nrow(data)/hz
    }

    print(paste( "done with" , i , "/" ,length(files)))
    metrics$files_i[metrics_row(files[i])] = i
  }

  save(metrics, file = file.path ( PROJHOME , "smallDFs" , "metrics.rda"))

}


