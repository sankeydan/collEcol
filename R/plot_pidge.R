#' plot.pidge
#'
#' A handy and interactive plotter for solo or group GPS tracks
#' @params type Solo or Group plots.
#' @params satellite True or False
#' @params satellite soom 19 is good for video
#' @export
#'

plot_pidge = function( type = c("solo", "group") , snap.or.vid = "video", no.error.messages = F,
                       borders = 800, tail.size = 20, frame.size = 1,
                       f_u = "all" , finishers = T , satellite = T, satellite.zoom = 19, d2c.plot = T, window = c("full", "tail.size"), cent.radius = 60, files = files.chooser(folder = "GPS") ){

  # type= "group"
  # snap.or.vid = "video"
  # no.error.messages = F
  # borders = 400
  # tail.size = 50
  # frame.size = 5
  # satellite = F
  # cent.radius = 60
  # files = files.chooser(folder = "mgGPS")
  # d2c.plot = T
  # window = "full"
  # finishers = T
  #
  library(stringr)
  library(ggmap)
  #library(collEcol)
  library(plyr)

  if ( no.error.messages == T){

    if (satellite.zoom == 19){
      wps.lat = 0.0005 # Warning alerts if you don't use this scaler
      wps.lon = 0.0007
    }
  }


  WHOAMI = "?"

  if( type == "solo") files = files.chooser(folder = "mGPS", g_s = "s", f_u = f_u)

  if( type == "group"){

    ugf = unique(uff( files , output = "flight.num")) # unique group flight

  }

  if ( satellite == T)  maptype = "satellite"


  if( type == "group"){

    if( snap.or.vid == "video"){

      if( satellite == T){

        subDir = paste0( type , "_satZm_", satellite.zoom, "_FrSz_" ,
                         frame.size, "_TlSz_", tail.size,"_", maptype)
        if (file.exists(file.path(PROJHOME, "videos", subDir)) == F){
          dir.create(file.path(PROJHOME, "videos", subDir))
        }
        #for ( i in 1:length(ugf)){
        for( i in 1){

          load( file.path ( PROJHOME ,  "data", "g&cGPS", paste0("g&c", ugf[i] , ".rda"))) # the object with which this file is called "data"
          num.indiv = length(data[1,1,])-1
          indiv = names(data[1,1,1:num.indiv])


          if ( file.exists(file.path(PROJHOME, "videos", subDir,  ugf[i] )) == F){
            dir.create(file.path(PROJHOME, "videos", subDir,  ugf[i] ))
          }

          for ( j in seq(tail.size,nrow(data),frame.size)){
            #for ( j in seq(tail.size,200,frame.size)){
            #for ( j in 570:nrow(data)){

            data2 = adply(data[(j-tail.size+1):j,c("lon","lat"),1:num.indiv],3)
            png(file = file.path( PROJHOME , "videos", subDir, ugf[i], j ))

            # colour
            rain = rainbow(num.indiv)

            map = get_map( location = c(data[j,"lon","centroid"],
                                        data[j,"lat","centroid"]),
                           zoom = satellite.zoom,
                           maptype = maptype)

            p = ggmap(map, extent = "device")  +
              theme(axis.line = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
              xlab('') +
              ylab('')
            if ( no.error.messages == T){
              for ( k in 1:num.indiv){
                sub = (((k-1)* tail.size)+1):(k * tail.size)
                data3 =  data2[sub[data2[sub,"lat"] <  data[j,"lat","centroid"]+wps.lat  &
                                     data2[sub,"lat"] >  data[j,"lat","centroid"]-wps.lat &
                                     data2[sub,"lon"] <  data[j,"lon","centroid"]+wps.lon  &
                                     data2[sub,"lon"] >  data[j,"lon","centroid"]-wps.lon ] ,]  # convoluted but this is basically to remove warning messages
                p = p + geom_path (data= data3, aes(x=lon, y=lat), color= rain[k], size = 2)

              }
            } else {


              for ( k in 1:num.indiv){
                sub = (((k-1)* tail.size)+1):(k * tail.size)
                p = p + geom_path (data= data2[sub,], aes(x=lon, y=lat), color= rain[k], size = 2)

              }
            }
            print(j)
            print(p)
            dev.off()
          }

        }



      }

      if( satellite == F){

        library(plotrix)
        subDir = paste0( type , "_noSat_" , "FrSz_",
                         frame.size, "_TlSz_", tail.size)

        if (file.exists(file.path(PROJHOME, "videos", subDir)) == F){
          dir.create(file.path(PROJHOME, "videos", subDir))
        }

        # For metric distances, need to load the coodinates of home
        load( file.path(PROJHOME , "smallDFs", "release.site.coords.rda"))

        for ( i in 1:length(ugf)){
          for ( i in 1){


            load( file.path ( PROJHOME ,  "data", "g&cGPS", paste0("g&c", ugf[i] , ".rda"))) # the object with which this file is called "data"
            indiv = names(data[1,1,1:num.indiv])
            load ( file.path( PROJHOME , "SmallDFs" , "pigeon_nums.rda"))


            num.indiv = length(data[1,1,])-1



            for ( m in 1:num.indiv){
              if ( length( which  (pigeon_nums$pigeon_nums == as.numeric(indiv[n]))) >0 ){
                vec[n]  = which  (pigeon_nums$pigeon_nums == as.numeric(indiv[n]))
              }
            }



            data.met = array(NA, dim(data) + c(0,-3,0), dimnames =list(NULL,c("x", "y"),NULL) ) # for metric plots in metres

            homeLat = rep(rsc$lat[rsc$site == "home"], nrow(data))
            homeLon = rep(rsc$lon[rsc$site == "home"], nrow(data))

            for( l in 1:(num.indiv + 1)){
              data.met[,"x",l] = get_dist(data[,"lon",l], homeLat, homeLon, homeLat, method = "distance")
              data.met[,"y",l] = get_dist(homeLon, data[,"lat",l], homeLon, homeLat, method = "distance")
            }


            if ( file.exists(file.path(PROJHOME, "videos", subDir,  ugf[i] )) == F){
              dir.create(file.path(PROJHOME, "videos", subDir,  ugf[i] ))
            }





            for ( j in seq(tail.size,nrow(data),frame.size)){

              # png(file = file.path( PROJHOME , "videos", subDir, ugf[i], paste0( j, ".png" )))

              cent.lat = data.met[j,2,(num.indiv+1)]
              cent.lon = data.met[j,1,(num.indiv+1)]

              if( d2c.plot == T){
                par(mfrow = c(2,1))
              }
              plot( cent.lat~ cent.lon ,
                    ylim = c((cent.lat - borders), (cent.lat + borders)), type = "n",
                    xlim = c((cent.lon - borders), (cent.lon + borders)), pch = 19, col = "blue")
              draw.circle (cent.lon, cent.lat, cent.radius , border = "red")

              leg.vec = rep(NA, length(pigeon)) # legend vector

              for ( m in 1:num.indiv){
                lines(data.met[(j-tail.size+1):j,2,m]~ data.met[(j-tail.size+1):j,1,m],
                      col = pigeon_nums$colour[pigeon_nums$pigeon_nums == as.numeric(pigeon[m])], lwd = 2)
                if ( length( which  (pigeon_nums$pigeon_nums == as.numeric(pigeon[m]))) >0 ){
                  leg.vec[m]  = which  (pigeon_nums$pigeon_nums == as.numeric(pigeon[m]))
                }
              }


              legend( "topleft"  , legend = sort(pigeon [ which (is.na(leg.vec) == F)]),
                      lty = 1 , col = pigeon_nums$colour[leg.vec][ which (is.na(leg.vec) == F)])

              if ( d2c.plot == T){

                load ( file.path( PROJHOME , "data" , "d2c" , paste0("d2c_" , ugf[i] , ".rda")))

                plot(d2c[,1][seq(1,nrow(d2c),hz)], col = rain[1], ylim = c(0,80), type = "n", xlab = "Time (s)" , cex.lab = 1.5, bty = "l", ylab = "Distance to centroid (m)")
                abline( h = fis.dist , lty = 2)
                abline( v = (j-tail.size+1)/hz )
                abline ( v = j /hz)

                for ( n in 1:length(pigeon)){
                  lines(d2c[,n][seq(1,nrow(d2c),hz)],  col = pigeon_nums$colour[pigeon_nums$pigeon_nums == as.numeric(pigeon[n])], lwd = 2)
                }

              }

              # dev.off()

              # print(paste( j , "/" , nrow(data)))
            }

            print(paste(i , "/" , length(ugf)) )



            # colour




          }
        }
      }
    }
  }
}














# if( type = "solo"){
#
#
#   for(i in 1:length(files)){
#
#     smalldf()
#     uff( files[i])
#     files = files.chooser(folder = "mGPS", g_s = "s")
#
#     png(file = paste0( type , files[i] ,".png"))
#
#     setwd("C:/Users/Dan/Dropbox/Data_Online/Flights/mGPS") ### Optional change to F2
#
#     data <- read.csv(files[i])
#
#     range.max = max(c(range (data$LATITUDE), range(data$LONGITUDE)))
#
#     plot.size = ifelse(range.max > 3434343434, 10,
#                        ifelse(range.max > 34343, 11, 12))
#
#     map <- get_map(location = c(((max(data$LONGITUDE) + min(data$LONGITUDE))/2),
#                                 ((max(data$LATITUDE) + min(data$LATITUDE))/2)), zoom = plot.size, maptype = "satellite")
#     print(ggmap(map) + geom_point(data = data , aes(x = data$LONGITUDE, y = data$LATITUDE), col = "red",  size=1)+
#             ggtitle(paste( "Pigeon =", str_split_fixed(files[i], "\\.", 18)[,frfrfrfr]," ", "Flight =", i ,sep=""))
#     )
#     print( paste( "done with" ,i  , "/", length(unique(Flight.num))))
#
#   }
#   setwd("C:/Users/Dan/Dropbox/Data_Online/Video_Folders/Sol.F1/Solo_traces")
#   dev.off()











