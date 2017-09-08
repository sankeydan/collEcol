#' plot.pidge
#'
#' A handy and interactive plotter for solo or group GPS tracks
#' @params type Solo or Group plots.
#' @params satellite True or False
#' @params satellite soom 19 is good for video
#' @export
#'

plot_map = function( type = c("solo", "group") , image.or.vid = "video", no.error.messages = F,
                       borders = 800, tail.size = 20, frame.size = 1, grid.width  = 10, png = T,
                     hz = 5, lines.or.points = "lines",
                       f_u = "all" , finishers = T , plot.links = T ,  maptype = "satellite",
                     zoom = 19, d2c.plot = T, window = c("full", "tail.size"),
                     files = files.chooser(folder = "GPS") ){

  library(stringr)
  library(ggmap)
  library(plyr)

  if ( no.error.messages == T){

    if (zoom == 19){
      wps.lat = 0.0005 # Warning alerts if you don't use this scaler
      wps.lon = 0.0007
    }
  }

  if( type == "solo") files = files.chooser(folder = "mGPS", g_s = "s", f_u = f_u)

  if( type == "group"){

    ugf = unique(uff( files , output = "flight.num")) # unique group flight

  }




  if( type == "group"){

    if( image.or.vid == "video"){

        subDir = paste0( type , "_satZm_", zoom, "_FrSz_" ,
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
            # png(file = file.path( PROJHOME , "videos", subDir, ugf[i], j ))


            map = get_map( location = c(data[j,"lon","centroid"],
                                        data[j,"lat","centroid"]),
                           zoom = zoom,
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
            #  dev.off()
          }

        }



      }



  }
}






















