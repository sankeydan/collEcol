#' plot_traj
#'
#' A handy and interactive plotter for solo or group GPS tracks. No google image (unlike plot_map) so faster function with smaller output files.
#' @params data input array with dimensions: latitude, longitude, individual
#' @params type Solo or Group plots.
#' @params satellite True or False
#' @params satellite soom 19 is good for video
#' @export
#'

plot_traj = function( data, type = c("solo", "group") , snap.or.vid = "video",
                       borders = 800, tail.size = 20, frame.size = 1, grid.width  = 10,
                      png = T, hz = 5, lines.or.points = "lines",
                       f_u = "all" , plot.links = T , d2c.plot = T,
                      window = c("full", "tail.size") ){


  library(stringr)
  library(ggmap)
  library(plyr)

  if ( no.error.messages == T){


  if( type == "solo") {


  }

  if( type == "group"){



  }


  if( type == "group"){

    if( snap.or.vid == "video"){



        library(plotrix)

        subDir = paste0( type , "_noSat_" , "FrSz_",
                         frame.size, "_TlSz_", tail.size, "_links_" , plot.links)

        if (file.exists(file.path(PROJHOME, "videos", subDir)) == F){
          dir.create(file.path(PROJHOME, "videos", subDir))
        }

        # For metric distances, need to load the coodinates of home
        load( file.path(PROJHOME , "smallDFs", "release.site.coords.rda"))

        for ( i in 1:length(ugf)){
          for ( i in 18:length(ugf)){


            indiv = names(data[1,1,1:length(data[1,1,])-1])
            load ( file.path( PROJHOME , "SmallDFs" , "pigeon_nums.rda"))

            vec = rep(NA, length(indiv))

            for ( n in 1:length(indiv)){
              if ( length( which  (pigeon_nums$pigeon_nums == as.numeric(indiv[n]))) >0 ){
                vec[n]  = which  (pigeon_nums$pigeon_nums == as.numeric(indiv[n]))
              }
            }



            data = data[,,c(which (is.na(vec) == F), length(data[1,1,]))]

            pigeon = names(data[1,1,1:length(data[1,1,])-1])

            cols = 1:length(pigeon)

            for ( m in 1:length(pigeon)){
              cols[m] = pigeon_nums$colour[pigeon_nums$pigeon_nums == as.numeric(pigeon[m])]
            }


            data.met = array(NA, dim(data) + c(0,-3,0), dimnames =list(NULL,c("x", "y"),c(pigeon , "centroid") )) # for metric plots in metres

            homeLat = rep(rsc$lat[rsc$site == "home"], nrow(data))
            homeLon = rep(rsc$lon[rsc$site == "home"], nrow(data))

            for( l in 1:(length(pigeon)+1)){
              data.met[,"x",l] = get_dist(data[,"lon",l], homeLat, homeLon, homeLat, method = "distance")
              data.met[,"y",l] = get_dist(homeLon, data[,"lat",l], homeLon, homeLat, method = "distance")
            }


            if ( file.exists(file.path(PROJHOME, "videos", subDir,  ugf[i] )) == F){
              dir.create(file.path(PROJHOME, "videos", subDir,  ugf[i] ))
            }





            for ( j in seq(tail.size,nrow(data),frame.size)){
              # for ( j in 50:55){

              if( d2c.plot == T){
                width = 480
                height = 960
              }

              if( d2c.plot == F){
                par(mfrow = c(1,1))
                width = 480
                height = 480
              }


              #  if ( png == T){

              png(file = file.path( PROJHOME , "videos", subDir, ugf[i], paste0( j, ".png" )),
                  bg = "white", width = width , height = height)

              #  }

              if( d2c.plot == T){
                par(mfrow = c(2,1))
              }


              cent.lat = data.met[j,2,(length(pigeon)+1)]
              cent.lon = data.met[j,1,(length(pigeon)+1)]


              plot( cent.lat~ cent.lon ,
                    ylim = c((cent.lat - borders), (cent.lat + borders)), type = "n",
                    xlim = c((cent.lon - borders), (cent.lon + borders)), pch = 19, col = "blue")
              draw.circle (cent.lon, cent.lat, cent.radius , border = "red")

              leg.vec = rep(NA, length(pigeon)) # legend vector



              if ( plot.links == T){

                abline( h = seq( round(cent.lat, -1 ) - grid.width  - borders  , round(cent.lat, -1) + borders + grid.width , grid.width) , lty = "dotted" , col = "lightgrey")
                abline( v = seq( round(cent.lon, -1 ) - grid.width -  borders , round(cent.lon, -1)  + borders + grid.width , grid.width) , lty = "dotted" , col = "lightgrey")

                load ( file.path( PROJHOME , "data" , "links" , paste0( "links_" , ugf[i], ".rda")))

                for ( o in 1:length(pigeon)){

                  nei = as.character(eval(parse(text = links[j,o])))

                  if ( is.na(nei[1]) == F){
                    for ( p in 1:length(nei)){
                      segments( data.met[j, 1, pigeon[o]] , data.met[j, 2, pigeon[o]] , data.met[ j , 1, nei[p] ] , data.met [ j , 2, nei[p]])
                    }
                  }

                }

              }

              for ( m in 1:length(pigeon)){
                if ( lines.or.points == "points"){
                  points(data.met[j,2,m]~ data.met[j,1,m],
                         col = cols[m], cex = 1.2 , pch = 19)
                } else{
                  lines(data.met[(j-tail.size+1):j,2,m]~ data.met[(j-tail.size+1):j,1,m],
                        col = cols[m], lwd = 2)
                }
                if ( length( which  (pigeon_nums$pigeon_nums == as.numeric(pigeon[m]))) >0 ){
                  leg.vec[m]  = which  (pigeon_nums$pigeon_nums == as.numeric(pigeon[m]))
                }
              }



              legend( "topleft"  , legend = pigeon[order(pigeon)] ,
                      lty = 1 , col = pigeon_nums$colour[leg.vec][order(pigeon)])

              if ( d2c.plot == T){

                load ( file.path( PROJHOME , "data" , "d2c" , paste0("d2c_" , ugf[i] , ".rda")))

                plot(d2c[,1][seq(1,nrow(d2c),hz)], ylim = c(0,80), type = "n", xlab = "Time (s)" , cex.lab = 1.5, bty = "l", ylab = "Distance to centroid (m)")
                abline( h = cent.radius , lty = 2)
                abline( v = (j-tail.size+1)/hz )
                abline ( v = j /hz)

                for ( n in 1:length(pigeon)){
                  lines(d2c[,n][seq(1,nrow(d2c),hz)],  col = cols[n], lwd = 2)
                }

              }

              #if ( png == T) {
              dev.off()
              # }

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






















