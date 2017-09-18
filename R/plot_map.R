#' plot_map
#'
#' A handy and interactive plotter for solo or group GPS tracks. Uses ggmaps to produce images or videos of animal trajectories on a googlemaps background.
#' @param data data array with dimensions time (rows), longitude / latitude (in that order; columns) & individual (3rd dimension). No third dimension necessary for solo plots.
#' @param type Solo or Group plots.
#' @param image.or.vid "image" or "video" (sequence of images) to be produced
#' @param tail.size animals represented by current position and the previous n positions (video only)
#' @param frame.size plot every row of data, or every 1/frame size for shorter running speeds and output files (video only)
#' @param zoom zoom = 19 is good for video. Other plot types are worth playing around with, try 13 for approx 5 km square
#' @param lines.or.points On the plotted map, animal trajectory represented by either "lines" or "points"
#' @param maptype "satellite" is default, though check ?get_map in the ggmap package for more options
#' @param wd Specify the working directory that you would like to save the plots into.
#' @param col Colour of plotted trajectory (solo only)
#' @param lwd Thickness of line
#' @param file.to.folder TRUE if you want to make videos when happy with other params, but for testing videos set to FALSE
#' @export



plot_map = function( data,  type = c("solo", "group") , image.or.vid = "video",
                     tail.size = 20, frame.size = 1, wd = getwd(), plot_name = "my_plot" ,
                     lines.or.points = "lines", col = "red" , lwd = 2, file.to.folder = T,
                     maptype = "satellite", zoom = 19 ){

  # data = data2
  # type = "group"
  # image.or.vid = "video"
  # tail.size = 20
  # file.to.folder = T
  # zoom = 19
  # frame.size = 5
  # maptype = "satellite"

  stopifnot(length(type) == 1 & length(image.or.vid) == 1)


  if ( type == "solo"){


    data = as.data.frame(data)
    names(data) = c("lon", "lat") # give the columns names

    centre = c ( (max( data[ ,1] ) + min( data [ ,1]) ) /2 , (max( data[ ,2] ) + min( data [ ,2]) ) /2 ) # find centre for map

    map = ggmap::get_map( location = centre,
                          zoom = zoom,
                          maptype = maptype) # load map
    p = ggmap::ggmap(map)

    p = p + ggplot2::geom_path (data= data ,  aes(x=lon, y=lat), color = col , size = lwd)

    print(p)
  }

  if ( type == "group"){

    for ( j in seq(tail.size,nrow(data),frame.size)){

      data2 = plyr::adply(data[(j-tail.size+1):j,1:2,],3)
      names(data2) = c("id" , "lon", "lat") # give the columns names

      centroid = data.frame ( lon = mean(data[j,1,]),
                              lat = mean(data[j,2,]))# A more advanced centriod algorithm is currently being updated for potential publication, email me and I can send it over though if you're interested.

      if( file.to.folder){
        png(file = file.path( wd , paste0( plot_name,  j , ".png")))
      }


      map = ggmap::get_map( location = c(centroid$lon,
                                         centroid$lat),
                            zoom = zoom,
                            maptype = maptype)

      p = ggmap::ggmap(map, extent = "device")  +
        theme(axis.line = element_blank(),
              axis.text  = element_blank(),
              axis.ticks = element_blank(),
              plot.margin = unit(c(0, 0, -1, -1), 'lines')) +
        xlab('') +
        ylab('')

      rain = rainbow(dim(data)[3])

      for ( k in 1:dim(data)[3]){
        sub = (((k-1)* tail.size)+1):(k * tail.size)
        p = p + ggplot2::geom_path (data= data2[sub,], aes(x=lon, y=lat), color= rain[k], size = 2)

      }

      print( paste ( j , "/" , nrow(data) ))
      print(suppressWarnings(p))

      if(file.to.folder){
        dev.off()
      }
    }
  }
}





























