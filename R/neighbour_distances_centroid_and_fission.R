# # Neighbour distances and fission.
#
#
# ndf = function ( fis.dist = 60 , hz = 5){
#
#
#   # unique group flight
#
#   filenames = files.chooser(folder = "mgGPS" , g_s = "g")
#   ugf = unique(uff( filenames , output = "flight.num"))
#
#   load( file.path ( PROJHOME, "smallDFs" , "metrics.rda"))
#
#   for ( i in 1:length(ugf)){ # for each unique flight
#
#     #  i = 39  # for the BIG ONE
#
#
# fff = 2
#
#     load( file.path ( PROJHOME ,  "data", "gGPS", paste0("g", ugf[i] , ".rda"))) # the object with which this file is called "data"
#     pigeon = names(data[1,1,])
#     dists.array = array( NA , c(nrow(data) , length(pigeon), length(pigeon) ), # each pigeon will have distance to each other pigeon
#                          dimnames = list(rep(NULL, nrow(data)), pigeon, pigeon)) # give correct names to dimensions
#
#
#     for ( j in 1:length(pigeon) ) {   # for each pigeon (as a focal)
#       for ( k in 1:length(pigeon) ) { # for each pigeon (neighbour)
#
#         if ( j != k){ # if focal is not neighbour
#
#           # use collEcol function get_dist() to calculate distance for each time step
#           dists.array[,j,k] = get_dist( data[,"lon", j], data[,"lat", j], data[,"lon" , k] , data[, "lat" ,k], method = "distance")
#         }
#       }
#     }
#
#     dists.array[1:10,,]
#
#     # centoid locator matrix
#     clm = array( NA, c( nrow(data) , length(pigeon) ))
#
#     for ( j in 1:length(pigeon)){
#
#       clm[,j] = apply( dists.array[,j,] , 1 , function(x) length( which(x < fis.dist)))
#     }
#
#     clm[1:10,]
#
#     #maximallyconnectedindivs
#
#     mcv =    apply(clm, 1, function(x) which(x == max(x)))
#
#     # centroid
#     centroid = array(NA, c(nrow(data), 2 ), dimnames = list(NULL, c("lon", "lat")))
#
#     for ( m in 1:nrow(data)){
#       centroid[m,1] = mean(data[m,"lon",mcv[[m]]]) # only take the mean of the guys in the biggest group to be out centroid
#       centroid[m,2] = mean(data[m,"lat",mcv[[m]]])
#     }
#
#     centroid[1:10,]
#
#     # add other parameters to centroid
#     centroid = cbind(centroid, time = data[,"time",1],
#                      head = get_head( lon1 =  centroid[,"lon"], lat1 = centroid[,"lat"]),
#                      speed = get_dist(lon1 =  centroid[,"lon"], lat1 = centroid[,"lat"], method = "speed" , hz = hz))
#
#     #put centroid into data
#
#     {dim(data)
#     foo = array(NA, dim(data) + c(0,0,1), dimnames = list ( NULL, names(data[1,,1]), c(names(data[1,1,]), "centroid")))
#     dim(foo)
#     cent.space = dim(data)[3]+1 # space for the centoid to slot into the array
#     foo[,,-cent.space] = data
#     foo[,,cent.space] = centroid
#     foo[1:5,,]
#     data = foo}
#
#     #  distance to the centroid
#
#     d2c = array( NA, c(nrow(data) , length(pigeon)))
#     mdtc = rep(NA, length(pigeon)) # mean dist2centroid
#
#     for( j in 1:length(pigeon)){
#       d2c[,j] = get_dist( data[,"lon", j], data[,"lat", j], centroid[,"lon"] , centroid[, "lat" ], method = "distance")
#       mdtc[j] = mean(d2c[,j])
#     }
#
#     d2c[1:10,]
#
#     # nice example plot
#
#
#     plot(d2c[,1] , main = ugf[i], type = "n" , ylim = c(0,80), xlab = "Time (s)" , cex.lab = 1.5, bty = "l", ylab = "Distance to centroid (m)")
#     abline( h = fis.dist , lty = 2)
#
#     load ( file.path( PROJHOME , "SmallDFs" , "pigeon_nums.rda"))
#
#     legend.col.vec = rep(NA, length(pigeon))
#
#     for ( n in 1:length(pigeon)){
#
#       lines(d2c[,n], col = pigeon_nums$colour[pigeon_nums$pigeon_nums == as.numeric(pigeon[n])])
#
#       if ( length( which  (pigeon_nums$pigeon_nums == as.numeric(pigeon[n]))) >0 ){
#         legend.col.vec[n]  = which  (pigeon_nums$pigeon_nums == as.numeric(pigeon[n]))
#       }
#
#     }
#
#
#     legend( "topleft"  , legend = pigeon , lty = 1 , col = pigeon_nums$colour[legend.col.vec  ] )
#
#
#     d2c2 = d2c[,-which(is.na(legend.col.vec) == T)]
#
#     # Save distance to centroid
#
#     assign ( paste0( "d2c_" , ugf[i]) , d2c)
#     save( d2c, file =  file.path(PROJHOME, "data", "d2c", paste0( "d2c_" , ugf[i] , ".rda")))
#
#
#     rm( list = paste0( "d2c_" , ugf[i] ))
#
#     # INDIVIDUAL FISSION INFORMATION
#
#     # num fission events
#     ne = rep (NA, length(pigeon))
#     # length fission events
#     lfe = rep (NA , length(pigeon))
#     # length between fission events
#     lbfe = rep ( NA , length(pigeon))
#     # proportion of time fissioned
#     ptf = rep(NA, length(pigeon))
#
#
#     for ( l in 1:length(pigeon)){
#
#       fis.times = which(d2c[,l] > fis.dist) # which timestanps are above threshold distance from centroid
#
#       fis.diff = if( length( fis.times) >0  ) fis.times[2:length(fis.times)] - fis.times[1:(length(fis.times)-1)]
#       if( length( fis.times) ==0  )
#         fis.diff = NA  # What are the time differences between the tmestamps?
#
#       if ( is.na(fis.diff[1]) == F){
#
#         if( length(which(fis.diff != 1)) > 0){
#
#           ne[l] =  length(which(fis.diff != 1)) + 1 # num of fission events
#
#           tfe = which(fis.diff != 1) # time of fission events
#           tfe2 = c(0, tfe, length(fis.diff))
#           lfe2= paste( tfe2[2:length(tfe2)] - tfe2[1:(length(tfe2)-1)]) # turning multiple values into vector of length 1.
#           # Best as a character that can be easily transformed
#           #back into numbers using eval(parse(text = CharacterVector))
#           character = c( "c( " ,rep (NA , (length(lfe2)*2-1)) , ")")
#           character[seq(2,length(lfe2)*2 , 2)] = lfe2
#           character[ seq(3 , (length(lfe2)*2-1), 2)] = ","
#           lfe[l] = paste(character , collapse = "")  # I realise this is the utimate faf!  But was stuck for how else to do it
#
#           ptf[l] = sum(eval (parse ( text = lfe[l]))) / nrow(data)
#
#           if( length(which(fis.diff != 1)) == 1){
#             lbfe[l] = as.character(fis.diff [ fis.diff !=1])
#           }
#
#           if( length(which(fis.diff != 1)) > 1){
#             lbfe2 = fis.diff [ fis.diff !=1] # length between fission events
#             character = c( "c( " ,rep (NA , (length(lbfe2)*2-1)) , ")")
#             character[seq(2,length(lbfe2)*2 , 2)] = lbfe2
#             character[ seq(3 , (length(lbfe2)*2-1), 2)] = ","
#             lbfe[l] = paste(character , collapse = "")
#           }
#         }
#
#         if ( length(which(fis.diff != 1)) == 0){
#           ne[l] = 1
#           lfe[l] = as.character(length(fis.diff))
#           ptf[l] = sum(eval (parse ( text = lfe[l]))) / nrow(data)
#           lbfe[l] = NA
#         }
#       }
#
#       if (is.na(fis.diff[1]) == T ){
#         ne[l] = 0
#         lfe[l] = NA
#         ptf[l] = 0
#         lbfe[l] = NA
#       }
#     }
#
#
#     load( file.path( PROJHOME, "smallDFs" ,"unique_flight_information.rda"))
#
#     head(ufi) # we are trying to get to $"filename" using the information from unique group flight (ugf)
#     group.file = ufi$filename [ paste0(ufi$group.num, ".", ufi$study.flight.num) == ugf[i]]
#     ind.files = paste0 ( rep( group.file, length(pigeon)),".",  pigeon) # then pasting pidgey name on the end
#
#     rows = metrics_row( ind.files, input = "ufi&id")
#
#     metrics$num.fission.events[rows] = ne
#     metrics$length.fission.events[rows] = lfe
#     metrics$proportion.time.fission[rows] = ptf
#     metrics$length.between.fission.events[rows] = lbfe
#     metrics$dist2centroid[rows] = mdtc
#
#     save( data, file = file.path(PROJHOME , "data/g&cGPS" , paste0("g&c" ,ugf[i], ".rda"))) # file.path() is the same as paste0 as far as I'm aware.
#
#
#     print(paste(  i, "/" , length(ugf) ))
#   }
#
#
#   save(metrics , file = file.path( PROJHOME, "smallDFs" , "metrics.rda"))
#
# }
