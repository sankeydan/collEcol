#' gGPS
#'
#'
#' This function is slow and can be optimised by not loading the entire csv. I.e. only the stuff I need.
#'



# combining the group GPSs into new RDAs for analysis and plots


gGPS = function(){

  # Set working directory

  setwd( file.path( PROJHOME, "data" , "mgGPS"))

  # libraries

  library(collEcol)
  library(chron)

  # load files

  load ( file.path( PROJHOME , "smallDFs" , "metrics.rda" ))

  # choose files

  gfiles = files.chooser(folder = "mgGPS")

  # Which unique flight does each file correspond to?

  flight.nums = uff(files.chooser(folder= "mgGPS"), output = "flight.num")

  # unique for for loop - long script maybe try to reduce

  uniq = rep(NA, length(unique(flight.nums)))

  for ( i in 1:length(uniq)){
    uniq[i] = which(flight.nums == unique(flight.nums)[i])[1]
  }

  uniq2 = c(uniq , length(flight.nums))

  # Pigeons per event
  ppe = uniq2[2:length(uniq2)]-uniq

 # obj. called "data"


  for ( i in 1:(length(uniq2)-1)){ # for each unique group flight event



    #for ( i in 19: 46){

    load(gfiles[1])

    start.time = rep(  as.POSIXct( paste(data$UTC.DATE[1], data$UTC.TIME[1])),  ppe[i]) # This is necessary to set the "class" of start.time
    nrows = rep(NA , ppe[i])
    for ( j in 1:ppe[i]){ # for each pigeon in the flock


      load(gfiles[uniq2[i]+j-1])
      assign ( paste0( "P" ,j), data) # assign their data to an object

      date = get(paste0("P",j ))$UTC.DATE


      if ( length(levels(date))!= 1) { # correcting for a bug where the data comes out as "UTC DATE"

        assign ( paste0( "P" ,j), get(paste0( "P" ,j))[date == names(table (date)[rev(order(table(date)))[1]]),])
           }


     start.time[j] =  as.POSIXct( paste(get(paste0( "P" ,j))$UTC.DATE[1], get(paste0( "P" ,j))$UTC.TIME[1])) # and Find their start time
     nrows[j] = metrics$cut_end[metrics_row( gfiles[uniq2[i]+j-1])]
       }


    max.start = max(start.time)+1 # Find the MAx start time, and +1 because there are 5hz for every second in our data, and "max.start" may not correspond to the first HZ. Important to start each individual on the first HZ of the second.


    na.vec = rep(0, ppe[i]) # This is for when the pigeon's time never matches with max.start.

    for ( j in 1:ppe[i]){ # for each pigeon in the flock
      foo =  which( as.POSIXct   ( paste ( get(paste0("P", j))$UTC.DATE  ,
                                           get(paste0("P", j))$UTC.TIME ))
                                                            == max.start)[1] # horrible code, but works. Which time == max.start
      if( is.na(foo) == F){
      assign( paste0("P",j) , get(paste0("P", j))[ foo:nrow( get(paste0("P", j))),])
      }

      na.vec[j] = foo
    }

    mp = which(is.na(na.vec)) # missing pigeon(s)

    #################
    files2 =  gfiles[uniq2[i]:(uniq2[i+1]-1)]
    rp = 1:ppe[i] # remaining pigeons. Cant ever get if else functions to work so I have
                 # done a cowboy tactic here, maybe come back and clean up later

    if (length(mp) > 0 ){
      rp = (1:ppe[i])[-mp] # remaining pigeons
      files2 =  gfiles[uniq2[i]:(uniq2[i+1]-1)][-mp]
    }

    ##################


    maxnrows = max(na.omit( nrows))     # How many rows?



    for ( j in rp){  # for each pigeon in the flock minus missing pigeons ( remaining pigeons)
      assign( paste0("P",j) , get(paste0("P", j))[ 1: maxnrows ,])
    }






    #build array
    assign( paste0("g" ,flight.nums[uniq][i]) , # specific name for each group flight
            array(NA, c( maxnrows, 5, (ppe[i]-length(mp))), # rows, variables, pigeon
            dimnames = list(NULL, # dimnames , null for row
               c("lon", "lat" , "time" , "head" , "speed"), # variable names
              c(paste( metrics_row(file = files2 , output = "pigeon")))))) # pigeon names
    g.temp = array(NA, c(maxnrows, 5, (ppe[i]-length(mp)))) # and a temporary array. (this is because we cannot use get() function to assign data in loop below)

    for ( j in rp){ # for each pigeon in the flock, add the following elements to the array

      g.temp[,1,j] = get(paste0("P", rp[j]))$LONGITUDE # Longitude / x
      g.temp[,2,j] = get(paste0("P", rp[j]))$LATITUDE # Latitude / y
      #g.temp[,3,j] =
      #as.integer(as.POSIXct( paste(get(paste0( "P" ,rp[j]))$UTC.DATE, get(paste0( "P" ,rp[j]))$UTC.TIME))) # seconds since 1970  }
      g.temp[,4,j] = get(paste0("P", rp[j]))$head # Heading
      g.temp[,5,j] = get(paste0("P", rp[j]))$speed # Speed (m/s)

    }

    for ( j in 1:length(rp)){ # for each pigeon
      for ( k in 1:length(g.temp[1,,1])){ # For the number of variables exracted from the original dataframes

        eval(parse(text=paste(paste0("g" ,flight.nums[uniq][i] , "[,k,j]") , "=" , "g.temp[,k,j]")))
        # This is a really nice bit of code which eliminates the problem of not being able to assign to a get() object
      }
    }

    data = get(paste0("g" ,flight.nums[uniq][i]) ) # cannot save a get() function.

    save( data, file = file.path(PROJHOME , "data/gGPS" , paste0("g" ,flight.nums[uniq][i], ".rda"))) # file.path() is the same as paste0 as far as I'm aware.
    rm(list = eval(paste0("g" ,flight.nums[uniq][i])))

    print( paste( "done with" , i, "/" , (length(uniq2)-1)))
  }

}




