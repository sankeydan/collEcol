#' Unique_flight_finder
#'
#' @param filenames enter filenames which you want to find flight specific information about
#' @param output start times?



uff = function( filenames, output = c("start.time", "flight.num")){
  library(stringr)
  foo = as.data.frame(str_split_fixed(filenames, "\\.", 17))
  foo2 = as.data.frame(paste(foo[,2], foo[,12], foo[,13], foo[,14]))

  load( file.path( PROJHOME, "smallDFs" , "unique_flight_information.rda"))

  if(output == "start.time"){

    d = rep( NA, length(foo2[,1]))
    for( i in 1:length(foo2[,1])){
      if( length(which( ufi$unique == as.character(foo2[i,1] ))) >0){
        d[i] = which( ufi$unique == as.character(foo2[i,1] ))
      }
    }
    return(ufi$start.time[ d])
  }

  if(output == "flight.num"){

   return(as.numeric(paste0(ufi$group.num[apply(foo2 , 1, function(x) which(ufi$unique == x))],
                         ".",
                         ufi$study.flight.num[apply(foo2 , 1, function(x) which(ufi$unique == x))])))
  }


}





