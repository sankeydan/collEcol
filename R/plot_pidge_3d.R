#' plot_pidge_3d
#' 
#' @param d3 an array of flight data structured in three dimensions 1.time, 2.(x,y,z), 3.id
#' @param box.size size of the box around the data
#' @param tail.size the size of the pigeons tails
#' @param frame.size to subset the dataset (larger numbers use less memory)
#' 

plot_pidge_3d  = function(d3 , box.size = 40, tail.size = 40, frame.size = 2,
                          prev.wd = getwd() , lwd = 2,  wd, grid = "equal" , axes = F, axis.lab = F) { 
  
  # working directory
  setwd(wd)
  
  sequence1 = seq(1, length(d3[,1,1])-tail.size-frame.size, frame.size)
  sequence2 = seq(tail.size, length(d3[,1,1])-frame.size, frame.size)
  
  cols = rainbow(nop)
  
  for( i in 1:(length(sequence1)-1)){
    sq1 = sequence1[i]
    sq2 = sequence2[i]
    
    if ( grid == "equal"){
      zll = centroid[sq2,3]-box.size # z lower limit
    }  
    
    if( grid == "ground"){
      zll = 0
    }
    
    if( axis.lab == T){
      xl = "x"
      yl = "y"
      zl = "z"
    }
    
    if ( axis.lab == F){
      xl = ""
      yl = ""
      zl = ""
    }
    
    open3d(windowRect=c(10,10,1000,1000))
    plot3d(d3[sq1:sq2,1,1],
           d3[sq1:sq2,2,1],
           d3[sq1:sq2,3,1], 
           xlab= xl,
           ylab = yl,
           zlab = zl,
           lwd = lwd,
           type = "l",
           add= F,
           axes = F,
           xlim = c(centroid[sq2,1]-box.size,
                    centroid[sq2,1]+box.size),
           ylim = c(centroid[sq2,2]-box.size,
                    centroid[sq2,2]+box.size),
           zlim = c(zll,
                    centroid[sq2,3]+box.size))
    
    if(axes == T){
    axes3d( labels = T)
    }
    
    for ( j in 1:nop){
      lines3d(d3[sq1:sq2,1,j],
              d3[sq1:sq2,2,j],
              d3[sq1:sq2,3,j], 
              col = cols[j],
              lwd = lwd)
    } 
    grid3d(side = "z", at = seq(-10000, 10000,10))
    
    snapshot3d( paste( i, "plot.png"))
    rgl.close()
  }
  setwd(prev.wd)
}

