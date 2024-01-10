#Functions



#1 share.within
  share.within <- function(yk , y, within=.05 ) {
    return(mean( abs(y-yk) <=within))
    
}

  
#2 Rescale
  rescale=function(x,min1,max1,drop.zeros=TRUE)
  {
   
    x=as.numeric(x)
    min0=min(x)
    max0=max(x)
    x1 = min1+((x-min0)*(max1-min1))/(max0-min0)
    
    if (drop.zeros) x1 = ifelse(x>0,x1,0)
    return(x1)
  }
  
  
#3 point.seq  - plot line by segments of width

  line.seg = function(x,y,lwd,col)
  {
    n=length(x)
    for (k in 1:(n-1))
        {
        lines(x=c(x[k],x[k+1]),y=c(y[k],y[k+1]),type='l',lwd=lwd[k],col=col)
        }
    
  }
  
  
  
  