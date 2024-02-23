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
  line.seg = function(x,y,lwd,col,g,lty=1)
  {
    n=length(x)
    for (k in 1:(n-1))
        {
        lines(x=c(x[k],x[k+1]-.05*(x[k+1]-x[k])),y=c(y[k],y[k+1]),type='l',lwd=lwd[k],col=adjustcolor(col,g[k]),lty=lty)
        
        }
    
  }
  
#4 MESSAGING
    format_msg <- function(msg,width=70, header='IMPORTANT.', pre="| ")
    {
    #Line counter
    j<-0
    #Lines with formatted message starts empty
      msg.lines=c()
    #Turn message into vector of words
      msg.left <- strsplit(msg,' ')[[1]]

    #Loop over lines
      while (length(msg.left)>0)
      {
     j=j+1
     msg.lines[j]=''

    #loop over words
      while (nchar(msg.lines[j]) + nchar(msg.left[1]) <width)
      {
      new.word <- msg.left[1]
      msg.left <- msg.left[-1]
      if (regexpr('\n', new.word)>0) break   #skip line if \n is found
      msg.lines[j] <- paste0(msg.lines[j],new.word," ")   #add the next word
      
      if (length(msg.left)==0) break
    }
      msg.lines[j]<- paste0(pre,"    ", msg.lines[j] ) 
      if (length(msg.left)==0) break
    }
      
  #formatted 
    #Add |  
      msg.lines <- gsub("\n", "\n|", msg.lines)
      
      
    #Join al
      msg.formatted <- paste0(msg.lines,collapse="\n")
      
    #Add header
      msg.formatted <- paste0(pre,header,"\n",msg.formatted)
      
    #Add ------------- on top
      sep.line <- c(paste0(rep('-',width+5)) , "\n" )
      msg.formatted<-c(sep.line, msg.formatted)
    
    return(msg.formatted)
    }

   exit <- function(...) {
    message(...)
    invokeRestart("abort")
    }
  
    gstop <- function(msg,format=FALSE) {
    #Format the message with line breaks and border if requested
    if (format==TRUE) msg=format_msg(msg) 
    message(msg)
    message("----------------------------------------")
    exit()
    }
  
  