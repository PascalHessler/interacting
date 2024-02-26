#Functions

    
 #Function like adjust color, BUT, the color is not transparent, thanks to chatGPT
    adjustcolor2 <- function(col, dark) {
      new_cols=c()
      for (dj in dark)
      {
      rgb_val <- col2rgb(col)
      new_rgb_val <- rgb_val * dj + (1 - dj) * 255
      new_rgb_val <- pmax(0, pmin(255, new_rgb_val))
      new_col <- rgb(new_rgb_val[1], new_rgb_val[2], new_rgb_val[3], maxColorValue = 255)
      new_cols=c(new_cols,new_col)
      }
      return(new_cols)
    }

      
    
#1 Plot line by segments of width and color
 line.seg = function(x,y,lwd,col,g,lty,type='l')
          {
            n=length(x)
            for (k in 1:(n-1)) {
                lines(x=c(x[k],x[k+1]),y=c(y[k],y[k+1]),type=type,lwd=lwd[k],col=adjustcolor2(col,(g[k]+g[k+1])/2),lty=lty)
                }
            }
  

#================================================
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
  
    
#5 Get breaks
  get.breaks=function(cut_var)
  {
  intervals=levels(z_bins)
  start_end_points <- sapply(intervals, function(interval) {
    bounds <- gsub("\\((.+),(.+)\\]", "\\1-\\2", interval)
    as.numeric(strsplit(bounds, "-")[[1]])
  })
  start_end_matrix <- t(matrix(start_end_points, nrow = 2, byrow = FALSE))
  df=data.frame(start_end_matrix)
  names(df)=c("from","to")
  return(df)
  }

  