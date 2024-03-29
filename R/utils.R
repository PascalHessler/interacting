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

 line.seg = function(x,y,lwd,col,g,lty=1,type='l')
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
  interval_matrix <- do.call(rbind, strsplit(gsub("\\[|\\]|\\(|\\)", "", levels(cut_var)), ",", fixed = TRUE))
  df_intervals <- as.data.frame(interval_matrix, stringsAsFactors = FALSE)
  df_intervals$V1 <- as.numeric(df_intervals$V1)
  df_intervals$V2 <- as.numeric(df_intervals$V2)
  names(df_intervals) <- c("from", "to")
  return(df_intervals)
  }
 
  

#----------------------------------
  
#Legacy functions
#6 Share

  share.within <- function(yk , y, within=.05 ) {
    return(mean( abs(y-yk) <=within))
    }

 
  
  
   rescale=function(x,min1,max1,drop.zeros=TRUE)
  {
   
    x=as.numeric(x)
    min0=min(x)
    max0=max(x)
    x1 = min1+((x-min0)*(max1-min1))/(max0-min0)
    
    if (drop.zeros) x1 = ifelse(x>0,x1,0)
    return(x1)
   }
   
   
#7 Add all vars at means
   add.covariates.at.mean=function(newdata, data)
   {
     #Add any variables missing at their mean
        missing_vars <- setdiff(names(data), names(newdata))
                   
    #Get means
        mean_values <- sapply(data[missing_vars], function(x) mean(x, na.rm = TRUE))

    #add them
        for(var in names(mean_values)) {
            newdata[[var]] <- mean_values[var]
            }
                   
    return(newdata)
     
    }
   
#8 Round2
  round2 <- function(x, min.d=2, max.d = 3) {
  # Convert to character to isolate the decimal part
    x_char <- as.character(x)
  
  # Find the first non-zero digit after the decimal
    fnz <- regexpr("[1-9]", sub(".*\\.", "", x_char))
  
  # Decide on the number of digits to round to
  # Use maxd if the first non-zero digit is beyond maxd digits, or if there are no non-zero digits within maxd places
    #d_round <- ifelse(fnz > max.d | fnz == -1, max.d, fnz + attr(fnz, "match.length") - 1)
    d_round = max(min(max.d, fnz), min.d)
  
  # Round the number
    rounded <- round(x, d_round)
    
  # force decimal
    rounded = formatC(rounded, format = "f", digits = min.d)
    return(rounded)
  }
  