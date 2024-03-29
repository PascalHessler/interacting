

  #  plot.x_on_axis = function(xlab,ylab,main, res , histogram, data,xs, gr,spotlights,cols,spotlight.labels,focal,moderation,nux,max.unique,fxz.list)

    make.plot     = function(xlab,ylab,main, res , histogram, data,xs,zs, gr, spotlights , cols , spotlight.labels , focal , moderation , nux1 , max.unique , fxz.list)
          {   
      #res: list with results from either simple.slopes or floodlight 
      
      #main is entered in call within interprobe(), specifying GAM Simple Slpoes vs GAM floodlight
      
      
    #1 Adjustments based on x or z on the x1 axis
        #X on AXIS    
            if ( focal!='categorical') {
              if (xlab=='') xlab='Focal Predictor'
              n.lines=length(spotlights)
              x1.range = range(data$x)
              x1s=xs
              #x2s=zs
            }
             
      #Z on Axis 
          if (focal=='categorical') {
            if (xlab=='') xlab='Moderator'
            n.lines=nux
            x1.range = range(data$z)
            x1s=zs
           #x2s=zs

            }
      
    #2 Unlist data.frames
           res.df <- do.call(rbind, res)
 
    #3 Set ylim
            ylim = range(res.df[,c('conf.low','conf.high')]) #Default y-range
            ylim[2]=ylim[2]+.35*diff(ylim)                   #Add at the top for the legend
            if (histogram==TRUE) ylim[1]=ylim[1]- n.lines*.1*diff(ylim)        #add at the bottom for the histogram
          
    #4 Set x-lim
            xlim = x1.range
            #xlim[1]=xlim[1]-.05*diff(xlim) #add margin to left to put the 'n=' 
            
    #5 Empty plot
            plot(x1s,res[[1]]$estimate,type='n',xlab=xlab,ylab=ylab,las=1,ylim=ylim,xlim=xlim,yaxt='n',cex.lab=1.3,font.lab=2)
            axis(2,at=pretty(ylim)[c(-1,-2)],las=1) #y-axis missing lower two ticks to give space to the histogram
         
            
          #Loop through the spotlights
               
              for (j in 1:n.lines) {
                 
               #Color of lines
                #Based on frequncies
                  g1=as.numeric(gr[,j])  #this is based on fxz
                
                #If continuous, expand to set of values colored together
                  if (nux>max.unique)  g = rep(g1 , each=length(x1s)/length(g1))
                  
                #If discrete then just the values once (one line per bin-pair)
                  if (nux<=max.unique) g = g1
                    
              
                #Lines
                  line.seg(x1s,res[[j]]$estimate,lwd=4*g, col=cols[j],g=g) 
              
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(x1s,rev(x1s)),
                        y=c(res[[j]]$conf.high,
                            rev(res[[j]]$conf.low)),
                            col=adjustcolor(cols[j],.1),border = NA)
                  
               #Dots within line if we have not binned data for plotting
                  if (nux <= max.unique) points(x1s,res[[j]]$estimate, col=adjustcolor2(cols[j],g),pch=16,cex=1.5) 

                  
              }#End loop nux
              
              
          #Headers
            yline = max(nchar(as.character(pretty(ylim)))) 
            mtext(side=3,line=1.5,font=2,cex=1.5, main)
     
          #Legend
              legend("top",inset=.01,bty='n',lwd=3,col=cols[1:n.lines],
                     title='Moderator Value',
                     title.font=2,
                     legend=spotlight.labels)
      
          #Histogram  
              if (histogram==TRUE) draw.histogram(fxz.list, focal, moderation='continuous', x1s, ux, nux, cols,ylim,xlim)
              #See draw.histogram.R

        }           
     
    
    
    
    