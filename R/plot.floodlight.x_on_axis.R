


    plot.floodlight.x_on_axis = function(xlab, simple.slopes, histogram, data,xs, ylab1,gr,spotlights,cols,spotlight.labels)
    {
    
        #Default xlabel
          if (xlab=='') xlab='X: Focal Predictor '
          
        #Unlist data.frames
           floodlight.df <- do.call(rbind, floodlight)
 
           #Combines the 3 dataframes, currently in a list, 
             
        #Set ylim
            ylim = range(floodlight.df[,c('conf.low','conf.high')]) #Default y-range
            ylim[2]=ylim[2]+.25*diff(ylim)                                  #Add at the top for the legend
            if (histogram==TRUE) ylim[1]=ylim[1]- length(spotlights)*.08*diff(ylim)        #add at the bottom for the histogram
          
        #Set x-lim
            xlim=range(data$x)
            xlim[1]=xlim[1]-.05*diff(xlim) #add margin to left to put the 'n=' 
            
        #Empty plot
            plot(xs,floodlight[[1]]$estimate,type='n',xlab=xlab,ylab=ylab1,las=1,ylim=ylim,xlim=xlim,yaxt='n',cex.lab=1.3)
            axis(2,at=pretty(ylim)[c(-1,-2)],las=1) #y-axis missing lower two tikcs to give space to the histogram
         
            
          #Loop trhough the spotlights
              n.lines=length(simple.slopes)
              j=1
              for (j in 1:n.lines) {
                  
                  
                  g1=as.numeric(gr[,j])
                  g=rep(g1 , each=length(zs)/length(g1))
                  
  
                #Lines
                  line.seg(zs,floodlight[[j]]$estimate,lwd=4*g, col=cols[j],g=g) 
              
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(zs,rev(zs)),
                        y=c(floodlight[[j]]$conf.high,
                            rev(floodlight[[j]]$conf.low)),
                            col=adjustcolor(cols[j],.1),border = NA)
                  
               #Dots if we have not binned data
                #  if (nuz==nbins) points(zs,simple.slopes[[j]]$estimate, col=adjustcolor2(cols[j],gr[[j]]),pch=16) 
                
              }#End loop nux
              
              
          #Headers
            
            yline = max(nchar(as.character(pretty(ylim)))) 
            mtext(side=3,line=1.5,font=2,cex=1.5,main2)
     
          
          #Legend
              legend("topleft",inset=.01,bty='n',lwd=3,col=cols[1:n.lines],
                     title='Moderator Value',
                     title.font=2,
                     legend=spotlight.labels)
      
          #Histogram  
              if (histogram==TRUE) draw.histogram(fxz.list, focal, moderation, zs, nux, cols,ylim,xlim)
                              #See draw.histogram.R

        }           
     
    
    
    
    