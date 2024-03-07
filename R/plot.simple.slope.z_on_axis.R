


    plot.simple.slopes.z_on_axis = function(xlab, simple.slopes, histogram, data,xs, ylab1,gr,spotlights,cols,nux,zs,bins,fx,nbins)
    {

    #Default xlabel
          if (xlab=='') xlab='Moderator'
          
    #Unlist data.frames
           simple.slopes.df <- do.call(rbind, simple.slopes)
 
             #Combines the 2 or 3 dataframes, currently in a list, 
             #on  each possible x-value in a single dataframe with 
             #estimate, SE, and conf.int
         
    #Set ylim
      ylim = range(simple.slopes.df[,c('conf.low','conf.high')]) #Default y-range
      ylim[2]=ylim[2]+.1*diff(ylim)                                  #Add at the top for the legend
      if (histogram==TRUE) ylim[1]=ylim[1]-nux*.08*diff(ylim)        #add at the bottom for the histogram
          
        #Set x-lim
            xlim=range(data$z)
            xlim[1]=xlim[1]-.05*diff(xlim) #add margin to left to put the 'n=' 
            
        #Empty plot
            plot(zs,simple.slopes[[1]]$estimate,type='n',xlab=xlab,ylab=ylab1,las=1,ylim=ylim,xlim=xlim,yaxt='n',cex.lab=1.3)
            axis(2,at=pretty(ylim)[c(-1,-2)],las=1) #y-axis missing lower two tikcs to give space to the histogram
            ltys=c(1,1,1)

            
          #Loop the 2 or 3 values of x slopes
              for (j in 1:nux) {
                #Lines
                  line.seg(zs,simple.slopes[[j]]$estimate,lwd=4*gr[[j]], col=cols[j],g=gr[[j]],lty=ltys[j]) 
              
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(zs,rev(zs)),
                        y=c(simple.slopes[[j]]$conf.high,
                            rev(simple.slopes[[j]]$conf.low)),
                            col=adjustcolor(cols[j],.1),border = NA)
                  
               #Dots if we have not binned data
                  if (nuz==nbins) points(zs,simple.slopes[[j]]$estimate, col=adjustcolor2(cols[j],gr[[j]]),pch=16) 
                
              }#End loop nux
              
              
          #Headers
            
            yline = max(nchar(as.character(pretty(ylim)))) 
            mtext(side=3,line=1.5,font=2,cex=1.5,main1)
     
          
          #Legend
              legend("topleft",inset=.01,bty='n',lty=ltys[1:nux],lwd=3,col=cols[1:nux],legend=as.character(ux))
      
          #Histogram  
              if (histogram==TRUE) draw.histogram(moderation, zs, nux, ylim,xlim, fx,cols, nbins,  bins)
                                            
                #See draw.histogram.R
    }
    