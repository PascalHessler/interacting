

  #  plot.x_on_axis = function(xlab,ylab,main, res , histogram, data,xs, gr,spotlights,cols,spotlight.labels,focal,moderation,nux,max.unique,fxz.list)

    make.plot     = function(type,xlab,ylab,main, res , histogram, data,xs,zs, gr, spotlights , cols , spotlight.labels ,
                             focal , moderation , max.unique , fxz.list,nux,nuz)
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
              nux1=nux
              x1.axis='x'
            }
             
      
      #Z on Axis 
          if (focal=='categorical') {
            if (xlab=='') xlab='Moderator'
            if (type=='simple slopes') n.lines=nux
            if (type=='floodlight')    n.lines=nux-1
                  #for categorical, the dy/dx is the diff in xs so 1 fewer line than x values
            x1.range = range(data$z)
            x1s=zs
            nux1=nuz
            x1.axis='z'
            

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
         
            
    #6 Loop making the 2 or 3 lines
               j=1
  
               col.seg = cols
               if (focal=='categorical' & type=='floodlight') col.seg=col.seg[-1]  
               
                 #If floodlighting a categoricall predictor, drop the first color for that's baseline
               
              for (j in 1:n.lines) {
                 
               #Color of lines
                #Based on frequencies
                  g1=as.numeric(gr[,j])  #this is based on fxz
                
                #If continuous, expand to set of values colored together
                  if (nux1>max.unique)  g = rep(g1 , each=length(x1s)/length(g1))
                  
                #If discrete then just the values once (one line per bin-pair)
                  if (nux1<=max.unique) g = g1
                    
              
                #Lines
                  line.seg(x1s,res[[j]]$estimate,lwd=4*g, col=col.seg[j],g=g) 
              
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(x1s,rev(x1s)),
                        y=c(res[[j]]$conf.high,
                            rev(res[[j]]$conf.low)),
                            col=adjustcolor(col.seg[j],.1),border = NA)
                  
               #Dots within line if we have not binned data for plotting
                  if (nux1 <= max.unique) points(x1s,res[[j]]$estimate, col=adjustcolor2(col.seg[j],g),pch=16,cex=1.5) 

                  
              }#End loop nux
              
              
          #Headers
            yline = max(nchar(as.character(pretty(ylim)))) 
            mtext(side=3,line=1.5,font=2,cex=1.5, main)
     
          #Legend
            if (x1.axis=='x')
              {
              legend("top",inset=.01,bty='n',lwd=3,col=cols[1:n.lines],
                     title='Moderator Value',
                     title.font=2,
                     legend=spotlight.labels)
            }
            
            if (x1.axis=='z')
              {
              if (type=='simple slopes') legend("top",inset=.01,bty='n',lwd=5,col=cols[1:n.lines],title='Focal Predictor Value',title.font=2,legend=xs)
              if (type=='floodlight')    legend("top",inset=.01,bty='n',lwd=5,col=cols[-1],title='Constrasted Focal Predictor',title.font=2,legend=paste0(xs[-1]," vs. ",xs[1]))
            
              
              }
            
            
          #Histogram  
              if (histogram==TRUE) draw.histogram(fxz.list, focal, moderation='continuous', x1s, ux1, nux1, cols,ylim,xlim,max.unique)
              #See draw.histogram.R

        }           
     
    
    
    
    