      
      draw.histogram = function(moderation, zs, y0, y1, nux, ylim, fx  )
              {
              #DISCRETE
                  if (moderation=='discrete')
                    {
                    #Width in plot between zs
                      bin.width=zs[2]-zs[1]
          
                    #Set coordinates for top/bottom of bars
                      y0=par('usr')[3]                   #bottom of graph
                      y1=y0+.05*nux*diff(ylim)           #10 or 15% of vertical distance for this
                      h = y0+(fx/max(fx))*.1*diff(ylim)  #height of bars based on their frequency (fx)
                      
                    #Loop plotting them
                      for (j in 1:nux) segments(x0=zs + (j-1)*.08*bin.width -.04*bin.width,
                                            x1=zs + (j-1)*.08*bin.width -.04*bin.width,
                                            y0=y0,y1=h[j,],col=cols[j],lwd=4)
                    
                    #Add sample size values
                      text(zs,y1,colSums(fx),               cex=.8,font=3,col='gray38')
                      text(min(zs)-.05*diff(xlim),y1,'n = ',cex=.8,font=3,col='gray38')
                    
                    } #End if nbins<20
            }
      
      



      draw.histogram(moderation, zs, y0, y1, nux, ylim, fx  )




  
  draw.histogram__1= function(
                      x=NULL,z=NULL,y=NULL,
                      data=NULL,
                      model=NULL,
                      k=NULL,
                      zs=NULL,
                      histogram=TRUE,
                      max.unique = 11,
                      n.bin.continuous = 10,
                      force.discrete.freqs=FALSE, #Should frequencies be shown for every value of moderator
                      shade.up.to = 50,           #below this sample size we shade to show few observations
                      xlab='moderator',
                      cols=c('red4','blue4','green4'),
                      ylab1='Dependent Variable',
                      ylab2='Marginal Effect',
                      main1="GAM Simple Slopes",
                      main2='GAM Floodlight' , 
                      draw.simple.slopes=TRUE,
                      draw.floodlight=TRUE)
  
  {
    
    
    
  }