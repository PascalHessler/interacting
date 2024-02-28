      
    draw.histogram = function(moderation, zs, y0, y1, nux, ylim,xlim, fx,cols, nbins,  z_bins)
          {
          #Box
            y0=par('usr')[3]
            x0=par('usr')[1]
            x1=par('usr')[2]
            yd=y0 + .08*nux*diff(ylim)*.9
             polygon(x=c(x0,x0,x1,x1),
                    border=NA,
                     y=c(y0,yd,yd,y0),col=adjustcolor('gray77',.25))  

      
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
                
                } #End if discrete
    
    
    #------------------------------------------------------------------
             
    #"CONTINUOUS" 
            if (moderation=='continuous')
            {
          #Get breakpoints for z bings
            breaks=get.breaks(z_bins) #function 5 in utils.R
        
          #Adjust y coordinates too be bottom of figure
            y0=par('usr')[3]
            y1=y0+.05*nux*diff(ylim)  #10% for 2 vars, 15% for 3
            
          
          #Cumulative frequencies by bin
            fx2=apply(fx, 2, cumsum)            #cumulative freq sum
            fx2=fx2/max(fx2[3,])                #rescale so largest bin has 100% of the height
            fx2 = rbind(rep(0,ncol(fx2)),fx2)   #add 0 as baseline
            fx2 = fx2*(y1-y0)+y0                #Express as share of the 10% of the ylim allocated to it
            
          #Nested loop for segmented lines
            for (j in 1:nux)
            {
              for (m in 1:nbins)
              {
                xs=c(breaks$from[m] , breaks$from[m] , breaks$to[m], breaks$to[m])
                ys=c(fx2[j,m] ,  fx2[j+1,m],  fx2[j+1,m], fx2[j,m])
                polygon(x=xs,y=ys,col=adjustcolor2(cols[j],.6))
              }} #End nested loop for histogram
            
          #Add sample size values
            text(rowMeans(breaks) ,y1,colSums(fx),pos=3,cex=.8,font=3,col='gray38')
            text(min(zs)-.05*diff(xlim),y1,'n = ',pos=3,cex=.8,font=3,col='gray38')
            
          
        } #End continuous
             
            
    } #End function
  