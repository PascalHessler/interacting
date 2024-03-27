




    draw.histogram = function(fxz.list, focal, moderation, zs, nux, cols,ylim,xlim)
          {
        
          #Get frequencies
             fxz=fxz.list$fxz 
                      
          
          #How many bins in the x-axis
            if (focal=='continuous') {
                x.axis.bins = nrow(fxz)
                lines.total = ncol(fxz)
            }
              
          #How many bins in the x-axis
            if (focal=='discrete') {
                x.axis.bins = ncol(fxz)
                lines.total = nrow(fxz)
                }
              
              
                
              
          #Box
            y0=par('usr')[3]
            x0=par('usr')[1]
            x1=par('usr')[2]
            yd=y0 + .08* lines.total *diff(ylim)*.9
             polygon(x=c(x0,x0,x1,x1),
                    border=NA,
                     y=c(y0,yd,yd,y0),col=adjustcolor('gray77',.25))  

      
 #Case 1 - x: continuous
     if (focal=='continuous')
        {
        #Get breakpoints for z bings
           breaks=get.breaks(fxz.list$xbins.values) #function 5 in utils.R
        
          #Adjust y coordinates to be bottom of figure
            y0=par('usr')[3]
            y1=y0+.05*lines.total*diff(ylim)  #10% for 2 vars, 15% for 3
            
          
          #Cumulative frequencies by bin
            fxz2=apply(fxz, 1, cumsum)            #cumulative freq sum
            fxz2=fxz2/max(fxz2[nrow(fxz2),])                #rescale so largest bin has 100% of the height
            fxz2 = rbind(rep(0,ncol(fxz2)),fxz2)   #add 0 as baseline
            fxz2 = fxz2*(y1-y0)+y0                #Express as share of the 10% of the ylim allocated to it
            
          
          #Nested loop for segmented lines
            for (j in 1:x.axis.bins)
            {
              for (m in 1:lines.total)
              {
                xs=c(breaks$from[j] , breaks$from[j] , breaks$to[j], breaks$to[j])
                ys=c(fxz2[m,j] ,  fxz2[m+1,j],  fxz2[m+1,j], fxz2[m,j])
                polygon(x=xs,y=ys,col=adjustcolor2(cols[m],.6))
              }} #End nested loop for histogram
            
          #Add sample size values
            text(rowMeans(breaks) ,y1,rowSums(fxz),pos=3,cex=.8,font=3,col='gray38')
            text(min(zs)-.05*diff(xlim),y1,'n = ',pos=3,cex=.8,font=3,col='gray38')
       }
             
             
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
                  text(zs,y1,colSums(fxz),              cex=.8,font=3,col='gray38')
                  text(min(zs)-.05*diff(xlim),y1,'n = ',cex=.8,font=3,col='gray38')
                
                } #End if discrete
    
    
    
    #------------------------------------------------------------------
             
    #"CONTINUOUS" 
            if (moderation=='continuous')
            {
         
          
        } #End continuous
             
            
    } #End function
  