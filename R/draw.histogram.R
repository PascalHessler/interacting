

    draw.histogram = function(fxz.list, focal, moderation, x1s, ux1, nux1, cols,ylim,xlim,max.unique)
          {
        
      #1 preliminaries
        #Get frequencies
             fxz=fxz.list$fxz 
          
        #Determine # of lines and bins
              x.axis.bins = nrow(fxz)
              lines.total = ncol(fxz)
                
              
        #2 Make gray box
            y0=par('usr')[3]
            x0=par('usr')[1]
            x1=par('usr')[2]
            yd=y0 + (.09+lines.total*.07)*diff(ylim)*.85
           
            polygon(x=c(x0,x0,x1,x1),
                    border=NA,
                     y=c(y0,yd,yd,y0),col=adjustcolor('gray77',.25))  
            #"Sample Size" 
                xc = (x1+x0)/2
                yd2 = (yd-y0)*1.05 + y0
                text(xc,yd2,"Sample Size",font=3,cex=1.1,pos=1)

                
        #3 PLOT
            
          #3.1 STACKED HISTOGRAM FOR CONTINUOUS X1 AXIS VARIABLE
              if (nux1>max.unique)
              {
              #Get breakpoints for z bings
                  breaks=get.breaks(fxz.list$x1bins) #function 5 in utils.R
            
              #Adjust y coordinates to be bottom of figure
                y0=par('usr')[3]
                y1=y0+.05*lines.total*diff(ylim)  #10% for 2 vars, 15% for 3
                
              
              #Cumulative frequencies by bin
                fxz2=apply(fxz, 1, cumsum)            #cumulative freq sum
                fxz2=fxz2/max(fxz2[nrow(fxz2),])                #rescale so largest bin has 100% of the height
                fxz2 = rbind(rep(0,ncol(fxz2)),fxz2)   #add 0 as baseline
                fxz2 = fxz2*(y1-y0)+y0                #Express as share of the 10% of the ylim allocated to it
                
          
              #Nested loop for stcked histogram via polygon()
                for (j in 1:x.axis.bins)
                {
                  for (m in 1:lines.total)
                  {
                    x1s=c(breaks$from[j] , breaks$from[j] , breaks$to[j], breaks$to[j])
                    ys=c(fxz2[m,j] ,  fxz2[m+1,j],  fxz2[m+1,j], fxz2[m,j])
                    polygon(x=x1s,y=ys,col=adjustcolor2(cols[m],.6))
                  }} #End nested loop for histogram
            
          #Add sample size values
            rs=rowSums(fxz)
            rs=ifelse(rs>1000000,paste0(round(rs/1000000,1),"M"),rs)
            rs=ifelse(rs>1000,paste0(round(rs/1000,1),"k"),rs)
            
            text(rowMeans(breaks) ,y1,rs,pos=3,cex=.8,font=3,col='gray38')
       }
             
             
#------------------------------------------
#Case 2 - Discrete x-axis
    if (nux1<=max.unique)
      {
        #Adjust y coordinates to be bottom of figure
            y0=par('usr')[3]
            y1=y0+.05*lines.total*diff(ylim)  #10% for 2 vars, 15% for 3
            
          
          #Cumulative frequencies by bin
            fxz2=(fxz/max(fxz))*(y1-y0)+y0           #cumulative freq sum

            
          x.width=(x1s[2]-x1s[1])
          for (j in 1:lines.total)
          {
            xj=x1s + x.width*(j-2)*.1
            segments(x0=xj , x1=xj, y0=y0, y1=fxz2[,j],lwd=4,col=cols[j])
          }
            
          #Add sample size values
            text(x1s ,y1,rowSums(fxz),pos=3,cex=.8,font=3,col='gray38')
       }
                     
             
  
    
#------------------------------------------------------------------
    
             
            
    } #End function
  