

make.fxz = function(data  , n.bin.continuous,  moderation  )
{
  
  
#------------------------------------------------------------
#CASE 1  x: contintuous, always plot three lines for spotlights of z
      
    if (nux>3) {
  
        #How many bins for x values?
            nxbins = n.bin.continuous
        
        #Cut x into nbins
            xbins  = cut(data$x ,nxbins,include.lowest=TRUE,labels=paste0('xbin_',1:(nxbins))) 
            
        #Cut z into three
            cuts=c()
            cuts[1] =( spotlights[1]+spotlights[2])/2
            cuts[2] =( spotlights[3]+spotlights[2])/2
            
            #Spotlights has to have three values, and it takes the midpoints of those three values to make the three bins
            
            zbins  = cut(data$z, breaks = c(-Inf, cuts, Inf),  labels = paste0("zbin_",1:3), include.lowest = TRUE)
            
        #Compute the cross frequencies
            fx   = table(zbins , xbins)
            return(fx)
      }
  
#------------------------------------------------------------
#CASE 2  x: discrete z: cont
      if (nux<3 & moderation=='continuous') {
            zbins  = cut(data$z, breaks = c(-Inf, quantile(data$z,c(1/3,2/3)), Inf),  labels = paste0("zbin_",1:3), include.lowest = TRUE)
            fx   = table(zbins , x)
            return(fx)
      }
  
  
#------------------------------------------------------------
#CASE 3  x: discrete z: discrete
      if (nux<3 & moderation=='discrete') {
            fx   = table(z , x)
            return(fx)
      }
  
  