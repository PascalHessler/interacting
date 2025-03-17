
#CASE 1  x: continuous nux>max.unique, always plot three lines for spotlights
  # 1.1  x-axis has x
  # 1.2  x-axis has z
#CASE 2  x: continuous nux<max.unique, plot three lines for spotlights of z
#CASE 3  x: categorical z: cont
#CASE 4  x: discrete z: discrete


make.fxz = function(data  , n.bin.continuous,  moderation ,nux,max.unique ,spotlights,xvar,zvar,moderator.on.x.axis)
{
  
  
#------------------------------------------------------------
#CASE 1  x: continuous nux>max.unique, always plot three lines for spotlights
    
    if (nux>max.unique) {
      
    #1.1 focal  the x-axis
        if (moderator.on.x.axis==FALSE)
          {
  

        #Cut x into nbins
            xbins   = cut(data[,xvar] ,n.bin.continuous , include.lowest=TRUE,labels=paste0('xbin_',1:(n.bin.continuous)))
            x1bins  = cut(data[,xvar] ,n.bin.continuous , include.lowest=TRUE) 
            
            
        #Cut z into three
            cuts=c()
            cuts[1] =( spotlights[1]+spotlights[2])/2
            cuts[2] =( spotlights[3]+spotlights[2])/2
            
            #Spotlights has to have three values, and it takes the midpoints of those three values to make the three bins
            
            zbins  = cut(data[,zvar], breaks = c(-Inf, cuts, Inf),  labels = paste0("zbin_",1:3), include.lowest = TRUE)
            
        #Compute the cross frequencies
            fxz   = table(xbins,zbins)
            output=namedList(fxz , x1bins)          #namedList() in Utils.R

            return(output)
            
      } #end 1.1
  
     
      
     #1.2 Moderator in the x-axis
        if (moderator.on.x.axis==TRUE)
          {
  

        #Cut x-axis into n.bins
            xbins   = cut(data[,zvar] ,n.bin.continuous , include.lowest=TRUE,labels=paste0('xbin_',1:(n.bin.continuous)))
            x1bins  = cut(data[,zvar] ,n.bin.continuous , include.lowest=TRUE) 
            
        #Cut spotlight variable into three
            cuts=c()
            cuts[1] =( spotlights[1]+spotlights[2])/2
            cuts[2] =( spotlights[3]+spotlights[2])/2
            
            #Spotlights has to have three values, and it takes the midpoints of those three values to make the three bins
            
            zbins  = cut(data[,xvar], breaks = c(-Inf, cuts, Inf),  labels = paste0("zbin_",1:3), include.lowest = TRUE)
            
        #Compute the cross frequencies
            fxz   = table(xbins,zbins)
            output=namedList(fxz , x1bins)          #namedList() in Utils.R

            return(output)
            
        } #End 1.2 
    }#END case 1
      
  
#------------------------------------------------------------
#CASE 2  x: continuous nux<max.unique, plot three lines for spotlights of z
      
    if (nux>3 & nux<=max.unique ) {
  
     
        #Cut z into three
            cuts=c()
            cuts[1] =( spotlights[1]+spotlights[2])/2
            cuts[2] =( spotlights[3]+spotlights[2])/2
            
            #Spotlights has to have three values, and it takes the midpoints of those three values to make the three bins
            
            zbins  = cut(data[,zvar], breaks = c(-Inf, cuts, Inf),  labels = paste0("zbin_",1:3), include.lowest = TRUE)
            
        #Compute the cross frequencies
            fxz   = table(data[,xvar],zbins)
            output=namedList(fxz)
            return(output)
      }
  
  
#------------------------------------------------------------
#CASE 3  x: categorical z: cont
      if (nux<=3 & moderation=='continuous') {
            zbins   = cut(data[,zvar] ,n.bin.continuous , include.lowest=TRUE,labels=paste0('zbin_',1:(n.bin.continuous)))
            x1bins  = cut(data[,zvar] ,n.bin.continuous , include.lowest=TRUE) 
      
        
            fxz   = table(zbins,data[,xvar])
            return(namedList(fxz,x1bins))  
      }

#------------------------------------------------------------
#CASE 4  x: discrete z: discrete
      if (nux<=3 & moderation=='discrete') {
            fxz   = t(table(data[,xvar],data[,zvar]))
            return(list(fxz=fxz))
      }
  
}