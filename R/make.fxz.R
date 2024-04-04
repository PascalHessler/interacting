

#note: namedList() used below in Utils.R

make.fxz = function(data  , n.bin.continuous,  moderation ,nux,max.unique ,spotlights,xvar,zvar)
{
  
  
#------------------------------------------------------------
#CASE 1  x: contintuous nux>max.unique, always plot three lines for spotlights of z
      
    if (nux>max.unique) {
  

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
            output=namedList(fxz , x1bins)
            return(output)
      }
  
  
#------------------------------------------------------------
#CASE 2  x: contintuous nux<max.unique, plot three lines for spotlights of z
      
    if (nux>3 & nux<=max.unique ) {
  
     
        #Cut z into three
            cuts=c()
            cuts[1] =( spotlights[1]+spotlights[2])/2
            cuts[2] =( spotlights[3]+spotlights[2])/2
            
            #Spotlights has to have three values, and it takes the midpoints of those three values to make the three bins
            
            zbins  = cut(data[,zvar], breaks = c(-Inf, cuts, Inf),  labels = paste0("zbin_",1:3), include.lowest = TRUE)
            
        #Compute the cross frequencies
            fxz   = table(data[,xvar],zbins)
            #output=namedList(fxz , ux)
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
#CASE 3  x: discrete z: discrete
      if (nux<=3 & moderation=='discrete') {
            fx   = table(data[,xvar],data[,zvar])
            return(fx)
      }
  
}