


get.regions.jn=function(j,xvar,zvar, focal,probe.bins)
  {
  #Add counter
    j$k=1:nrow(j)
    
  #Keep significant rows
    js=j[j$conf.low*j$conf.high>0,]
    
  #initialize output
    output='Johnson-Neyman Regions of Significance\n'
    
  #Early return if empty
    if (nrow(js)==0) {
      output=paste0(output,"The effect of ",xvar, "is not significant for any value of the moderator considered")
      return(output)
    }
    
    
  #Find start/end of sets of values
    #Find batches of observations within sig/ns ranges
      if (focal!='categorical') {
        breaks <- c(TRUE, diff(js$k) != 1 | diff(js$z) != 0 | diff(sign(js$marginal.effect))!=0)
      } 
    
      if (focal=='categorical')
      {
        same.contrast <- c( js$contrast[-1] == js$contrast[-nrow(js)])
        breaks <- c(TRUE, diff(js$k) != 1 | !same.contrast | diff(sign(js$marginal.effect))!=0)
        
      }
    
      js$batch <- cumsum(breaks)
      j2 <- do.call(rbind,
                    by(js, js$batch, function(sub_j) {
                      sub_j[c(1, nrow(sub_j)), ]
                    }))

#Print output 
      
#1 Non-Categorical X
      if (focal!='categorical')
      {
    #Loop over unique moderator values
      uz=unique(j2$z)
        k=0
        for (zk in uz)
        {
          j3=j2[j2[,zvar]==zk,]
          xs=j3[,xvar]
          xs <- data.frame(matrix(xs, ncol = 2, byrow = TRUE))
          names(xs)=c('from.x','to.x')
          sk=sign(j3$marginal.effect)
          signk <- sk[seq(1, length(sk), by = 2)]
          xs$sign.text=ifelse(signk==1,'positive','negative')
          
        if(nrow(xs)>0)
          {
            for (rowk in 1:nrow(xs))
      
            {
            k=k+1
            xr=xs[rowk,]
            output=paste0(output, 
                         k ,") When '",zvar,"' = ",round(zk,2),", the effect of '",xvar,"' is ",xr$sign.text,
                        " in the range of '",xvar,"': [",round(xr$from.x,2)," to ", round(xr$to.x,2),"]\n")
            } #End loop for rows
      
          } #End if nrow>0
        } #End loop over zs
      } #End if categorical
      
#2 Categorical X
      if (focal=='categorical')
      {
        
        #Show note about range of significance only includig observed values
          show.note = FALSE 
        
        #Loop over unique moderator values
        ucon=unique(j2$contrast)
        k=0
        for (conk in ucon)
        {
          j3=j2[j2[,'contrast']==conk,]
          zs=j3[,zvar]
          zs <- data.frame(matrix(zs, ncol = 2, byrow = TRUE))
          names(zs)=c('from.z','to.z')
          sk=sign(j3$marginal.effect)
          signk <- sk[seq(1, length(sk), by = 2)]
          zs$sign.text=ifelse(signk==1,'positive','negative')
          
          conk.clean <- gsub("mean\\(", "", conk)  # Remove "mean("
          conk.clean <- gsub("\\)", "", conk.clean)      # Remove ")"
          
          if(nrow(zs)>0)
          {
            for (rowk in 1:nrow(zs))
              
            {
              k=k+1
              zr=zs[rowk,]
              output=paste0(output, 
                            k ,") The contrast for '",xvar,'" of ',conk.clean," is ",zr$sign.text,
                            " in the range of '",zvar,"': [",round(zr$from.z,2)," to ", round(zr$to.z,2),"]\n")

            } #End loop for rows
            
          } #End if nrow>0
        } #End loop over zs
      } #End if categorical    
      
      
    #Add note about max and min
    output=paste0(output,"\n\nNotes:\n",
                  "1) Regions of significance reported by interprobe() never include values outside\n",
                  "range of observed data.\n",
                  "2) These regions are precise to 1/",probe.bins," of the range of x-values. For \n",
                  "greater precision (and slower estimation) set 'probe.bins' to a value greater than '",probe.bins,"'.")
      
    return(output)

    }

