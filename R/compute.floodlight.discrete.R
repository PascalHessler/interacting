  
    compute.floodlight.discrete=function(ux, zs, model,xvar,zvar)
        
      {
      floodlight = list()

      #Marginal effect for condition 2 - 1, or both 3-1 and 2-1, so we exclude from loop 1, and add to all
        j=1
        for (xj in ux[-1])
        {
         #Make prediction data
          ndj = expand.grid(z=zs,x=c(as.character(ux[1]),xj))
          names(ndj)[1:2]=c(zvar,xvar)
        #Save marginal effects results
          options(warn=-1)
          floodlight[[j]] = data.frame(marginaleffects::slopes(model, newdata = ndj,by=zvar))
          floodlight[[j]][,xvar]=xj
          options(warn=-0)
         floodlight[[j]]=subset(floodlight[[j]] ,term==xvar)
            #Note: suppress warnings because `marginaleffects` warns about k as a missing variable
            #SEE https://github.com/vincentarelbundock/marginaleffects/issues/1031
          j=j+1 
        } #End loop
        
        return(floodlight)
    }