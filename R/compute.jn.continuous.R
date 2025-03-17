

    compute.jn.continuous=function(spotlights, data, xs,zs,model,xvar,zvar,moderator.on.x.axis)
      {
  
  
      #When x is in the x-axis
        if (moderator.on.x.axis==FALSE)
        {
          jn = list()
          j=1
          for (zj in spotlights)
          {
          #Make prediction data
            ndj = expand.grid(z=zj,x=xs)
            ndj = add.covariates.at.mean(ndj, data)
            names(ndj)[1:2]=c(zvar,xvar)
            
          #Save marginal effects results
            options(warn=-1)
            jn[[j]] = data.frame(marginaleffects::slopes(model, newdata = ndj, var=xvar))
            jn[[j]][,zvar]=zj
          
            options(warn=-0)
                #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
           
            j=j+1 
          } #End loop
          
        return(jn)  
        }
  
  

      
     #When z is in the x-axis
        if (moderator.on.x.axis==TRUE)
        {
          jn = list()
          j=1
          for (xj in spotlights)
          {
          #Make prediction data
            ndj = expand.grid(x=xj,z=zs)
            ndj = add.covariates.at.mean(ndj, data)
            names(ndj)[1:2]=c(xvar,zvar)
            
          #Save marginal effects results
            options(warn=-1)
            jn[[j]] = data.frame(marginaleffects::slopes(model, newdata = ndj, var=xvar))
            jn[[j]][,xvar]=xj
          
            options(warn=-0)
                #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
           
            j=j+1 
          } #End loop
          
        return(jn)  
        }
  
  
  
  
  
    } #End function
        

