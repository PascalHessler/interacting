

compute.floodlight.continuous=function(spotlights, data, xs,model,xvar,zvar)
      {
          floodlight = list()
          j=1
          for (zj in spotlights)
          {
          #Make prediction data
            ndj = expand.grid(z=zj,x=xs)
            ndj = add.covariates.at.mean(ndj, data)
            names(ndj)[1:2]=c(zvar,xvar)
            
          #Save marginal effects results
            options(warn=-1)
            floodlight[[j]] = data.frame(marginaleffects::slopes(model, newdata = ndj, var=xvar))
            floodlight[[j]][,zvar]=zj
          
            options(warn=-0)
                #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
           
            j=j+1 
          } #End loop
          
        return(floodlight)  
    } #End function
        

