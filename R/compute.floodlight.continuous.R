

compute.floodlight.continuous=function(spotlights, data, xs)
      {
          floodlight = list()
          j=1
          zj=spotlights[1]
          for (zj in spotlights)
          {
          #Make prediction data
            ndj = expand.grid(z=zj,x=xs)
            ndj = add.covariates.at.mean(ndj, data)

          #Save marginal effects results
            options(warn=-1)
            floodlight[[j]] = marginaleffects::slopes(model, newdata = ndj, var='x')
            floodlight[[j]]$z=zj

            options(warn=-0)
                #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
           
            j=j+1 
          } #End loop
          
        return(floodlight)  
    } #End function
        
        
}

