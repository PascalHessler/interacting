


get.regions.jn=function(j,xvar,zvar)
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
      breaks <- c(TRUE, diff(js$k) != 1 | diff(js$z) != 0 | diff(sign(js$marginal.effect))!=0)
      js$batch <- cumsum(breaks)
      j2 <- do.call(rbind,
                    by(js, js$batch, function(sub_j) {
                      sub_j[c(1, nrow(sub_j)), ]
                    }))
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
                  "\n", k ,") When '",zvar,"' = ",round(zk,2),", the effect of '",xvar,"' is ",xr$sign.text,
                  " in the range of '",xvar,"': [",round(xr$from.x,2)," to ", round(xr$to.x,2),"]")
      }

    }
    }
    return(output)

    }

