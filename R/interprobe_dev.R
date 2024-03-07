#' Probe interactions computing Simple Slopes and Floodlight (Johnson-Neyman) 
#' 
#' The interaction is probed, by default, as proposed in Simonsohn (2024), estimating a GAM model
#' and computing both simple slopes and floodlight/Johnson-Neyman procedure.
#'  
#'@param x the predictor of interest (in an experiment, the discrete randomly 
#'assigned manipulation). Can be the name of a variable (e.g., x='treatment') or data (e.g., x=c(1,3,1,4)
#'@param z the moderator. Can be the name of variable, or a vector with data
#'@param y the dependent variable. Can be the name of variable, or a vector with data
#'@param draw TRUE/FALSE for whether to make a plot
#'@param col1 color used when x takes its lowest value
#'@param col2 color used when x takes its highest value
#'
#'@export

#OUTLINE
  #1 Validation
  #2 Get a dataframe
  #3 Create data if given vectors 
  #4 Number of unique x & z values



interprobe_dev <- function(
                    x=NULL,z=NULL,y=NULL,
                    data=NULL,
                    model=NULL,
                    k=NULL,
                    zs=NULL,
                    spotlights=NULL,
                    histogram=TRUE,
                    max.unique = 11,
                    n.bin.continuous = 10,
                    force.discrete.freqs=FALSE, #Should frequencies be shown for every value of moderator
                    shade.up.to = 50,           #below this sample size we shade to show few observations
                    xlab='',
                    cols=c('red4','blue4','green4'),
                    ylab1='Dependent Variable',
                    ylab2='Marginal Effect',
                    main1="GAM Simple Slopes",
                    main2='GAM Floodlight' , 
                    draw.simple.slopes=TRUE,
                    draw.floodlight=TRUE,
                    
                    ...)
                    
  {
  
  #0 If x is specified and it is in a model/data version of interprobe() treat as string
    if (!is.null(model) | !is.null(data)) {
        x <- deparse(substitute(x))
        z <- deparse(substitute(z))
        if (!is.null(y)) y <- deparse(substitute(y))
    }
  
  #1 Validate input and determine what was provided, vector, model, or data.frame
        v = validate.input.combinations(data , model, x, y ,z)
          
          #NOTE: See ./validate.input.combinations.R
          #outputs $input data, input xyz, input
          
      
  
  #------------------------------------------------------------------------------
  
  #2 Get a dataframe
      if (v$input.data==FALSE & v$input.xyz==TRUE)  data = data.frame(x=x,z=z,y=y)
      if (v$input.model==TRUE)                      data = model$model
      
        #v is a list produced in #1 above
  
      
  #2.1 Remove "GAM" from  figure headers for non-GAM models
      if (v$input.model==TRUE) {
        if (!inherits(model, "gam")) {
          
          #Pre-print 'linear' if we know it is linear
            linear.st=''
            if (inherits(model, "lm")) linear.st='Linear '
          
          #Substitute default headers
            if (main1=="GAM Simple Slopes") main1=paste0(linear.st,"Simple Slopes")
            if (main2=="GAM Floodlight")    main2=paste0(linear.st,"Floodlight")
          
          
        }
      }
      
  #------------------------------------------------------------------------------
  
  #3 Create data if given vectors 
     
        if (v$input.data==FALSE & v$input.xyz==TRUE)
        {
          #Put vectors into dataframe
            data=data.frame(x, z, y)
          #Rename variables to refer to variable names
            x='x'
            z='z'
            y='y'
        }
      
         
  #------------------------------------------------------------------------------
          
    #4 Number of unique x & z values
        #4.1 Count
          ux  = sort(unique(data$x))
          uz  = sort(unique(data$z))
          nux = length(ux)     #nux number of unique x values
          nuz = length(uz)     #nuz number of unique z values
          
        #4.2 Check if only 1 value
          if (nux==1) stop("interprobe says: there is only one observed value for the variable 'x'")
          if (nuz==1) stop("interprobe says: there is only one observed value for the variable 'z'")
          
        #4.3 Categorize as 'continuous' or 'discrete' moderators
          moderation = ifelse(nuz>max.unique, 'continuous', 'discrete')
          focal       = ifelse(nux>max.unique, 'continuous', 'discrete')
          
      
  #--------------------------------------------------------------------
  #5 set moderator values for computing marginal effects
          if (moderation=='discrete')   zs = uz
          if (moderation=='continuous') zs = seq(min(data$z),max(data$z),length.out=100)
          
    #set focal predictor values
          if (focal=='discrete')   xs = ux
          if (focal=='continuous') xs = seq(min(data$x),max(data$x),length.out=100)

          
  #--------------------------------------------------------------------------------
          
  #6 Estimate model (if the user did not enter a model)
          if (v$input.model==FALSE) model = estimate.model(nux,data,k)
          
          #1. v is a list produced in #1 above (see validate.input.combinations.R)
          #2. see estimate.model.R
      
  #--------------------------------------------------------------------------------
     
      #7 Compute simple slopes 
        
      if (draw.simple.slopes==TRUE) {
          
        #7.1 x has 2 or 3 possible values
          
          
              if (nux %in% c(2,3))
              {
              simple.slopes = list()
              j=1
              for (xj in ux)
              {
                #Make prediction data
                  ndj = expand.grid(z=zs,x=xj)
                  
                #Save marginal effects results
                  options(warn=-1)
                  simple.slopes[[j]] = marginaleffects::predictions(model, newdata = ndj,by='z')
                  options(warn=-0)
                 
                    #Note: suppress warnings because `marginaleffects` warns about k as a missing variable
                 
                  j=j+1 
                } #End loop
    
              }  #End nux in 2,3
              
              
          
        #7.2 x has more 4+ values
              if (nux>=4)
              {
              #Spotlights of z
                if (is.null(spotlights)) spotlights=quantile(data$z,c(.15,.5,.85),type=3)
              
              simple.slopes = list()
              j=1
              for (zj in spotlights)
              {
                #Make prediction data
                  ndj = expand.grid(z=zj,x=xs)
                  ndj = add.covariates.at.mean(ndj, data)

                #Save marginal effects results
                  options(warn=-1)
                  simple.slopes[[j]] = marginaleffects::predictions(model, newdata = ndj)
                  options(warn=-0)
                      #Note: suppress warnings because `marginaleffects` warns about k as a missing variable when it is specified
                 
                  j=j+1 
                } #End loop
                
              } #End 7.2 if nux>=4
       
    }#End 7 if simple slopes     
            
 #--------------------------------------------------------------------------------
  #8 Compute floodlight / Johnson-Neyman
        
      if (draw.floodlight==TRUE)
      {
          
      #8.1 x has 2 or 3 possible values
          if (nux %in% c(2,3))
          {
            
            floodlight = list()

          #Marginal effect for condition 2 - 1, or both 3-1 and 2-1, so we exclude from loop 1, and add to all
            j=1
            for (xj in ux[-1])
            {
              #Make prediction data
                  ndj = expand.grid(z=zs,x=c(as.character(ux[1]),xj))
                  
                #Save marginal effects results
                  options(warn=-1)
                  floodlight[[j]] = marginaleffects::predictions(model, newdata = ndj,by='z')
                  floodlight[[j]]$x=xj
                  options(warn=-0)
                 
                    #Note: suppress warnings because `marginaleffects` warns about k as a missing variable
                    #SEE https://github.com/vincentarelbundock/marginaleffects/issues/1031
                  j=j+1 
                } #End loop
    
            
      } #End if nux in c(2,3)
        
        
       #8.2 x has more 4+ values
              if (nux>=4)
              {
              #Spotlights of z
                if (is.null(spotlights)) spotlights=quantile(data$z,c(.15,.5,.85),type=3)
              
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
                
                
            } #End if nux>=4
        
        
        
    } #End if draw floodlight
          
          
          
 #--------------------------------------------------------------------------------
  
          
# 8 N of observations per bin, for both histogram and line colors
    
      #8.0 Ploting frequencies of x or z?
            if (nux>=4) on_x_axis='x'
            if (nux>=3) on_x_axis='z'
        
            
            
      #8.1 Setup bins and get their frequencies
          
      #8.1.1) Z on x-axis
            if (on_x_axis=='z')
              {
              #Continuous Z
                  if (moderation=='continuous') {
                    bins = cut(data$z,n.bin.continuous)  #n.bin.continuous defaults to 10 bins for continuous data
                    fx = table(data$x,bins)
                  }
              #Discrete Z
                  if (moderation=='discrete')  {
                      bins = zs
                      nbins = length(unique(bins))
                      fx = table(data$x,data$z) 
                      
                  } #End discrete Z
              } #End z on x-axis
           
            
      #8.1.2) X on x-axis   
           #if (on_x_axis=='x')
              #{
              #Continuous x
               #   if (focal=='continuous') {
                    #bins = cut(data$x,n.bin.continuous)  #n.bin.continuous defaults to 10 bins for continuous data
                    #zbins= cut(data$z,breaks=spotlights)  #n.bin.continuous defaults to 10 bins for continuous data
                    #fx = table(zbin,bins)
                    
                  #}
              #Discrete x
                  #if (focal=='discrete')  {
                   #   bins = xs
                      #fx = table(data$x,data$z) 
                  #} #End discrete Z
              #} #End z on x-axis
            
          
             #px = prop.table(fx,1)           #share of observations
             #nbins = length(unique(bins))
            #} End of 8.1.2
 
            

      #8.2 Set tone of line for each segment
          if (on_x_axis=='z') n.segments = nux
          if (on_x_axis=='x') n.segments = length(spotlights)
            
          
      if (on_x_axis=='z')
      {
          gr=list()
          for (j in 1:n.segments)
          {
          #Color is going to be shaded if it is n=20, or < 1/3 the uniform distribution
                gray.frequency = pmin(fx[j,]/shade.up.to,1)
                gray.percent   = pmin(px[j,]/((1/nbins)*(1/3)),1)  #Share in relation to 1/3 of the uniform expectation
                gr[[j]] = pmin(gray.frequency, gray.percent)
                
          } #End for
          
      #8.4 Adjust if relying on 'continuous'
          if (moderation == 'continuous') {
            for (k in 1:length(gr)) gr[[k]]=rep(gr[[k]],each=length(zs)/n.bin.continuous) #n.bin.continuous defaults to 10 bins for continuous data
          }
          
      }  #RElyingo n  x-axis=z  - pending figuring out for x on the x-axis when everything is continuous
          
          
             
    #TEMP
    if (on_x_axis=='x') {
      gr=list()
      nbins=length(levels(bins))
      for (j in 1:nbins) gr[[j]] = rep(1,nbins)
    }
          
          
#--------------------------------------------------------------------------------
          
#PLOTTING
 
  #Make two panels if both are treu 
      if (draw.simple.slopes+draw.floodlight==2) {
        
      #Two plots side by side
        old_mfrow <- par('mfrow')
        par(mfrow=c(1,2))
        on.exit(par(mfrow=old_mfrow)) # Ensure original par settings are restored on function exit
 
        }
          
#9 SIMPLE
       if (draw.simple.slopes==TRUE)
       {
          
      #9.1 x has 2 or 3 possible values
        if (nux %in% c(2,3))
        {
          
        #Default xlabel
          if (xlab=='') xlab='Moderator'
          
        #Unlist data.frames
           simple.slopes.df <- do.call(rbind, simple.slopes)
 
             #Combines the 2 or 3 dataframes, currently in a list, 
             #on  each possible x-value in a single dataframe with 
             #estimate, SE, and conf.int
         
        #Set ylim
            ylim = range(simple.slopes.df[,c('conf.low','conf.high')]) #Default y-range
            ylim[2]=ylim[2]+.1*diff(ylim)                                  #Add at the top for the legend
            if (histogram==TRUE) ylim[1]=ylim[1]-nux*.08*diff(ylim)        #add at the bottom for the histogram
          
        #Set x-lim
            xlim=range(data$z)
            xlim[1]=xlim[1]-.05*diff(xlim) #add margin to left to put the 'n=' 
            
        #Empty plot
            plot(zs,simple.slopes[[1]]$estimate,type='n',xlab=xlab,ylab=ylab1,las=1,ylim=ylim,xlim=xlim,yaxt='n',cex.lab=1.3)
            axis(2,at=pretty(ylim)[c(-1,-2)],las=1) #y-axis missing lower two tikcs to give space to the histogram
         
              #ltys=c(1,2,4)
            ltys=c(1,1,1)

            
          #Loop the 2 or 3 values of x slopes
              for (j in 1:nux) {
                #Lines
                  line.seg(zs,simple.slopes[[j]]$estimate,lwd=4*gr[[j]], col=cols[j],g=gr[[j]],lty=ltys[j]) 
              
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(zs,rev(zs)),
                        y=c(simple.slopes[[j]]$conf.high,
                            rev(simple.slopes[[j]]$conf.low)),
                            col=adjustcolor(cols[j],.1),border = NA)
                  
               #Dots if we have not binned data
                  if (nuz==nbins) points(zs,simple.slopes[[j]]$estimate, col=adjustcolor2(cols[j],gr[[j]]),pch=16) 
                
              }#End loop nux
              
              
          #Headers
            
            yline = max(nchar(as.character(pretty(ylim)))) 
            mtext(side=3,line=1.5,font=2,cex=1.5,main1)
     
          
          #Legend
              legend("topleft",inset=.01,bty='n',lty=ltys[1:nux],lwd=3,col=cols[1:nux],legend=as.character(ux))
      
          #Histogram  
              if (histogram==TRUE) draw.histogram(moderation, zs, y0, y1, nux, ylim,xlim, fx,cols, nbins,  z_bins)
                                            
                #See draw.histogram.R
     
                    
        }
    
           
    
         
         
         
       #9.2 x has 4+
        if (nux %in% >=4)
        {
          
        #Default xlabel
          if (xlab=='') xlab='X: Focal Predictor '
          
        #Unlist data.frames
           simple.slopes.df <- do.call(rbind, simple.slopes)
 
           #Combines the 3 dataframes, currently in a list, 
             
        #Set ylim
            ylim = range(simple.slopes.df[,c('conf.low','conf.high')]) #Default y-range
            ylim[2]=ylim[2]+.15*diff(ylim)                                  #Add at the top for the legend
            if (histogram==TRUE) ylim[1]=ylim[1]- length(spotlights)*.08*diff(ylim)        #add at the bottom for the histogram
          
        #Set x-lim
            xlim=range(data$x)
            xlim[1]=xlim[1]-.05*diff(xlim) #add margin to left to put the 'n=' 
            
        #Empty plot
            plot(xs,simple.slopes[[1]]$estimate,type='n',xlab=xlab,ylab=ylab1,las=1,ylim=ylim,xlim=xlim,yaxt='n',cex.lab=1.3)
            axis(2,at=pretty(ylim)[c(-1,-2)],las=1) #y-axis missing lower two tikcs to give space to the histogram
         
              #ltys=c(1,2,4)
            ltys=c(1,1,1)

            
          #Loop trhough the spotlights
              n.lines=length(simple.slopes)
              j=1
              for (j in 1:n.lines) {
              
  #TEMP - single width line
   gr=list()
  gr[[j]] =rep(1,100)
  
                #Lines
                  line.seg(zs,simple.slopes[[j]]$estimate,lwd=4*gr[[j]], col=cols[j],g=gr[[j]],lty=ltys[j]) 
              
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(zs,rev(zs)),
                        y=c(simple.slopes[[j]]$conf.high,
                            rev(simple.slopes[[j]]$conf.low)),
                            col=adjustcolor(cols[j],.1),border = NA)
                  
               #Dots if we have not binned data
                #  if (nuz==nbins) points(zs,simple.slopes[[j]]$estimate, col=adjustcolor2(cols[j],gr[[j]]),pch=16) 
                
              }#End loop nux
              
              
          #Headers
            
            yline = max(nchar(as.character(pretty(ylim)))) 
            mtext(side=3,line=1.5,font=2,cex=1.5,main1)
     
          
          #Legend
              
              legend("topleft",inset=.01,bty='n',lty=ltys[1:nux],lwd=3,col=cols[1:n.segments],
                     legend=round(spotlights,2))
      
          #Histogram  
              if (histogram==TRUE) draw.histogram(moderation, zs, y0, y1, nux, ylim,xlim, fx,cols, nbins,  z_bins)
                                            
                #See draw.histogram.R
     
                    
        }
         
         
         
              
         
         
         
      
          
         
  } #End of function
    

#--------------------------------------------------------------------------------
          
#10 PLOT 2 FLOODLIGHT

  #10.1 x has 2 or 3 possible values
        if (nux %in% c(2,3))
        {
        
          
        #Unlist data.frames
           floodlight.df <- do.call(rbind, floodlight)
     
          
        #Set ylim
            ylim = range(floodlight.df[,c('conf.low','conf.high')]) #Default y-range
            ylim[2]=ylim[2]+.1*diff(ylim)                                  #Add at the top for the legend
            if (histogram==TRUE) ylim[1]=ylim[1]-nux*.08*diff(ylim)        #add at the bottom for the histogram
          
        #Set x-lim
            xlim=range(data$z)
            xlim[1]=xlim[1]-.05*diff(xlim) #add margin to left to put the 'n=' 
            
        #Empty plot
            plot(zs,floodlight[[1]]$estimate,type='n',xlab=xlab,ylab=ylab2,las=1,ylim=ylim,xlim=xlim,yaxt='n',cex.lab=1.3)
            axis(2,at=pretty(ylim)[c(-1,-2)],las=1) #y-axis missing lower two tikcs to give space to the histogram
         
            ltys=c(1,1,1)

            
            #Loop the 2 or 3 values of x slopes
              for (j in 1:(nux-1)) {
                #Lines
                  line.seg(zs,floodlight[[j]]$estimate,lwd=4*gr[[j]], col=cols[j+1],g=gr[[j]],lty=ltys[j]) 
              
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(zs,rev(zs)),
                        y=c(floodlight[[j]]$conf.high,
                            rev(floodlight[[j]]$conf.low)),
                            col=adjustcolor(cols[j+1],.1),border = NA)
                  
               #Dots if we have not binned data
                  if (nuz==nbins) points(zs,floodlight[[j]]$estimate, col=adjustcolor2(cols[j+1],gr[[j]]),pch=16) 
                
              }#End loop nux
              
          #Headers
            mtext(side=3,line=1.5,font=2,cex=1.5,main2)
          
          #Legend
            if (nux==2) legend("topleft",inset=.01,bty='n', lwd=3,col=cols[2],  legend = dms(floodlight[[1]][1,2]))
            if (nux==3) legend("topleft",inset=.01,bty='n', lwd=3,col=cols[2:3],legend = paste0(ux[2:3]," - ",ux[1]))
            
        }


    #Put histogram at the bottom if requested 
    #
        if (histogram==TRUE) draw.histogram(moderation, zs, y0, y1, nux, ylim,xlim, fx,cols, nbins,  z_bins)
                                            
                #See draw.histogram.R
                #Single function for continuous or discrete
          
         
#FINAL OUTPUT
        output=list(simple.slopes=simple.slopes.df, 
                    floodlight=floodlight.df, 
                    model=model, 
                    frequencies=fx,
                    color.adjustments=gr)
        
        return(invisible(output))          
          
  } #End of function

