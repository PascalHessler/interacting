#'Probe Interactions With GAM Simple Slopes and GAM Johnson-Neyman Curves 
#' 
#' Probe interaction as proposed in Simonsohn (2024), estimating a GAM model
#' and computing simple slopes ('spotlight') and Johnson-Neyman ('jn') 
#' curves off the GAM model. While designed for GAM it can be used to probe
#' other models including lm()
#'  
#'@param x the focal predictor of interest (in an experiment, the discrete randomly 
#'assigned manipulation). Can be the name of a variable (e.g., x='treatment') when 
#'providing a data or a model argument, or a vector with values when the data are 
#'not passed as a dataframe. 
#'@param z the moderator. Same options as for `x`
#'@param y the dependent variable. Same options as for `x`.
#'Does not need to be specified when specifying a pre-existing `model` object.
#'@param data an optional data frame containing the focal predictor, moderator and 
#'dependent variable. If `data` is specified, `x`, `z` and `y` must be names of 
#'variables in that dataframe. `model` and `data` may not be both specified in 
#'a given `interprobe()` call (because model objects (e.g. `lm1` in 
#'`lm1=lm(y~x*z,data=df)`) include the data on which they are based.
#'@param model an optional model output object, containing an interaction to be probed.
#'Any model accepted by the package 'marginaleffects' including lm, glm, and gam
#'is accepted. If unspecified, a GAM model is estimated on  provided data. If specified
#'x (focal predictor) and z (the moderator) must also be specified, with variable names.
#'@param k level of smoothness/flexibility used by mgcv::gam() to fit functions, 
#'the default used by interprobe() is k=3, increasing it will lead to more wiggly
#' functions increasing risk of over-fitting. mgcv::gam() uses a much higher default
#' which interprobe opts-out of by specifying it to be k=3. e.g., gam(y~s(x,k=3)).
#'@param spotlights vector with three values of the moderator, or focal predictor, 
#'at which simple slopes and Johnson-Neyman curve are computed. 
#'Defaults to 15th, 50th and 85th percentile of moderator values in the data.
#'@param spotlight.labels labels to use in the legend to indicate the spotlight values
#'@param histogram logical on whether sample sizes are depicted under the figure (default: TRUE)
#'@param max.unique the sample size can be depicted for each possible value in the x 
#'axis or aggregating frequencies within a ranges of values (e.g., showing the 
#'aggregate frequency of values between 100 and 200 or showing the frequency of 
#'100, 101, 102... separately). When the number of unique values in the data 
#'is bigger than 'max.unique' the frequencies are aggregated.
#'@param n.bin.continuous integer with number of bins in histogram (alters figure
#'only if frequencies are aggregated, see `max.unique`).
#'@param n.max in the figure, lines get wider and darker for bigger sample sizes up to a 
#'point. n.max is the point at which lines do not get any darker/wider as sample 
#'size get larger (e.g., with the default of `nmax`=50, whether there are 50 or 
#'100 observations with x=3, the line depicting the estimated functional form will
#'be equally dark and wide around x=3).
#'@param xlab label for the x axis, defaults to be "Focal Predictor" when x is on the x-axis
#'and "Moderator" when z is in the x-axis. Users should replace default with a
#'clear descriptor of the variables (e.g., "z: age of participants")
#'@param cols colors used in the plot (defaults to blue, red, green)
#'@param ylab1 label for the y axis of the simple slopes plot (defaults to 
#''Dependent Variable'). 
#'@param ylab2 label for the y axis of the Johnson-Neyman plot (defaults to
#''Marginal Effect'). 
#'@param main1 Header for simple slopes figure (defaults to 'GAM Simple Slopes')
#'@param main2 Header for Johnson Neyman figure (defaults to 'GAM Johnson-Neyman')
#'@param legend.round vector with minimum and maximum number of decimals to round 
#'in the legend, e.g., c(0,0) forces 0 decimals
#'@param cols vector with three colors used in the plot (defaults to blue, red, green)
#'@param legend.round numeric vector of size two with the minimun and maximum 
#'number of decimals to show on the legend.
#'@param draw which plots to draw?  
#'   \itemize{
#'     \item \code{"both"}:  Draws both Simple Slopes and Johnson-Neyman (default)
#'     \item \code{"simple slopes"}: Only Simple Slopes
#'     \item \code{"jn"}: Only Johnson-Neyman
#'   }
#'@param save.as name of file to save figure to. Extension of the file name determines
#'whether figure is a .svg or a .png file (e.g., file='c:/temp/figure1.svg'). 
#'Default is NULL, in which case the figure is not saved, only shown on screen.
#'@param xlim numeric vector of length 2, giving the x coordinates range
#'@param ylim1 numeric vector of length 2, giving the y coordinates range for Simple Slopes plot
#'@param ylim2 numeric vector of length 2, giving the y coordinates range for Johnson-Neyman (jn) plot
#'@param legend.simple.slopes text to place on legend title (e.g., "Moderator 
#'Values") in Simple Slopes plot
#'@param legend.johnson.neyman text to place on legend title (e.g., 
#'"Effect of focal predictor") in Johnson Neyman plot 
#'@param x.ticks values to use for ticks in x-axis, either a vector or numeric values
#'or a dataframe with two columns. The first column contains x-axis variable values, the second
#'the labels to show in the x-axis for those values, e.g., x.ticks=data.frame(values=c(1,4,7),
#'labels=c("Against","Neutral","Favor")).  
#'@param y.ticks same as x.ticks, for the y-axis
#'@param quiet if TRUE interprobe() does not print output on the console as it runs.
#'@param probe.bins interger specifying how many different values to probe the interactions for 
#'(the bigger this nummber, the more precise the regions of significance are and smoother the plots).
#'@export



interprobe <- function(
                    x=NULL,z=NULL,y=NULL,
                    model=NULL,
                    data=NULL,
                    jn.x.axis='focal',
                    k=NULL,
                    spotlights=NULL,
                    spotlight.labels=NULL,
                    histogram=TRUE,
                    max.unique = 11,
                    n.bin.continuous = 10,
                    n.max = 50,           #below this sample size we shade to show few observations
                    xlab='',
                    cols=c('red4','dodgerblue','green4'),
                    ylab1='',
                    ylab2='',
                    main1="GAM Simple Slopes",
                    main2="GAM Johnson-Neyman",
                    legend.round=c(1,4),
                    draw="both",
                    save.as=NULL,
                    xlim=NULL,
                    ylim1=NULL,
                    ylim2=NULL,
                    legend.simple.slopes  = NULL,
                    legend.johnson.neyman = NULL,
                    x.ticks=NULL,
                    y1.ticks=NULL,
                    y2.ticks=NULL,
                    quiet=FALSE,
                    probe.bins=100)
                    
  {
 
#1 Preliminaries
  #1.0 Get var names
        xvar <- clean_string(deparse(substitute(x)))
        zvar <- clean_string(deparse(substitute(z)))
        yvar <- clean_string(deparse(substitute(y)))

        
        
  #1.1 Validate input and determine what was provided, vector, model, or data.frame
        validate.arguments(x, z ,y ,  model,data, k,spotlights,spotlight.labels,histogram, max.unique,n.bin.continuous, n.max ,
                              xlab,ylab1,ylab2,main1,main2,cols,draw,legend.round,xlim,save.as,xvar,zvar,yvar,
                              x.ticks, y1.ticks, y2.ticks)

  #1.2 If 'data' exist, create local x,y,z
        if (!is.null(data)) {
          x=xvar
          z=zvar
          y=yvar
        }
  
  #1.3 Validate combination to determine if we were given a model or a dataset or vectors
    v = validate.input.combinations(data , model, x, y ,z)

          
  #1.4 Show message of what we will be done
        if (quiet==FALSE)
        {
        cat(paste0("Probing the interaction of '",xvar, "'⋅'" , zvar,"'\n"))
        }
  
  
  #1.5 Create data if it does not exist
    
      #Extract if provided
        if (v$input.data==FALSE & v$input.xyz==TRUE)  {
          data.text = paste0("data = data.frame(",xvar,",", zvar,",", yvar,")")
          data=eval2(data.text)
          
        } 
      
      #Get from Model if that was provided    
        if (v$input.model==TRUE)                      data = model$model

    
  #2 Determine discrete, categorical, continuous
    
      #2.1 Unique values
        ux  = sort(unique(data[,xvar]))
        uz  = sort(unique(data[,zvar]))
        nux = length(ux)     #nux number of unique x values
        nuz = length(uz)     #nuz number of unique z values
        
        #If only 1, stop
            if (nux==1) exit("interprobe says: there is only one observed value for the focal (x) variable, '"    ,xvar,"'")
            if (nuz==1) exit("interprobe says: there is only one observed value for the moderator (z) variable, '",zvar,"'")
            
      #2.2  Categorize as 'continuous', 'discrete', 'categorical' focal predictor
            if (nux>max.unique)          focal = "continuous"
            if (nux<=max.unique & nux>3) focal = "discrete"
            if (nux<=3)                  focal = "categorical"
           
      #2.3  Moderator type
            if (nuz>  max.unique)        moderation='continuous'
            if (nuz<= max.unique)        moderation='discrete'
        
        

  #3 Set xs and zs for plotting
        
        #Moderator
          if (moderation=='continuous') zs = seq(min(data[,zvar]),max(data[,zvar]),length.out=probe.bins)
          if (moderation=='discrete')   zs = uz

        #Focal
          if (focal=='continuous')   xs = seq(min(data[,xvar]),max(data[,xvar]),length.out=probe.bins)
          if (focal!='continuous')   xs = ux


  #4 Estimate model if not provided           
      
      if (v$input.model==FALSE) {
         model = estimate.model(nux,data,k,xvar,zvar,yvar)  #see estimate.model.R
          }
  
        
        
        
  #5 Set spotlight values and labels
      
      #If not set by user  
       if (is.null(spotlights)) {
           
          #Spotlights for x or z
            spotvar <- if (jn.x.axis == 'focal') data[, zvar] else data[, xvar]
           
          #Get the spotlights
               spotlights=quantile(spotvar , c(.15,.5,.85),type=3)
            
          #Get the labels  
               if (is.null(spotlight.labels)) {
               spotlight.labels=paste0(
                                c("15th percentile (","50th percentile (", "85th percentile (") ,
                                c(round2(as.numeric(spotlights), max.d=legend.round[2] , min.d=legend.round[1] )),
                                c(")",")",")"))
               } #End if no labels
         }  #End if no spotlights
           
         #Note: round2() is a function in utils.r that does rounding with default formatting
         
      #If user set spotlights but not spotlight.labels, assign them
          if (is.null(spotlight.labels)) spotlight.labels=as.numeric(spotlights)
          
  #6 Compute simple slopes        
      if (nux <=3)  simple.slopes = compute.slopes.discrete  (ux, zs, model,xvar,zvar)
      if (nux  >3)  simple.slopes = compute.slopes.continuous(spotlights, data, xs,zs, model,xvar,zvar,jn.x.axis)

        
        
  #7 Compute johnson-neyman
      if (nux <=3)  jn = compute.jn.discrete  (ux, zs, model,xvar,zvar)
      if (nux  >3)  jn = compute.jn.continuous(spotlights, data, xs,zs,model,xvar,zvar,jn.x.axis)
      

          
  #8 Get fxz and gr
        #fx:  Frequencies of each bin to determine line width and histogram
        #gr:  How transparent to make the line that is being plotted, it's the n observation in bin over n=100
        
          #Frequencies
            fxz.list = make.fxz(data  , n.bin.continuous,  moderation ,nux,max.unique ,spotlights,xvar,zvar,jn.x.axis)
            fxz=fxz.list$fxz
            
          #As % of the adequate sample size in n.max
            gr = fxz
            for (j in 1:ncol(fxz)) gr[,j] = pmin(fxz[,j]/n.max,1)

            
  #9 Prepare output to be returned to enable plotting independently by user 
      #clean <- function(str) gsub("[^A-Za-z]", "", str)
      df1 <- data.frame(do.call(rbind, simple.slopes))
      df2 <- data.frame(do.call(rbind, jn))
    
    #Drop variables produces by {marginaleffects} which are not needed
      df1 <- df1[, !names(df1) %in% c("rowid", "y","s.value","p.value","statistic",yvar)]
      df2 <- df2[, !names(df2) %in% c("rowid", "y","s.value","statistic","term",
                              "predicted_lo",'predicted_hi','predicted',yvar)]

    #Rename      
      names(df1)[names(df1) == "estimate"] <- "y.hat"
      names(df2)[names(df2) == "estimate"] <- "marginal.effect"

      
    #Frequencies
     if (ncol(fxz)==2)  frequencies=data.frame(bin=rownames(fxz), f1=fxz[,1],f2=fxz[,2],row.names = NULL)
     if (ncol(fxz)==3)  frequencies=data.frame(bin=rownames(fxz), f1=fxz[,1],f2=fxz[,2],f3=fxz[,3],row.names = NULL)
      
    #Prepare output list
      output=list(simple.slopes = df1, johnson.neyman = df2, frequencies=frequencies)
    
      
  #10 Prepare labels for figures
      
      #10.1. Remove "GAM" from default figure headers for non-GAM models
            if (v$input.model==TRUE) {
              if (!inherits(model, "gam")) {
                
                #Pre-print 'linear' if we know it is linear
                  linear.st=''
                  if (inherits(model, "lm")) linear.st='Linear '
                
                #Substitute default headers
                  if (main1=="GAM Simple Slopes")   main1=paste0(linear.st,"Simple Slopes")
                  if (main2=="GAM Johnson-Neyman")  main2=paste0(linear.st,"Johnson-Neyman")
                  }
                  }      
        

          
      #10.3 y-axix
          if (ylab1=='') ylab1=yvar
          if (ylab2=='') ylab2=paste0("Marginal effect of ",xvar)      
      

  #11 Save figure
      if (!is.null(save.as)) {
              
          #Get extension of file name
              extension= tools::file_ext(save.as)
                  
          #Type of figure file
              
              if (draw=='both')
              {
                if (extension=='svg') svg(save.as,width=14,height=7)
                if (extension=='png') png(save.as,width=14000,height=7000,res=1000)
                par(mfrow=c(1,2))
                par(oma=c(0,1,0,0))
                

              } else {
                
                if (extension=='svg') svg(save.as,width=7,height=7)
                if (extension=='png') png(save.as,width=7000,height=7000,res=1000)
                par(oma=c(0,1,0,0))
                
              }     
  #12 Plot Simple Slopes       
      if (draw %in% c("both","simple slopes"))
      {
        make.plot (type='simple slopes', xlab, ylab1, main1, simple.slopes , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                   focal,moderation,max.unique,fxz.list,nux,nuz,xvar,zvar,xlim,ylim1,
                   legend.title=legend.simple.slopes , x.ticks , y1.ticks , jn.x.axis)
        
    
              }
          #Plot Johnson-Neyman (jn)
             if (draw %in% c("both","jn"))
               {
                make.plot (type='jn', xlab, ylab2, main2, jn , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                     focal , moderation , max.unique , fxz.list , nux , nuz , xvar , zvar , xlim , ylim2,
                     legend.title=legend.johnson.neyman , x.ticks , y2.ticks , jn.x.axis)  
               }
              
          #End
            message("The figures have been saved to '",file,"'")
            dev.off()        
      }          
                  
  
      
#12 Plot on screen
      old_par = par(no.readonly = TRUE)
      par(oma=c(0,1,0,0))
      
        if (draw=='both')
        {
    #12.1 #Two plots side by side
            old_par = par(no.readonly = TRUE)
            par(mfrow=c(1,2))
            on.exit(par(old_par)) 
        } 
      
      
    #12.2 Plot simple slopes     
        
        if (draw %in% c('simple slopes','both'))
          {
          output.simple.slopes = make.plot (type='simple.slopes',xlab,ylab1,main1, simple.slopes , histogram, data,xs,zs, gr, spotlights , cols , spotlight.labels ,
                                 focal , moderation , max.unique , fxz.list,nux,nuz,xvar,zvar,xlim,ylim1,legend.title=legend.simple.slopes,
                                 x.ticks, y1.ticks ,jn.x.axis)  
          }
      
      
    #12.3 Plot jn/Johson-Neyman     
      if (draw %in% c('jn','both'))
      {

       output.johnson.neyman = make.plot (type='jn', xlab, ylab2, main2, jn , histogram, 
                                          data,xs, zs, gr,spotlights,cols,spotlight.labels,
                                          focal,moderation,max.unique,fxz.list,nux,nuz,xvar,zvar,
                                          xlim,ylim2,legend.title=legend.johnson.neyman,x.ticks,y2.ticks,jn.x.axis)  
      }
#12 Add histogram bins (NULL for discrete x1axis)
      if (draw %in% c('both','simple slopes')) breaks=output.simple.slopes
      if (draw %in% c('jn'))                   breaks=output.johnson.neyman
     # output$frequencies = cbind(output$frequencies,breaks)

      
#13 REport JN points unless only simple slopes requested or quiet==TRUE

      regions.jn = 'N/A'
       if (draw!='simple slopes' & quiet==FALSE) {
        regions.jn = get.regions.jn(jn , xvar , zvar ,focal,probe.bins)
         cat(regions.jn)  
       }
     
      
#13 return output for plotting on your own
      if (!is.null(breaks))
      {
      output$frequencies$bin_from=breaks$from
      output$frequencies$bin_to  =breaks$to
      }
      
      
#14 Resort the list
      #output=output[c(4,1,2,3)]
      invisible(output)          
}      
