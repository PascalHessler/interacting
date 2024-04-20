#'Probe Interactions With GAM Simple Slopes and GAM Johnson-Neyman Curves 
#' 
#' Probe interaction as proposed in Simonsohn (2024), estimating a GAM model
#' and computing simple slopes ('spotlight') and Johnson-Neyman ('floodlight') 
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
#'@param spotlights vector with three values of the moderator at which simple slopes 
#'and Johnson-Neyman curve are computed. Defaults to 15th, 50th and 85th percentile
#'of moderator values in the data.
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
#'     \item \code{"both"}: Draws both Simple Slopes and Johnson-Neyman
#'     \item \code{"simple slopes"}: Only Simple Slopes
#'     \item \code{"jn"}: Only Johnson-Neyman
#'   }
#'@param file name of file to save figure to. Extension of the file name determines
#'whether figure is a .svg or a .png file (e.g., file='c:/temp/figure1.svg'). 
#'Default is NULL, in which case the figure is not saved, only shown on screen.
#'@param xlim numeric vector of length 2, giving the x coordinates range
#'@param ylim numeric vector of length 2, giving the y coordinates range
#'@param legend.simple.slopes text to place on legend title (e.g., "Moderator 
#'Values") in Simple Slopes plot
#'@param legend.johnson.neyman text to place on legend title (e.g., 
#'"Effect of focal predictor") in Johnson Neyman plot 
#'
#'@export



interprobe <- function(
                    x=NULL,z=NULL,y=NULL,
                    model=NULL,
                    data=NULL,
                    k=3,
                    spotlights=NULL,
                    spotlight.labels=NULL,
                    histogram=TRUE,
                    max.unique = 11,
                    n.bin.continuous = 10,
                    n.max = 50,           #below this sample size we shade to show few observations
                    xlab='',
                    cols=c('red4','dodgerblue','green4'),
                    ylab1='Dependent Variable',
                    ylab2='Marginal Effect',
                    main1="GAM Simple Slopes",
                    main2="GAM Johnson-Neyman",
                    legend.round=c(1,4),
                    draw="both",
                    file=NULL,
                    xlim=NULL,
                    ylim=NULL,
                    legend.simple.slopes  = NULL,
                    legend.johnson.neyman = NULL)
                    
  {
 
 
  
  #0 Get var names
        xvar <- clean_string(deparse(substitute(x)))
        zvar <- clean_string(deparse(substitute(z)))
        yvar <- clean_string(deparse(substitute(y)))

   #First length and type of arguments
    validate.arguments(x, z ,y ,  model,data, k,spotlights,spotlight.labels,histogram, max.unique,n.bin.continuous, n.max ,
                              xlab,ylab1,ylab2,main1,main2,cols,draw,legend.round,xlim,file,xvar,zvar,yvar)   

      
    if (!is.null(data)) {
      x=xvar
      z=zvar
      y=yvar
    }
    
  #What will be done
        message(paste0("Probing the interaction with:\n",
                "   - Focal predictor : ",xvar,"\n",
                "   - Moderator : " ,zvar,"\n"))
  
  #1 Validate input and determine what was provided, vector, model, or data.frame
  
 



  #Then combination to determine if we were given a model or a dataset or vectors
    v = validate.input.combinations(data , model, x, y ,z)



          
  #2 Create data
    #Extract if provided
      if (v$input.data==FALSE & v$input.xyz==TRUE)  {
        data.text = paste0("data = data.frame(",xvar,",", zvar,",", yvar,")")
        data=eval2(data.text)
        
        }
        
      if (v$input.model==TRUE)                      data = model$model
             
    
  #3 Number of unique x & z values
        #3.1 Count
          ux  = sort(unique(data[,xvar]))
          uz  = sort(unique(data[,zvar]))
          nux = length(ux)     #nux number of unique x values
          nuz = length(uz)     #nuz number of unique z values
          
    
        #3.2 Check if only 1 value
          if (nux==1) exit("interprobe says: there is only one observed value for the focal (x) variable, '"    ,xvar,"'")
          if (nuz==1) exit("interprobe says: there is only one observed value for the moderator (z) variable, '",zvar,"'")
          
        #3.3 Categorize as 'continuous', 'discrete', 'categorical' focal predictor
          if (nux>max.unique)          focal = "continuous"
          if (nux<=max.unique & nux>3) focal = "discrete"
          if (nux<=3)                  focal = "categorical"
         
        #3.4 Moderator type
          moderation = ifelse(nuz > max.unique, 'continuous', 'discrete')

      
        #3.5
          if (is.null(xlab)) {
            xlab=ifelse(focal=='categorial','Moderator','Focal Predictor')
            
          }
          
            
  

  #4 set moderator values for computing marginal effects
          if (moderation=='discrete')   zs = uz
          if (moderation=='continuous') zs = seq(min(data[,zvar]),max(data[,zvar]),length.out=100)
          
        #set focal predictor values
          if (focal!='continuous')   xs = ux
          if (focal=='continuous')   xs = seq(min(data[,xvar]),max(data[,xvar]),length.out=100)

        
  #5 Estimate model (if the user did not provide it as an argument)
          if (v$input.model==FALSE) model = estimate.model(nux,data,k,xvar,zvar,yvar)

  
  #6 Set spotlight values and labels
        
       if (is.null(spotlights)) {
         spotlights=quantile(data[,zvar],c(.15,.5,.85),type=3)
         if (is.null(spotlight.labels)) {
             spotlight.labels=paste0(
                              c("15th percentile (","50th percentile (", "85th percentile (") ,
                              c(round2(as.numeric(spotlights), max.d=legend.round[2] , min.d=legend.round[1] )),
                              c(")",")",")"))
         }
            
         #Note: round2() is a function in utils.r that does rounding with default formatting
         
       }
          
      #If user set spotlights but not spotlight.labels, assign them
          if (is.null(spotlight.labels)) spotlight.labels=as.numeric(spotlights)
          
  
  #6 Compute simple slopes        
      if (nux <=3)  simple.slopes = compute.slopes.discrete  (ux, zs, model,xvar,zvar)
      if (nux  >3)  simple.slopes = compute.slopes.continuous(spotlights, data, xs,model,xvar,zvar)
       

  #7 Compute floodlight
      if (nux <=3)  floodlight = compute.floodlight.discrete  (ux, zs, model,xvar,zvar)
      if (nux  >3)  floodlight = compute.floodlight.continuous(spotlights, data, xs,model,xvar,zvar)
      

          
  #8 Get fxz and gr
        #fx:  Frequencies of each bin to determine line width and histogram
        #gr:  How transparent to make the line that is being plotted, it's the n observartion in bin over n=100
        
          #Frequencies
            fxz.list = make.fxz(data  , n.bin.continuous,  moderation,nux , max.unique ,spotlights ,xvar,zvar)
            fxz=fxz.list$fxz
            
          #As % of the adequate sample size in n.max
            gr = fxz
            for (j in 1:ncol(fxz)) gr[,j] = pmin(fxz[,j]/n.max,1)

            
  #9 Prepare output to be returned to enable plotting independently by user 
      #clean <- function(str) gsub("[^A-Za-z]", "", str)
      df1 <- data.frame(do.call(rbind, simple.slopes))
      df2 <- data.frame(do.call(rbind, floodlight))
    
    #Drop 
      df1 <- df1[, !names(df1) %in% c("rowid", "y","s.value","p.value","statistic",yvar)]
      df2 <- df2[, !names(df2) %in% c("rowid", "y","s.value","p.value","statistic","term",
                              "predicted_lo",'predicted_hi','predicted',yvar)]

    #Rename      
      names(df1)[names(df1) == "estimate"] <- "y.hat"
      names(df2)[names(df2) == "estimate"] <- "marginal.effect"

      
    #Frequencies
     if (ncol(fxz)==2)  frequencies=data.frame(bin=rownames(fxz), f1=fxz[,1],f2=fxz[,2],row.names = NULL)
     if (ncol(fxz)==3)  frequencies=data.frame(bin=rownames(fxz), f1=fxz[,1],f2=fxz[,2],f3=fxz[,3],row.names = NULL)

      
      output=list(simple.slopes = df1, johnson.neyman = df2, frequencies=frequencies)
    
      
  #10 Remove "GAM" from  figure headers for non-GAM models
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
  
      
      
  #11 Plot for saving 
      if (!is.null(file)) {
              
          #Get extension of file name
              extension= tools::file_ext(file)
                  
          #Type of figure file
              if (extension=='svg') svg(file,width=14,height=7)
              if (extension=='png') png(file,width=14000,height=8000,res=1200)

          #Two plots side by side
                old_mfrow <- par('mfrow')
                par(mfrow=c(1,2))
                on.exit(par(mfrow=old_mfrow)) # Ensure original par settings are restored on function exit
           
          #Plot simple slopes (spotlight)
            make.plot (type='simple slopes', xlab, ylab1, main1, simple.slopes , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                   focal,moderation,max.unique,fxz.list,nux,nuz,xvar,zvar,xlim,ylim,legend.title=legend.simple.slopes)

          #Plot Johson-Neyman (floodlight)
     
           make.plot (type='floodlight', xlab, ylab2, main2, floodlight , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                   focal,moderation,max.unique,fxz.list,nux,nuz,xvar,zvar,xlim,ylim,legend.title=legend.johnson.neyman)  
      
          #End
            message("The figures have been saved to '",file,"'")
            dev.off()        
      }          
                  
  
      
#12 Plot on screen
      if (draw=='both')
        {
    #12.1 #Two plots side by side
                old_mfrow <- par('mfrow')
                par(mfrow=c(1,2))
                on.exit(par(mfrow=old_mfrow)) # Ensure original par settings are restored on function exit
        } 
      
      
    #12.2 Plot simple slopes     
        
        if (draw=='both' | draw %in% c('simple slopes','simple_slopes','simple','simple.slopes'))
          {
          output.simple.slopes = make.plot (type='simple slopes', xlab, ylab1, main1, simple.slopes , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                   focal,moderation,max.unique,fxz.list,nux,nuz,xvar,zvar,xlim,ylim,legend.title=legend.simple.slopes)  
          }
      
      
    #12.3 Plot Floodlight/Johson-Neyman     
      if (draw=='both' | draw %in% c('johnson', 'jn','johnson-neyman','johnsonneyamn','floodlight'))
      {

       output.johnson.neyman = make.plot (type='floodlight', xlab, ylab2, main2, floodlight , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                   focal,moderation,max.unique,fxz.list,nux,nuz,xvar,zvar,xlim,ylim,legend.title=legend.johnson.neyman)  
      }
#12 Add histogram bins (NULL for discrete x1axis)
      if (draw %in% c('both','simple.slopes')) breaks=output.simple.slopes
      if (draw %in% c('johnson.neyman'))       breaks=output.johnson.neyman
     # output$frequencies = cbind(output$frequencies,breaks)
#13 return output for plotting on your own
      if (!is.null(breaks))
      {
      output$frequencies$bin_from=breaks$from
      output$frequencies$bin_to  =breaks$to
      }
      invisible(output)          
}      
