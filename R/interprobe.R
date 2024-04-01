#' Probe interactions computing Simple Slopes and Floodlight (Johnson-Neyman) 
#' 
#' The interaction is probed, by default, as proposed in Simonsohn (2024), estimating a GAM model
#' and computing both simple slopes and floodlight/Johnson-Neyman procedure.
#'  
#'@param x the predictor of interest (in an experiment, the discrete randomly 
#'assigned manipulation). Can be the name of a variable (e.g., x='treatment') 
#'or data (e.g., x=c(1,3,1,4)
#'@param z the moderator. Can be the name of variable, or a vector with data
#'@param y the dependent variable. Can be the name of variable, or a vector with data
#'@param data an optional data frame with variables used in the model
#'@param model an optional model which will be probed (can be any model accepted 
#'by package 'marginaleffects' including lm, glm, gam
#'@param k level of smoothness/flexibility used by mgcv::gam() to fit functions, 
#'the default used by interprobe() is k=3, increasing it will lead to more wiggly
#' functions increasing risk of over-fitting. mgcv::gam() uses a much higher default
#' which interprobe opts-out of by specifying it to be k=3. In gam it is gam(y~s(x,k=3)).
#'@param spotlights vector with three values of the moderator at which simple slopes 
#'and Johnsohn-Neyman curve are computed. Defaults to 15th, 50th and 85th percentile
#'of moderator values in the data.
#'@param spotlight.labels labels to use in the legend to indicate the spotlight values
#'colors used in the plot (defaults to blue, red, green)
#'@param histogram logical on whether sample sizes are depicted under the 
#'@param max.unique integer with the cutoff at which interprobe() reports frequencies
#'for sets rather than individual values in the histogram
#'@param n.bin.continuous integer with number of bins to make for histogram
#'@param shade.up.to integer with the frequency count at which color signaling
#'sample size maxes out and not longer gets darker with bigger samples.
#'@param xlab label for the x axis, defaults to be "Focal Predictor" when x is on the x-axis
#'and "Moderator" when z is in the x-axis. Users should replace default with a
#'clear descriptor of the variables (e.g., "z: age of participants")
#'@param ylab1 label for the y axis of the simple slopes plot (defaults to 
#''Dependent Variable'). 
#'@param ylab2 label for the y axis of the Johnson-Neyman plot (defaults to
#''Marginal Effect'). 
#'@param main1 Header for simple slopes figure (defaults to 'GAM Simple Slopes')
#'@param main2 Header for Johnson Neyman figure (defaults to 'GAM Johnson-Neyman')
#'@param legend.max.d highest number of decimals to show for moderator in legend in figure
#'@param legend.min.d lowest  number of decimals to show for moderator in legend in figure
#'
#'@param cols vector with three colors used in the plot (defaults to blue, red, green)
#'@param draw TRUE/FALSE for whether to make a plot

#'@export



interprobe <- function(
                    x=NULL,z=NULL,y=NULL,
                    data=NULL,
                    model=NULL,
                    k=3,
                    spotlights=NULL,
                    spotlight.labels=NULL,
                    histogram=TRUE,
                    max.unique = 11,
                    n.bin.continuous = 10,
                    shade.up.to = 50,           #below this sample size we shade to show few observations
                    xlab='',
                    cols=c('red4','blue4','green4'),
                    ylab1='Dependent Variable',
                    ylab2='Marginal Effect',
                    main1="GAM Simple Slopes",
                    main2="GAM Johnson-Neyman",
                    legend.max.d=4,
                    legend.min.d=1,

                    draw=TRUE,
                    
                    ...)
                    
  {
  
  #0 If x is specified and it is in a model/data version of interprobe() treat as string
    if (!is.null(model) | !is.null(data)) {
        x <- deparse(substitute(x))
        z <- deparse(substitute(z))
        if (!is.null(y)) y <- deparse(substitute(y))
    }
  
  #1 Validate input and determine what was provided, vector, model, or data.frame
  
  #First legnth and type of arguments
    validate.arguments(x, z ,y , data, model, k,spotlights,spotlight.labels,histogram, max.unique,n.bin.continuous, shade.up.to ,
                              xlab,ylab1,ylab2,main1,main2,cols,draw)      
  
  #Then combination to determine if we were given a model or a dataset or vectors
    v = validate.input.combinations(data , model, x, y ,z)
        
  #2 Create data
    #Extract if provided
      if (v$input.data==FALSE & v$input.xyz==TRUE)  data = data.frame(x=x,z=z,y=y)
      if (v$input.model==TRUE)                      data = model$model
      
    #Combine vectors otherwise
        if (v$input.data==FALSE & v$input.xyz==TRUE)
        {
          #Put vectors into dataframe
            data=data.frame(x, z, y)
          #Rename variables to refer to variable names
            x='x'
            z='z'
            y='y'
        }
             

  #3 Number of unique x & z values
        #3.1 Count
          ux  = sort(unique(data$x))
          uz  = sort(unique(data$z))
          nux = length(ux)     #nux number of unique x values
          nuz = length(uz)     #nuz number of unique z values
          
        #3.2 Check if only 1 value
          if (nux==1) stop("interprobe says: there is only one observed value for the variable 'x'")
          if (nuz==1) stop("interprobe says: there is only one observed value for the variable 'z'")
          
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
          if (moderation=='continuous') zs = seq(min(data$z),max(data$z),length.out=100)
          
        #set focal predictor values
          if (focal!='continuous')   xs = ux
          if (focal=='continuous')   xs = seq(min(data$x),max(data$x),length.out=100)

        
  #5 Estimate model (if the user did not provide it as an argument)
          if (v$input.model==FALSE) model = estimate.model(nux,data,k)

  
  #6 Set spotlight values and labels
        
       if (is.null(spotlights)) {
         spotlights=quantile(data$z,c(.15,.5,.85),type=3)
         if (is.null(spotlight.labels)) {
             spotlight.labels=paste0(
                              c("15th percentile (","50th percentile (", "85th percentile (") ,
                              c(round2(as.numeric(spotlights), max.d=legend.max.d, min.d=legend.min.d )),
                              c(")",")",")"))
         }
            
         #Note: round2() is a function in utils.r that does rounding with default formatting
         
       }
          
      #If user set spotlights but not spotlight.labels, assign them
          if (is.null(spotlight.labels)) spotlight.labels=as.numeric(spotlights)
          
  
  #6 Compute simple slopes        
      if (nux <=3)  simple.slopes = compute.slopes.discrete  (ux, zs, model)
      if (nux  >3)  simple.slopes = compute.slopes.continuous(spotlights, data, xs,model)
       
  #7 Compute floodlight
      if (nux <=3)  floodlight = compute.floodlight.discrete  (ux, zs, model)
      if (nux  >3)  floodlight = compute.floodlight.continuous(spotlights, data, xs,model)
      
          
  #8 Get fxz and gr
        #fx:  Frequencies of each bin to determine line width and histogram
        #gr:  How transparent to make the line that is being plotted, it's the n observartion in bin over n=100
        
          #Frequencies
            fxz.list = make.fxz(data  , n.bin.continuous,  moderation,nux , max.unique ,spotlights )
            fxz=fxz.list$fxz
            
            
          #As % of the adequate sample size in shade.up.to
            gr = fxz
            for (j in 1:ncol(fxz)) gr[,j] = pmin(fxz[,j]/shade.up.to,1)
                

  #9 Prepare output to be returned to enable plotting independently by user 
      clean <- function(str) gsub("[^A-Za-z]", "", str)
      df1 <- data.frame(do.call(rbind, simple.slopes))
      df2 <- data.frame(do.call(rbind, floodlight))
      df1 <- df1[, !names(df1) %in% c("rowid", "y","s.value","p.value","statistic")]
      df2 <- df2[, !names(df2) %in% c("rowid", "y","s.value","p.value","statistic","term","predicted_lo",'predicted_hi','predicted')]
      names(df1) = c('yhat','se.yhat','conf.low','conf.high',clean(z),clean(x))
      names(df2) = c('dydx','se.dydx','conf.low','conf.high',clean(z),clean(x))
      df1=df1[,c(6,5,1,2,3,4)]
      df2=df2[,c(6,5,1,2,3,4)]
      output=list(simple.slopes = df1, floodlight = df2, frequencies=fxz)
      if (draw==FALSE) return(output)
          
    
  #10 Prepare the canvas for plotting
    
          #10.1 Remove "GAM" from  figure headers for non-GAM models
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
          
          #10.2 Choose 1 or 2 plots
            #Two plots side by side
                old_mfrow <- par('mfrow')
                par(mfrow=c(1,2))
                on.exit(par(mfrow=old_mfrow)) # Ensure original par settings are restored on function exit
           
      
      
  #11 Plot simple slopes     
        make.plot (type='simple slopes', xlab, ylab1, main1, simple.slopes , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                   focal,moderation,max.unique,fxz.list,nux,nuz)

  #12 Plot Floodlight/Johson-Neyman     
     
      make.plot (type='floodlight', xlab, ylab2, main2, floodlight , histogram, data,xs, zs, gr,spotlights,cols,spotlight.labels,
                   focal,moderation,max.unique,fxz.list,nux,nuz)  
      
      
  #13 return output for plotting on your own
     
      invisible(output)          
    }      