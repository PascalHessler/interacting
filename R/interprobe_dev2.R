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



interprobe_dev <- function(
                    x=NULL,z=NULL,y=NULL,
                    data=NULL,
                    model=NULL,
                    k=NULL,
                    zs=NULL,
                    spotlights=NULL,
                    spotlight.labels=NULL,
                    histogram=TRUE,
                    max.unique = 11,
                    n.bin.continuous = 5,
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
                    legend.max.d = 5,          #maximum number of decimals
                    legend.min.d = 2,
                    
                    
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
          
        #3.3 Categorize as 'continuous' or 'discrete' moderators
          moderation = ifelse(nuz > max.unique, 'continuous', 'discrete')
          focal      = ifelse(nux > max.unique, 'continuous', 'discrete')
          
      
  #4 set moderator values for computing marginal effects
          if (moderation=='discrete')   zs = uz
          if (moderation=='continuous') zs = seq(min(data$z),max(data$z),length.out=100)
          
        #set focal predictor values
          if (focal=='discrete')   xs = ux
          if (focal=='continuous') xs = seq(min(data$x),max(data$x),length.out=100)

        
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
          if (is.null(spotlight.labels)) spotlight.lables=as.numeric(spotlights)
          
  
  #6 Compute simple slopes        
    
      if (draw.simple.slopes==TRUE) {
          if (nux <=3)  simple.slopes = compute.slopes.discrete  (ux, zs, model)
          if (nux  >3)  simple.slopes = compute.slopes.continuous(spotlights, data, xs)
          }
       
  #7 Compute floodlight
      if (draw.floodlight ==TRUE) {
          if (nux <=3)  floodlight = compute.floodlight.discrete  (ux, zs, model)
          if (nux  >3)  floodlight = compute.floodlight.continuous(spotlights, data, xs)
      }  
    
          
  #8 Get fxz and gr
        #fx:  Frequencies of each bin to determine line width and histogram
        #gr:  How transparent to make the line that is being plotted, it's the n observartion in bin over n=100
        
          #Frequencies
            fxz.list = make.fxz(data  , n.bin.continuous,  moderation)
            fxz=fxz.list$fxz
            
            
          #As % of the adequate sample size in shade.up.to
            gr = fxz
            for (j in 1:ncol(fxz)) gr[,j] = pmin(fxz[,j]/shade.up.to,1)
                

  #9 Return without plotting if not asking to plot
      output=namedList(simple.slopes  ,  floodlight, frequencies=fxz)
      if (draw.simple.slopes==FALSE & draw.floodlight==FALSE) return(output)
          
    
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
              
            if (draw.simple.slopes+draw.floodlight==2) {
                  
                #Two plots side by side
                  old_mfrow <- par('mfrow')
                  par(mfrow=c(1,2))
                  on.exit(par(mfrow=old_mfrow)) # Ensure original par settings are restored on function exit
           
                  }
            
      
      
  #11 Plot simple slopes     
      if (draw.simple.slopes==TRUE)
        
      {
      if (nux >  3) plot.x_on_axis (xlab,ylab1, main1, simple.slopes , histogram, data,xs, ylab1,gr,spotlights,cols,spotlight.labels)
      if (nux <= 3) plot.z_on_axis.R(xlab, simple.slopes, histogram, data,xs, ylab1, spotlights, cols , nux , zs , bins , fxz , nbins)
       
      }
      
  #12 Plot Floodlight/Johson-Neyman     
      if (draw.floodlight==TRUE)
        
      {
      if (nux >  3) plot.x_on_axis (xlab,ylab2, main2, floodlight , histogram, data,xs, ylab1,gr,spotlights,cols,spotlight.labels)
      if (nux <= 3) plot.z_on_axis.R(xlab, simple.slopes, histogram, data,xs, ylab1, spotlights, cols , nux , zs , bins , fxz , nbins)
       
      }
      
      
          

      
          