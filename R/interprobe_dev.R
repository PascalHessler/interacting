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
                    draw=TRUE,
                    histogram=TRUE,
                    xlab='moderator',
                    col1='red4',
                    col2='dodgerblue',
                    col3='purple',
                    ylab1='Dependent Variable',
                    ylab2='Marginal Effect',
                    main1="GAM Simple Slopes",
                    main2='GAM Floodlight' , 
                    ...)
                    
  {
  
  #1 Validation
  
       #1.0 Detect input style, use later to decide how to proceed
          input.xz = input.data = input.xyz = input.model <- FALSE
         
          if (!is.null(data))                           input.data=TRUE
          if (!is.null(model))                          input.model=TRUE
          if (!is.null(x) & !is.null(z) & !is.null(y))  input.xyz=TRUE
          if (!is.null(x) & !is.null(z) & is.null(y))   input.xz=TRUE
          
          
      
          
       #1.1 Validate input style combinations
          validate.input.combinations(input.xz, input.xyz, input.data, input.model) 
          
          #NOTE: See ./validate.input.combinations.R

          
      
  
  #------------------------------------------------------------------------------
   
  #2 Get a dataframe
      
      #2.1  If x,y,z are vectors, make it data(x,y,z)
          if (input.data==FALSE & input.xyz==TRUE)  data = data.frame(x=x,z=z,y=y)
          
      #2.2 If model, grab dataset from model
          if (input.model==TRUE)                    data = model$model
          
      
  #------------------------------------------------------------------------------
  
  #3 Create local variables xvar, zvar, yvar
        
      #3.1 input data--> vectorize 
        if (input.data==TRUE | input.model==TRUE)
        {
          xvar=data[,x]
          zvar=data[,z]
          yvar=data[,y]
        }
      

      #3.2 if vectors already
        if (input.data==FALSE & input.xyz==TRUE)
        {
          xvar = x
          zvar = z
          yvar = y
        }
      
          
   
          
  #------------------------------------------------------------------------------
          
    #4 Number of unique x values
        #4.1 Count
          ux  = unique(xvar)   
          uz  = unique(zvar)
          nux = length(ux)     #nux number of unique x values
          nuz = length(uz)     #nuz number of unique z values
          
        #4.2 Check if only 1 value
          if (nux==1) stop("interprobe says: there is only one observed value for the variable 'x'")
          if (nuz==1) stop("interprobe says: there is only one observed value for the variable 'z'")
          
          
  #--------------------------------------------------------------------------------
          
    #5 Estimate model
          #5.1 Already estimated, name it g
            if (input.model==TRUE)  g = model
          
          #5.2 Not yet estimated
            if (input.model==FALSE)
            {
            #Two or three x values
              if (nux<4)  
              {
              #Make xvar a factor to estimate GAM with it
                xvar = factor(xvar)
                
              #Estimate model, with /without k
                if (!is.null(k)) g = mgcv::gam(yvar~s(zvar,by=xvar,k=k)+xvar) 
                if ( is.null(k)) g = mgcv::gam(yvar~s(zvar,by=xvar)+xvar) 
              
              
            }
            
            
          #multi x
            
            
            
          }
          
          
 
          return(namedList(xvar,zvar,yvar,data))
             
}
  

