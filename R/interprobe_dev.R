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


interprobe <- function(
                    x=NULL,z=NULL,y=NULL,
                    data=NULL,
                    model=NULL,
                    k=NULL,
                    zs=NULL,
                    spotlights=NULL,
                    draw=TRUE,
                    histogram=TRUE,
                    nbins=10,
                    shade.up.to = 50,  #below this sample size we shade to show few observations
                    xlab='moderator',
                    cols=c('red4','blue4','green4'),
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
  
  #3 Create data if given vectors 
        
      #3.1 input data--> vectorize 
        #if (input.data==TRUE | input.model==TRUE)
        #{
          #xvar=data[,'x']
          #zvar=data[,'z']
          #yvar=data[,'y']
        #}
          
        

      #3.2 if vectors 
        if (input.data==FALSE & input.xyz==TRUE)
        {
          #Put vectors into dataframe
            data=data.frame(x, z, y)
          #Rename variables to refer to variable names
            x='x'
            z='z'
            y='y'
        }
      
          
  #------------------------------------------------------------------------------
          
    #4 Number of unique x values
        #4.1 Count
          ux  = sort(unique(data$x))
          uz  = sort(unique(data$z))
          nux = length(ux)     #nux number of unique x values
          nuz = length(uz)     #nuz number of unique z values
          
        #4.2 Check if only 1 value
          if (nux==1) stop("interprobe says: there is only one observed value for the variable 'x'")
          if (nuz==1) stop("interprobe says: there is only one observed value for the variable 'z'")
          
  #--------------------------------------------------------------------
  #5 set moderator values
        #3.3 Set of values for z moderations
          if (is.null(zs))
            {
              if (nuz<=nbins) zs = uz
              if (nuz>nbins)  zs = sequence(min(data$z),max(data$z),length.out=nbins)
              
            }
    #How many bins of moderator value
          nbins=length(zs)
  #--------------------------------------------------------------------------------
          
  #6 Estimate model (if the user did not enter a model)
          
      if (input.model==FALSE)
      {
        #6.1 x has 2 or 3 possible values
        
            if (nux %in% c(2,3))
               {
             #Make xvar a factor to estimate GAM with it
                  data$x = factor(data$x)
                    
             #Estimate model, with /without k
                  if (!is.null(k)) model = try(mgcv::gam(y~s(z,by=x,k=k)+x, data=data),silent=TRUE )
                  if ( is.null(k)) model = try(mgcv::gam(y~s(z,by=x)+x, data=data),    silent=TRUE )
                  check.gam.error(model) #check.gam.error.R - stops if gam gave an error msg
              } #End nux<4
            
        
        #6.2 x has 4+ values        
            if (nux>=4)
            {
            if (!is.null(k)) model = try(mgcv::gam(y~s(z,k)+s(x,k=k)+ti(x,z,k=k),data=data),silent=TRUE) 
            if ( is.null(k)) model = try(mgcv::gam(y~s(z)  +s(x)    +ti(x,z),data=data),silent=TRUE) 
            check.gam.error(model) #check.gam.error.R - stops if gam gave an error msg

            }
             
      } #ENd run model 
  #--------------------------------------------------------------------------------
     
      #7 Compute simple slopes 
        
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
              simple.slopes[[j]] = marginaleffects::predictions(model, newdata = ndj,by='z')
             
              j=j+1 
            } #End loop

          }  #End nux in 2,3
          
          
          
        #7.2 x has more 4+ values
          if (nux>3)
          {
            
            
            
          }
        
 #--------------------------------------------------------------------------------
  #8 Compute floodlight / Johnson-Neyman
        
      #8.1 x has 2 or 3 possible values
          if (nux %in% c(2,3))
          {
          nd = expand.grid(z=zs,x=ux)
          floodlight = marginaleffects::slopes(model, newdata = nd,by='z')
          
          #Subset
            floodlight = data.frame(floodlight[1:((nux-1)*length(zs)),]  )
                
          #Marginal effects produces x3 - x2, x2 -x1  and x1, we only need the first two.
          #We take the number of x values, for each there are zs marginal effects, so
          #nx*nz gives us the number of estimates we want to keep.
          }  
    
    
 #--------------------------------------------------------------------------------
  #9 Plot Simple Slopes
        
           

    #9.1  x has 2 or 3 possible values
        if (nux %in% c(2,3))
        {
          
          #Unlist data.frames
           simple.slopes.df <- do.call(rbind, simple.slopes)
 
               #Combines the 2 or 3 dataframes, currentlhy in a list, 
               #on  each possible x-value in a single dataframe with 
               #estimate, SE, and conf.int
           
        #Set ylim
            ylim = range(simple.slopes.df[,c('conf.low','conf.high')]) #Default y-range
            ylim[2]=ylim[2]+.1*diff(ylim)                                   #Add at the top for the legend
            if (histogram==TRUE) ylim[1]=ylim[1]-.15*diff(ylim)             #add at the bottom for the histogram
              
        #Split the x-axis into intervals (# of min(unique values,  zcutoff)
              z_intervals <- cut(data$z, breaks = length(zs))
              
              
        #Frequency of xs in each intervale
              fx = table(data$x,z_intervals)  #Frequencies
              px = prop.table(fx,1)           #share of observations
              fz = table(data$z,z_intervals)  #Frequencies
              
          #Color adjustment for each line
              gr=list()
              for (j in 1:nux)
              {
              #Color is going to be shaded if it is n=20, or < 1/3 the uniform distribution
                gray.frequency = pmin(fx[j,]/shade.up.to ,1)
                gray.percent   = pmin(px[j,]/((1/nbins)*(1/3)),1)  #Share in relation to 1/3 of the uniform expectation
                gr[[j]] = pmin(gray.frequency, gray.percent)
                
              }
              
          #Empty plot
              plot(zs,simple.slopes[[1]]$estimate,type='n',xlab='',ylab='',las=1,ylim=ylim)
              ltys=c(1,2,4)
          #Loop the 2 or 3 values of x slopes
              for (j in 1:nux) {
                #Lines
                  #line.seg(zs,simple.slopes[[j]]$estimate,lwd=rep(4,nbins), col=cols[j],g=gr[[j]],lty=j)
                  line.seg(zs,simple.slopes[[j]]$estimate,lwd=4*gr[[j]], col=cols[j],g=gr[[j]],lty=ltys[j]) 
                
                  #Changing both width and tly leads to weird looking lines
              
                #Confidence regions
                  polygon(x=c(zs,rev(zs)),
                        y=c(simple.slopes[[j]]$conf.high,
                            rev(simple.slopes[[j]]$conf.low)),
                            col=adjustcolor(cols[j],.03),border = NA)
                  
               #Dots if less than nbins<20
                  if (nbins<=20) points(zs,simple.slopes[[j]]$estimate, col=adjustcolor2(cols[j],gr[[j]]),pch=16) 
                
              }
              
          #Headers
            mtext(side=1,line=2.5,font=2,cex=1.5,xlab)
            mtext(side=2,line=3,font=2,cex=1.5,ylab1)
            mtext(side=3,line=1.5,font=2,cex=1.5,main1)
     
          
          #Legend
              legend("topleft",inset=.01,bty='n',lty=ltys[1:nux],lwd=3,col=cols[1:nux],legend=as.character(ux))
              
            #histograms at the bottom
              #if (histogram==TRUE)
              #{
                
              #Less than 20 - Discrete vertical bars
                #if (nbins<20)
                #{
                #Width in plot between zs
                  bin.width=zs[2]-zs[1]

                y0=par('usr')[3]                   #bottom of graph
                y1=y0+.1*diff(ylim)                #10% of vertical distance for this
                h = y0+(fx/max(fx))*.1*diff(ylim)
                for (j in 1:3) segments(x0=zs + (j-1)*.08*bin.width -.04*bin.width,
                                        x1=zs + (j-1)*.08*bin.width -.04*bin.width,
                                        y0=y0,y1=h[j,],col=cols[j],lwd=4)
                
                
                text(zs,y1,colSums(fx),cex=.8,pos=3,font=3,col='gray38')
               
                  
                  
                }
                
              #Set braks to concide with those already in the graph
                breaks=axTicks(1)
                breaks=c(min(z),breaks,max(z))
                h1=hist(z[x==ux[1]],plot=FALSE,breaks=breaks)
                h2=hist(z[x==ux[2]],plot=FALSE,breaks=breaks)

             #Shorter variable names
                b1=h1$breaks
                b2=h2$breaks
                c1=h1$counts
                c2=h2$counts
              
            #Adjust y coordinates too be bottom of figure
              y0=par('usr')[3]
              y1=y0+.1*diff(ylim)
              d1 =y0+ (c1 /max(c1+c2)) * (y1-y0)
              d2 =d1+ (c2 /max(c1+c2)) * (y1-y0)

            
              for (k in 1:length(h1$mids) )
              {
                
                polygon(x=c(b1[k],b1[k],b1[k+1],b1[k+1]),
                        y=c(y0,d1[k],d1[k],y0),col=col1)
                polygon(x=c(b1[k],b1[k],b1[k+1],b1[k+1]),
                        y=c(d1[k],d2[k],d2[k],d1[k]),col=col2) 
                
              } #End for
              
              
          } #End if histogram==TRUE  
            
            
            
          
          
          
          #If <20 moderator values, plot them individually
          
            if (nuz<20) {
                freqs = list()
                for (xk in ux) freqs[table(data$z[data$x==xk])
                
              
            }
            
          
          #Plot the slopes
            #Ylims
              ylim = range(c(yh1.ub , yh1.lb , yh2.ub , yh2.lb))
              ylim[2]=ylim[2]+.1*diff(ylim)
              
            #Space for histogram
              if (histogram==TRUE) ylim[1]=ylim[1]-.15*diff(ylim)
              
            #Empty plot
              plot(zs,yh2,type='n',xlab='',ylab='',las=1,ylim=ylim)
         
          
        #Gap between contiguous zs
          tick.width = zs[2]-zs[1]
          
        #Subset of values with uni
          
         
            #histograms at the bottom
              if (histogram==TRUE)
              {
              #Set braks to concide with those already in the graph
                breaks=axTicks(1)
                breaks=c(min(z),breaks,max(z))
                h1=hist(z[x==ux[1]],plot=FALSE,breaks=breaks)
                h2=hist(z[x==ux[2]],plot=FALSE,breaks=breaks)

             #Shorter variable names
                b1=h1$breaks
                b2=h2$breaks
                c1=h1$counts
                c2=h2$counts
              
            #Adjust y coordinates too be bottom of figure
              y0=par('usr')[3]
              y1=y0+.1*diff(ylim)
              d1 =y0+ (c1 /max(c1+c2)) * (y1-y0)
              d2 =d1+ (c2 /max(c1+c2)) * (y1-y0)

            
              for (k in 1:length(h1$mids) )
              {
                
                polygon(x=c(b1[k],b1[k],b1[k+1],b1[k+1]),
                        y=c(y0,d1[k],d1[k],y0),col=col1)
                polygon(x=c(b1[k],b1[k],b1[k+1],b1[k+1]),
                        y=c(d1[k],d2[k],d2[k],d1[k]),col=col2) 
                
              } #End for
           
        
          
 
          return(namedList(xvar,zvar,yvar,data))
             
}
  

