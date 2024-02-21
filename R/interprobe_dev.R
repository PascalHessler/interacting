#' Probe interactions computing Simple Slopes and Floodlight (Johnson-Neyman) 
#' 
#' The interaction is probed, by default, as proposed in Simonsohn (2024), estimating a GAM model
#' and computing both simple slopes and floodlight/Johnson-Neyman procedure.
#'  
#'@param x the predictor of interest (in an experiment, the discrete randomly 
#'assigned manipulation).
#'@param y the dependent variable.
#'@param z the moderator
#'@param draw boolean TRUE/FALSE for whether to make the plot
#'@param col1 color used when x takes its lowest value
#'@param col2 color used when x takes its highest value
#'
#'@export



interprobe_dev <- function(model,
                    x,z,y,
                    data,
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
          if (!missing(data))                            input.data=TRUE
          if (!missing(model))                           input.model=TRUE
          if (!missing(x) | !missing(y) | !missing(z))   input.xyz=TRUE
          if (!missing(x) |  missing(y) | !missing(z))   input.xz=TRUE
          
       #1.1 Validate input style combinations
          validate.input.combinations(input.xz, input.xyz, input.data, input.model) 
          
              #See ./validate.input.combinations.R

          
       #1.2 Check that x,y,z are of the same length
         if (input.xyz==TRUE & input.data==FALSE & input.model==FALSE) {
           nx=length(x)
           ny=length(y)
           nz=length(z)
           if (nx!=ny | nx!=nz) {
             stop (paste0("interprobe says: the vectors x,y,z are of different ",
                          "length\nnx=",nx," | ny=",ny,"  | nz=",nz))
           
           } #Different length
         }#Input xyz
  
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
 
          return(namedList(xvar,zvar,yvar,data))
             
}
  


x=rnorm(100)
z=rnorm(100)+5
z=rnorm(100)+25

interprobe_dev(x,y,z)


  
  
  
  #2 Is model provided
  
  #Estimate model if not provided 
     if (is.null(model)==TRUE)
     {
           #is x binary?
              binary  <- length(unique(data$x))==2   #True if exactly 2 unique values
      
          #If x is binary, make it factor
              if (binary==TRUE) data$x=factor(data$x)
             
          #Run the model
             gam1 = run.gam(data,k)  #see script: 'run.gam.R'
             
             
             
             
         
      #moderator grid
         if (is.null(zs))   zs = seq(min(z),max(z),length.out=100)
  
      #Prediction datasets
         
         #binary
           if (binary==TRUE)
           {
             ux=unique(x)
             nd1 = data.frame(z=zs,x=ux[1])  
             nd2 = data.frame(z=zs,x=ux[2])  
             
             
           }
         
        #non-binary
          if (binary==FALSE)
           {
            
             
             
           }
           
       
 
        
          #Prediction values and SET
            fit2 = predict(gam1,newdata=nd2, se.fit=TRUE)
            fit1 = predict(gam1,newdata=nd1, se.fit=TRUE)
            yh2 = fit2$fit
            yh1 = fit1$fit
            se2 = fit2$se.fit
            se1 = fit1$se.fit
          #Difference
            dy    = yh2 - yh1
            dy.se =  sqrt(se2^2+se1^2)
            
          #Confidence bands
            df   = gam1$df.residual
            tc   = qt(p=.975,df=df)
            
            yh2.lb = yh2 - tc * se2
            yh2.ub = yh2 + tc * se2
            yh1.lb = yh1 - tc * se1
            yh1.ub = yh1 + tc * se1
            dy.lb  = dy - tc * dy.se
            dy.ub  = dy + tc * dy.se
            
        #Spotlight 
               spotlights.default=FALSE
               if (is.null(spotlights)) {
                 spotlights=quantile(z,c(.15,.5,.85) , type=3) 
                 spotlights.default=TRUE
                   }
       
            
      #Get the ys
              ys1=predict(gam1,newdata = data.frame(x=ux[1],z=spotlights))
              ys2=predict(gam1,newdata = data.frame(x=ux[2],z=spotlights))
       
            #The delta
              dys=ys2-ys1
        
              
                    
        
    if (draw==TRUE)    
    {
      
      #Start the figure
     par(mfrow=c(1,2))
     
          #Plot 1 - Simple Slopes
            
            #Set widths of lines
              z2 = z[x==ux[2]]
              z1 = z[x==ux[1]]
              tick.width = zs[2]-zs[1]
              w2 = sapply(zs,function(x) share.within(x,z2, 3*tick.width))
              w1 = sapply(zs,function(x) share.within(x,z1, 3*tick.width))
              lwd2 = rescale(w2, min1=.35 , max=8)
              lwd1 = rescale(w1, min1=.35 , max=8)
              gr1 = rescale(w1, min1=.01 , max=.9)
              gr2 = rescale(w2, min1=.01 , max=.9)
              
            #Ylims
              ylim = range(c(yh1.ub , yh1.lb , yh2.ub , yh2.lb))
              ylim[2]=ylim[2]+.1*diff(ylim)
              
            #Space for histogram
              if (histogram==TRUE) ylim[1]=ylim[1]-.15*diff(ylim)
              
            #Empty plot
              plot(zs,yh2,type='n',xlab='',ylab='',las=1,ylim=ylim)
              
              
            #Lines    
              #line.seg(zs,yh1,lwd=lwd1,col=col1,g=gr1,lty=1) 
              #line.seg(zs,yh2,lwd=lwd2,col=col2,g=gr2,lty=2) 
              line.seg(zs,yh1,lwd=rep(4,length(z)),col=col1,g=gr1,lty=1) 
              line.seg(zs,yh2,lwd=rep(4,length(z)),col=col2,g=gr2,lty=2) 
            
                            
            #CI bands
              polygon(x=c(zs,rev(zs)),y=c(yh1.ub,rev(yh1.lb)),col=adjustcolor(col1,.1),border = NA)
              polygon(x=c(zs,rev(zs)),y=c(yh2.ub,rev(yh2.lb)),col=adjustcolor(col2,.1),border = NA)
              
            #Headers
              mtext(side=1,line=2.5,font=2,cex=1.5,xlab)
              mtext(side=2,line=3,font=2,cex=1.5,ylab1)
              mtext(side=3,line=1.5,font=2,cex=1.5,main1)
              

            #Legend
              legend("topleft",inset=.01,bty='n',lty=c(1,2),lwd=3,col=c(col1,col2),legend=as.character(ux))
              
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
              
              
          } #End if histogram==TRUE
              
                  #----------------------
              
          #Plot 2
              #Color adustment  for line of floodlight
                grm=pmin(gr1,gr2)
              
              #ylim
                ylim=range(c(dy.ub,dy.lb))
                
              #Space for histogram
                if (histogram==TRUE) ylim[1]=ylim[1]-.15*diff(ylim)
                
              #Start the plot
                plot(zs,dy,type='n',col=col3,lwd=2,xlab='',ylab='',ylim=ylim,las=1)
         
                  
            
           #Quantiles ilnes
            
         
            line.seg(zs,dy,lwd=rep(4,length(z)),col=col3,g=grm,lty=1) 
            polygon(c(zs,rev(zs)),c(dy.ub,rev(dy.lb)),col=adjustcolor(col3,.1),border=NA)        
            abline(h=0,col='gray80',lty=1)

            
            
        #Headers
              mtext(side=1,line=2.5,font=2,cex=1.5,xlab)
              mtext(side=2,line=3,font=2,cex=1.5,ylab2)
              mtext(side=3,line=1.5,font=2,cex=1.5,main2)
            
            
          #Add spotlights
              
            
         
            #Plot them
              points(spotlights ,dys , pch=16,col=col3,cex=1.5)
              text(spotlights+.03*diff(range(zs)),dys+.03*diff(ylim),round(dys,1),cex=.8,col=col3)
              
            #Legend
              legend('top',pch=16,col=col3,legend='15th, 50th and 85th percentile',bty='n',cex=.8,pt.cex=1.2,text.col = col3)
    
              
          #histograms at the bottom
            if (histogram==TRUE) {
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
                
              } #End of for
                    
       }#End of if (histogram==TRUE)
           
              
    } #End if (plot==TRUE)
       
      #output
        return(invisible(list(
           #Readme
               readme=paste0("The output of the interprobe() function includes\n",
                                '1) df.full.grid: a dataframe with values needed to plot\n',
                                'the simple slopes and the floodlight figures based on \n',
                                'the full range of moderator values considered.\n\n',
                                '2) df.spotlight: the three values for highlight in the\n',
                                'floodlight graph, equivalent to a spotlight analysis.\n\n',
                                '3) gam: The gam model on which the results are based'),
            
                #Predicted values based on full range
                  df.whole.grid= data.frame(zs=zs, 
                                            yhat1=yh1 , 
                                            se.1=se1,
                                            yhat2=yh2,
                                            se.2=se2,
                                            dy, 
                                            se.dy=dy.se),
                #just spotlight
                    df.spotlight = data.frame(xs=spotlights, dys=dys),
                
              
                #Gam model
                   gam=gam1
                
                #Readme
                
                ))) #End list and return() 
              
        } #End if is.null() 
  
} 
                    
   

