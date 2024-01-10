#' Probe an interaction with GAM  
#' 
#' The interaction is probed as proposed in Simonsohn (2024), estimating a GAM model
#' and computing both simple slopes and floodlight/Johnson-Neyman procedure.
#'  
#'@param x the predictor of interest (in an experiment, the discrete randomly 
#'assigned manipulation).
#'@param y the dependent variable.
#'@param z the moderator
#'@param col1 color used when x takes its lowest value
#'@param col2 color used when x takes its highest value
#'
#'@export

gam.probe=function(x,z,y,k,zs=NULL,spotlights=NULL,
          xlab='moderator',
          col1='red4',
          col2='dodgerblue',
          coldy='purple',
          ylab1='Dependent Variable',
          ylab2='Marginal Effect',main1="Simple Slopes",main2='Floodlight' , ...)
{
  gam.probe.binary(x=x,z=z,y=y,k=k,zs=zs,spotlights=spotlights,xlab=xlab,
                   ylab1=ylab1,ylab2=ylab2,col1=col1,col2=col2,coldy=coldy,
                   main1=main1, main2=main2,...)
  
}

  
gam.probe.binary = function(x,z,y,k,zs, spotlights, 
                            xlab, ylab1, ylab2, col1,col2,
                            coldy,main1,main2,
                            ...)
{
  
  #moderator range
    if (is.null(zs))   zs = seq(quantile(z,.05),quantile(z,.95),length.out=100)

  #Unique x values
    ux=unique(x)

  #Make dataframe
    x=factor(x)

    df=data.frame(x,z,y)
   
 
  #Check binary
    if (length(unique(x))!=2) stop("x needs to be a binary variable")
    
      xn=factor(xn)
      
      #Binary predictor
        g1 = mgcv::gam(y~s(z,by=x,k=k)+x,data=df) 
    
        #Values of the moderator
          #new data
            nd1=data.frame(z=zs,x=ux[1])  
            nd2=data.frame(z=zs,x=ux[2])  
        
        
          #Prediction values and SET
            fit2 = predict(g1,newdata=nd2, se.fit=TRUE)
            fit1 = predict(g1,newdata=nd1, se.fit=TRUE)
            yh2 = fit2$fit
            yh1 = fit1$fit
            se2 = fit2$se.fit
            se1 = fit1$se.fit
          #Difference
            dy    = yh2 - yh1
            dy.se =  sqrt(se2^2+se1^2)
            
          #Confidence bands
            df   = g1$df.residual
            tc   = qt(p=.975,df=df)
            
            yh2.lb = yh2 - tc * se2
            yh2.ub = yh2 + tc * se2
            yh1.lb = yh1 - tc * se1
            yh1.ub = yh1 + tc * se1
            dy.lb  = dy - tc * dy.se
            dy.ub  = dy + tc * dy.se
            
   
        
            
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
              
              
            #Empty plot
              ylim = range(c(yh1.ub , yh1.lb , yh2.ub , yh2.lb))
              ylim[2]=ylim[2]+.1*diff(ylim)
              plot(zs,yh2,type='n',xlab='',ylab='',las=1,ylim=ylim)

            #Lines    
              line.seg(zs,yh1,lwd=lwd1,col=col1,lty=1) 
              line.seg(zs,yh2,lwd=lwd2,col=col2,lty=2) 
                #line.seg(): segmented line of varying width, see utils. #3
              
            #Quantiles values
             # qt=10:90/100
             #  q1 = quantile(df1$z,qt)
             #  q2 = quantile(df2$z,qt)


                            
            #CI bands
              polygon(x=c(zs,rev(zs)),y=c(yh1.ub,rev(yh1.lb)),col=adjustcolor(col1,.1),border = NA)
              polygon(x=c(zs,rev(zs)),y=c(yh2.ub,rev(yh2.lb)),col=adjustcolor(col2,.1),border = NA)
              
            #Headers
              mtext(side=1,line=2.5,font=2,cex=1.5,xlab)
              mtext(side=2,line=2.5,font=2,cex=1.5,ylab1)
              mtext(side=3,line=1.5,font=2,cex=1.5,main1)
              

            #Legend
              legend("topleft",inset=.01,bty='n',lty=c(1,2),lwd=3,col=c(col1,col2),legend=as.character(ux))
              
        
        #----------------------
              
          #Plot 2
            plot(zs,dy,type='l',col=coldy,lwd=2,xlab='',ylab='',ylim=range(c(dy.ub,dy.lb)))
            polygon(c(zs,rev(zs)),c(dy.ub,rev(dy.lb)),col=adjustcolor(coldy,.1),border=NA)        
            abline(h=0,col='gray78',lty=2)

        #Headers
              mtext(side=1,line=2.5,font=2,cex=1.5,xlab)
              mtext(side=2,line=2.5,font=2,cex=1.5,ylab2)
              mtext(side=3,line=1.5,font=2,cex=1.5,main2)
            
            
          #Add spotlights
              
           #Default values
               if (is.null(spotlights)) spotlights=quantile(z,c(.25,.5,.75) , type=3)  
           
            #Get the ys
              ys1=predict(g1,newdata = data.frame(x=ux[1],z=spotlights))
              ys2=predict(g1,newdata = data.frame(x=ux[2],z=spotlights))
              
            #The delta
              dys=ys2-ys1
              
            #Plot them
              points(spotlights ,dys , pch=16,col=coldy,cex=1.5)
              text(spotlights,dys+.015*diff(ylim),round(dys,1),cex=.8,col=coldy)
            
    #output
        return(invisible(list(
           #Readme
               readme=paste0("The output of the gam.probe() function includes\n",
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
                   gam=g1
                
                #Readme
                
                ))) #End list and return() 
        } #End function
                    
   

