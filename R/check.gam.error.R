#This is run after a gam is estimated to provide useful information about something being wrong
#note: it is assuming that errors always arise because of high k
  
  check.gam.error=function(model)
  {
  
  #Give detailed error if try-error
      if ("try-error" %in% class(model)) {
        msg=paste0("interprobe() says: The GAM model could not be estimated. ",
             "This often occurs when predictors have few possible values. ",
             "You can rely on the argument 'k' in interprobe(), setting it to ",
             "a low value like k=3 or k=2 and see if that fixes the problem.")
        message(format_msg (msg, header='Problem.'))
        exit()
      }
   
  
}