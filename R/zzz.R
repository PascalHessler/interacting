
    
        
  .onLoad <- function(libname, pkgname) {
  
     
  
    } #End of onLoad


#8. Attaching 
    
    .onAttach <- function(libname, pkgname) {
        
        cheatsheet_cookie <- file.path(tools::R_user_dir("interacting", which = "cache"), "cheatsheet_used")

      
      #While developing:
         packageStartupMessage ("Attached: 'interacting' (version: ",packageVersion('interacting'),  ")\n",
                                "##################################################################\n",
                                "DEVELOPMENT VERSION - DOUBLE CHECK IF USING FOR PUBLISHABLE PAPERS\n",
                              "This Version 2025 03 20 - 11.38AM\n",
                                "***      Subject to breaking changes and possible errors      ***")
      #Cheatsheet 
         if (!file.exists(cheatsheet_cookie)) {
            packageStartupMessage("\nRun  `cheatsheet()` to see how to customize labels, axes, and other settings in interprobe()")
          }
         
  } #End on attach

