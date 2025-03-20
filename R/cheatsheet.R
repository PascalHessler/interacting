#' Open the cheatsheet for interacting pkg
#'
#' @export
cheatsheet <- function() {
  #Cookie
    cheatsheet_cookie <- file.path(tools::R_user_dir("interacting", which = "cache"), "cheatsheet_used")
    dir.create(dirname(cheatsheet_cookie), showWarnings = FALSE, recursive = TRUE)

     if (!file.exists(cheatsheet_cookie)) {
          writeLines('1', cheatsheet_cookie)
        }
  
  # Get the path to the cheatsheet image
  cheatsheet_path <- system.file("cheatsheet.png", package = "interacting")
  
  # Check if the file exists
  if (cheatsheet_path == "") {
    stop("______Cheatsheet not found. Ensure that 'cheatsheet.png' is inside 'inst/' in your package.")
  }
  
  # Open the image using the default system viewer
  utils::browseURL(cheatsheet_path)
}