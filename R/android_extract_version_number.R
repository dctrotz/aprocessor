#' Extract Android Version Number
#' 
#' This function attempts to extract a version number from string data that
#' has other information about the version.  
#' For example: `Android MarshMallow 6.0 Os For MaPan` becomes: `6.0`
#' @param str The string containting the version info to be extracted
#' @keywords android, version
#' @export
#' @examples 
#' android_extract_version_number("Android MarshMallow 6.0 Os For MaPan")

android_extract_version_number <- function(str) { 
    m <- str_match(str, "^[^0-9]*([0-9])(\\.[0-9])(\\.[0-9])?")[,2:4]
    paste(m[!is.na(m)], sep="", collapse="")
}
