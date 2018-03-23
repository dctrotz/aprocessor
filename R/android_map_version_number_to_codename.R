#' Map Android version number to Android release name
#' 
#' This function maps a given version number to a release name.  This is very 
#' specific to only official Android releases. See \href{https://source.android.com/setup/build-numbers}{Android: Codenames, Tags, and Build Numbers}
#' For example: `6.0` becomes: `marshmallow`
#' @param version_string A string indicating the version number to be mapped to a release name
#' @keywords android, version
#' @export
#' @examples 
#' android_extract_version_number("4.4")
#' [1] "kitkat"

android_map_version_number_to_codename <- function(version_string) {
    if(str_detect(version_string, "^[^0-9]*1\\.6(\\.[0-9])?")) {
        "donut"
    } else if(str_detect(version_string, "^[^0-9]*2\\.[0-1](\\.[0-9])?")) {
        "eclair"
    } else if(str_detect(version_string, "^[^0-9]*2\\.2(\\.[0-3])?")) {
        "froyo"
    } else if(str_detect(version_string, "^[^0-9]*2\\.3(\\.[0-7])?")) {
        "gingerbread"
    } else if(str_detect(version_string, "^[^0-9]*3\\.[0-2](\\.[0-6])?")) {
        "honeycomb"
    } else if(str_detect(version_string, "^[^0-9]*4\\.0(\\.[0-5])?")) {
        "ice cream sandwich"
    } else if(str_detect(version_string, "^[^0-9]*4\\.[1-3](\\.[0-1])?")) {
        "jellybean"
    } else if(str_detect(version_string, "^[^0-9]*4\\.4(\\.[0-4])?")) {
        "kitkat"
    } else if(str_detect(version_string, "^[^0-9]*5\\.[0-1](\\.[0-1])?")) {
        "lollipop"
    } else if(str_detect(version_string, "^[^0-9]*6\\.0(\\.[0-1])?")) {
        "marshmallow"
    } else if(str_detect(version_string, "^[^0-9]*7\\.[0-1](\\.[0-2])?")) {
        "nougat"
    } else if(str_detect(version_string, "^[^0-9]*8\\.[0-1](\\.[0-9])?")) {
        "oreo"
    } else {
        "unknown"
    }
}
