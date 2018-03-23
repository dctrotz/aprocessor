#' Fix invalid android version strings with assumed values
#' 
#' This function attmepts to fix invalid android version strings that are a 
#' single number. The fix makes the assumption that the number represents the
#' major version number of the release.  However, prior to Android 5.0 the major 
#' version number was shared across a number of releases:
#' i.e. 4.0 Ice Cream Sandwich, 4.1 Jellybean, and 4.4 KitKat.
#' See \href{https://source.android.com/setup/build-numbers}{Android: Codenames, Tags, and Build Numbers}
#' @param version_string A string indicating the major version number to be fixed
#' @param snap_to_min A logical value indicating whether the version number 
#' should snap to the minimum version for the given major value otherwise it
#' will snap to the latest known version for the major version number provided.
#' @keywords android, version
#' @export
#' @examples 
#' android_version_append_minor_version("5")
#' [1] "5.1.1"

android_version_append_minor_version <- function(version_string, snap_to_min = FALSE) {
	if (version_string == "5") {
		if(snap_to_min) {
			"5.0"
		} else {
			"5.1.1"
		}
	} else if (version_string == "6") {
		if(snap_to_min) {
			"6.0"
		} else {
			"6.0.1"
		}
	} else if (version_string == "7") {
		if(snap_to_min) {
			"7.0"
		} else {
			"7.1.2"
		}
	} else if (version_string == "8") {
		if(snap_to_min) {
			"8.0"
		} else {
			"8.1"
		}
	} else {
		version_string
	}
}
