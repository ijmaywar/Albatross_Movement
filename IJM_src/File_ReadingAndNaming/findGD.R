
###########################################################################
#
# This function navigates your computer's directory to find the Drive for
# Desktop folder
#
# IJM
#
###########################################################################

findGD <- function(computer) {

    if (computer == "MacMini") {
        GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/";
    } else if (computer == "MacBookPro") {
        GD_dir = ""; # Fill this in....
    } else {
        disp("Cannot find computer.")
        break
    }
 
    return (GD_dir) 
}