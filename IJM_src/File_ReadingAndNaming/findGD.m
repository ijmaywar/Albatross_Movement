
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This function navigates your computer's directory to find the Drive for
% Desktop folder
%
% IJM
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function GD_dir = findGD(computer)

    if strcmp(computer,"MacMini")
        GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/";
    elseif strcmp(computer,"MacBookPro")
        GD_dir = ""; % Fill this in....
    else
        disp("Cannot find computer.")
        return
    end

end