
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This function navigates Google Drive to find the directories you want
%
% IJM
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function dir = NavigateGD(computer,datalvl,location,szn,tagtype,datatype)

    GD_dir = findGD(computer);
    tag_dir = strcat(GD_dir,"/",datalvl,"/",location,"/Tag_Data/");

    if strcmp(datalvl,"L0")
        dir = strcat(tag_dir,szn,"/",tagtype,"/");
    elseif strcmp(datalvl,"L1")
        if contains(tagtype,["AGM","Axy5","AxyAir"])
            tagtype = "Technosmart";
        end
        
        if strcmp(datatype,"Accelerometer")
            dir = strcat(tag_dir,datatype,"/Acc_",tagtype,"/",szn,"/");
        elseif strcmp(datatype,"GPS")
            dir = strcat(tag_dir,datatype,"/GPS_",tagtype,"/",szn,"/");
        else
            dir = strcat(tag_dir,datatype,"/",szn,"/");
        end
    elseif strcmp(datalvl,"L2")
        dir = strcat(tag_dir,datatype,"/",szn,"/");
    else
        disp("Can't find data level.")
        return
    end

end
