
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This function navigates Google Drive to find the directories you want
%
% IJM
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function dir = NavigateGD(datalvl,computer,location,szn,tagtype,datatype)

    GD_dir = findGD(computer);
    
    str_datalvl = strcat("L",int2str(datalvl));
    tag_dir = strcat(GD_dir,str_datalvl,"/",location,"/Tag_Data/");

    if datalvl == 0
        dir = strcat(tag_dir,szn,"/",tagtype,"/");
    elseif datalvl == 1
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
    elseif datalvl == 2
        dir = strcat(tag_dir,datatype,"/",szn,"/");
    else
        disp("Can't find data level.")
        return
    end

end
