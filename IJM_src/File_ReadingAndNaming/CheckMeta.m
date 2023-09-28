%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CheckMeta.m
% Check that all files have a corresponding metadata entry in fullmeta 
% Ian Maywar
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% fileList is the list of files in a directory in table format
%
% split_num is the number of string chunks (seperated by "_") that make up
% the name of the bird
% 
% meta_col is the name of the column in fullmeta to be searched for the
% bird
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function message = CheckMeta(fullmeta,fileList,split_num,meta_col)

    for i = 1:height(fileList)
        namesplit = strsplit(fileList.name{i},"_");
        name = strjoin({namesplit{1:split_num}},{'_'});
       
        findmeta = find(strcmp(fullmeta.(meta_col),name));
        if isempty(findmeta)
            message = strcat(name," cannot be found in metadata");
            return
        elseif length(findmeta)>1
            message = strcat("There are multiple metadata entries for ",name);
            return
        end
    end

    if i==height(fileList)
        message = "Metadata found for every bird.";
    end

    message = strcat("CheckMeta: ", message);

end
