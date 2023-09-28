function fileList_noaux = exFAT_aux_remove(fileList)
    fileList_noaux = fileList(strcmp(extractBefore(fileList.name,3),'._')==false,:);
end



