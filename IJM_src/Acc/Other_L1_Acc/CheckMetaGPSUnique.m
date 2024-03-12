function CheckMetaGPSUnique(L0_fileList,GPS_fileList,fullmeta)
    % Make sure that all birds have metadata and GPS data
    for i = 1:height(L0_fileList)
    
        if height(L0_fileList) == 1
            namesplit = strsplit(L0_fileList.name,'_');
        else
            namesplit = strsplit(L0_fileList.name{i},'_');
        end
        dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
        bird_names{i} = dep_ID;
        
        % meta
        findmeta = find(strcmp(fullmeta.Deployment_ID,dep_ID));
        if isempty(findmeta)
            disp(strcat(dep_ID," cannot be found in metadata"))
            return
        elseif length(findmeta)>1
            disp(strcat("There are multiple metadata entries for ",dep_ID))
            return
        end
    
        % GPS
        birdGPSname = strcat(dep_ID,"_GPS_L1_2.csv");
        findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));
    
        if isempty(findGPS)
            disp(strcat("There is no GPS file for ", dep_ID))
            return
        elseif length(findGPS)>1
            disp(strcat("There are multiple GPS files for ", dep_ID))
            return
        end
    end
    
    % check that there's only one file per bird
    uniquebirds = unique(bird_names);
    
    if length(uniquebirds) ~= height(L0_fileList)
        disp("There is a bird with multiple files.")
        return
    end
    
    disp('Metadata and GPS data found for each bird')
end