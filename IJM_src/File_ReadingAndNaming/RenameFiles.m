%
% Batch rename files
%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2020_2021';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "GLS"; % Options: 'AGM', 'Axy5', 'AxyAir', 'Catlog', 'iGotU'
datatype = "GLS"; % Options: "Accelerometer", "GPS", "GLS", "Magnetometer", "EEG"
datalvl = 0; % Options: 0 ,1, 2
computer = "MacMini"; % Options: "MacMini", "MacBookPro"

newname = true; % Options: true, false

%% Set environment

GD_dir = findGD(computer);

% Full_metadata sheet
% fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'Sheet',location,'TreatAsEmpty',{'NA'});
% fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'Sheet','HRL','TreatAsEmpty',{'NA'});
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'Sheet','GLS','TreatAsEmpty',{'NA'});
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% Find files
directory = NavigateGD(datalvl,computer,location,szn,tagtype,datatype);
% directory = strcat(directory,"AxyTrek");
cd(directory)
fileList = exFAT_aux_remove(struct2table(dir('*.txt')));

nfiles = height(fileList);
rename_table = table(cell(nfiles,1),cell(nfiles,1),cell(nfiles,1),cell(nfiles,1),'VariableNames',{'Old_fileName','Old_ID','New_fileName','Deployment_ID'}); 

%% Loop through each file and make a list of current and new file names
for id = 1:nfiles
   
    % Get the file name 
    rename_table.Old_fileName{id} = string(fileList.name{id});
    [~, f,ext] = fileparts(fileList.name{id});
    nameSplit = strsplit(f,'_');

    % CHANGE THIS ACCORDINGLY
    Old_BirdName = strcat(nameSplit{1});%,"_",nameSplit{2},"_",nameSplit{3});
    rename_table.Old_ID{id} = string(Old_BirdName);

    % Find metadata
    if ~newname % For OG_IDs   
        if ismember(tagtype,["Catlog","iGotU"])
            findmeta = find(strcmp(fullmeta.GPS_OG_ID,Old_BirdName));
        elseif ismember(tagtype,["AGM","Axy5","AxyAir","Technosmart"])
            findmeta = find(strcmp(fullmeta.Acc_OG_ID,Old_BirdName));
        elseif strcmp(tagtype,"GLS")
            findmeta = find(strcmp(fullmeta.GLS_OG_ID,Old_BirdName));
        end
    else % For names that have already been updated to the naming convention but need to be tweaked.
        findmeta = find(strcmp(fullmeta.Deployment_ID,Old_BirdName));
    end

    if isempty(findmeta)
        disp(strcat(Old_BirdName," cannot be found in metadata."))
        return
    elseif length(findmeta)>1
        disp(strcat(Old_BirdName," has more than one metadata entry."))
        return
    else
        birdmeta = fullmeta(findmeta,:);
    end

    % Deployment ID: SPEC_capdate_darvic
    % L0: Dep_ID_TagType_L0
    % L1: Dep_ID_DataType_L1_level
    Dep_ID = birdmeta.Deployment_ID;
    rename_table.Deployment_ID{id} = string(Dep_ID);

    % CHANGE THIS ACCORDINGLY
    if datalvl == 0
        rename = strcat(Dep_ID,'_',tagtype,'_L0',ext); 
    elseif datalvl == 1
        rename = strcat(Dep_ID,'_',datatype,'_L1',ext);
    elseif datalvl == 2
        rename = strcat(Dep_ID,'_',datatype,'_L2',ext);
    else
        disp("Data level not found.")
        return
    end
    rename_table.New_fileName{id} = string(rename); 
end

% Check for duplicates
unique_birds = unique(string(rename_table.Old_ID));
for i = 1:length(unique_birds)
    find_bird = find(strcmp(rename_table.Old_ID,unique_birds(i)));
    if length(find_bird)>1
        disp(strcat(unique_birds(i), "has duplicates."))
        return
    end
end

disp("rename_table has been written and there are no duplicate files. Check rename_table to make sure it's correct before continuing.")

%% Write rename file
mkdir rename_info
writetable(rename_table,strcat(directory,'rename_info/rename_table.csv'),'delimiter',',');

%% Safety

% so that I don't accidentally run the rename section

%% RENAME FILES: Check to see that the file names look correct first!!!!
% Make sure that the tagtype and the data step is correct !!!!
for id = 1:height(rename_table)
    movefile(rename_table.Old_fileName{id}, rename_table.New_fileName{id});
end

