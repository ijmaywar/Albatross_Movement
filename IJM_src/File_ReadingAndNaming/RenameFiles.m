%
% Batch rename files
%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2022_2023';
location = 'Midway'; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "Catlog"; % Options: 'AGM', 'Axy5', 'AxyAir', 'Catlog', 'iGotU'
computer = 'MBP'; % Options: 'MBP', 'ThinkPad'

%% Set environment

if strcmp(computer,'MBP')
    % Matlab functions toolbox
    addpath(genpath('/Users/ian/Documents/ThorneLab_FlightDynamics/IJM_src/'))
    fullmeta = readtable('/Volumes/LaCie/Full_metadata.xlsx','Sheet',location,'TreatAsEmpty',{'NA'});
elseif strcmp(computer,'ThinkPad')
    addpath(genpath("G:/Other computers/My MacBook Pro (1)/ThorneLab_FlightDynamics/IJM_src"))
    fullmeta = readtable("G:/My Drive/Full_metadata.xlsx",'Sheet',location,'TreatAsEmpty',{'NA'});
else
    disp("Can't Find Computer")
    return
end

% Specify the field season, location, and Acc tag type
if ismember(tagtype,["Catlog","iGotU"])
    fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);
else
    fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location) & strcmp(fullmeta.ACC_TagType,tagtype),:);
end

% Find files
%directory = strcat('/Volumes/LaCie/L0/Bird_Island/Tag_Data/',szn,'/',tagtype,'/');
directory = '/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Midway/Tag_Data/2022_2023/Catlog/';
cd(directory)
fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

nfiles = height(fileList);
rename_table = table(cell(nfiles,1),cell(nfiles,1),cell(nfiles,1),cell(nfiles,1),'VariableNames',{'Old_fileName','Old_ID','New_fileName','Deployment_ID'}); 

%% Loop through each file and make a list of current and new file names
for id = 1:nfiles
   
    % Get the file name 
    rename_table.Old_fileName{id} = string(fileList.name{id});
    [~, f,ext] = fileparts(fileList.name{id});
    nameSplit = strsplit(f,'_');

    % CHANGE THIS ACCORDINGLY
    Old_BirdName = strcat(nameSplit{1},"_",nameSplit{2},"_",nameSplit{3});
    rename_table.Old_ID{id} = string(Old_BirdName);

    % Find metadata
    if ismember(tagtype,["Catlog","iGotU"])
        findmeta = find(strcmp(fullmeta.GPS_OG_ID,Old_BirdName));
    else
        findmeta = find(strcmp(fullmeta.Acc_OG_ID,Old_BirdName));
    end

    if isempty(findmeta)
        disp(strcat(Old_BirdName," cannot be found in metadata."))
        return
    elseif length(findmeta)>1
        disp(strcat(Old_BirdName," has more than one metadata entry."))
        return
    else
        birdmeta = fullmeta(findmeta,:);
        % Deployment ID: SPEC_capdate_darvic
        % L0: Dep_ID_TagType_L0
        % L1: Dep_ID_DataType_L1_level
        Dep_ID = birdmeta.Deployment_ID;
        rename_table.Deployment_ID{id} = string(Dep_ID);
    
        % CHANGE THIS ACCORDINGLY
        rename = strcat(Dep_ID,'_',tagtype,'_L0',ext);
        rename_table.New_fileName{id} = string(rename); 
    end
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

