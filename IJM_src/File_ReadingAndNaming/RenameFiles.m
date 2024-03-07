%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Batch rename files
%   
%   How to use this code:
%   1. Paste the path for the files you want to rename and assign it to the
%   variable "directory"
%   2. Fill in other USER INPUTED VALUES
%   3. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

directory = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/to_share/Gillies/Immersion_2019_2022/2021_2022/LUX/";
fileExt = "lux";

szn = '2021_2022';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
% Genus = "great";
Tag = "GLS"; % Options: "Aux", "GPS", "GLS" 
datatype = "GLS";
TagType = "GLS"; % Options: 
                  % GPS: "Catlog", "iGotU"
                  % Aux: "AGM", "Axy5", "AxyAir", "GCDC", "NRL", "uMoth"
                  % GLS 
                  % AxyTrek
datalvl = 0; % Options: 0, 1, 2
% datasublvl = 2; % Options: 1, 2, 3
computer = "MacMini"; % Options: "MacMini", "MacBookPro"

newname = false; % Options: true, false

%% Set environment

GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/";

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:); % & strcmp(fullmeta.Genus,Genus),:);
% fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location) & strcmp(fullmeta.Aux_TagType,TagType),:);
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn),:);


cd(directory)
fileList = dir(strcat('*.',fileExt));
fileList(startsWith({fileList.name},'._')) = [];
fileNames = string({fileList.name});

% fileList = dir;
% fileList = fileList(4:end,:);

fileList(endsWith({fileList.name},'driftadj.lux')) = [];

nfiles = height(fileList);
rename_table = table(cell(nfiles,1),cell(nfiles,1),cell(nfiles,1),cell(nfiles,1),'VariableNames',{'Old_fileName','Old_ID','New_fileName','Deployment_ID'}); 

%% Loop through each file and make a list of current and new file names
for id = 1:nfiles
   
    % Get the file name 
    % fileName = fileList(id).name;
    fileName = fileList(id).name;
    rename_table.Old_fileName{id} = string(fileName);
    [~, f,ext] = fileparts(fileName);
    nameSplit = strsplit(f,'_');

    % Find metadata
    if ~newname % For OG_IDs   
        if ismember(Tag,["GPS","AxyTrek"])
            num_ = count(string(fullmeta.Pos_OG_ID(1)),"_");
            Old_BirdName = findOBN(num_,nameSplit);
            findmeta = find(strcmp(fullmeta.Pos_OG_ID,Old_BirdName));
        elseif ismember(Tag,["Aux"])           
            num_ = count(string(fullmeta.Aux_OG_ID(1)),"_");
            Old_BirdName = findOBN(num_,nameSplit);
            findmeta = find(strcmp(fullmeta.Aux_OG_ID,Old_BirdName));
        elseif strcmp(Tag,"GLS")
            num_ = count(string(fullmeta.GLS_OG_ID(1)),"_");
            Old_BirdName = findOBN(num_,nameSplit);
            darvic = nameSplit{3};
            findmeta = find(strcmp(fullmeta.GLS_OG_ID,Old_BirdName));
            % findmeta = find(strcmp(fullmeta.Darvic,darvic));
        else
            disp("Can't find Tag.")
            return
        end

    else % For names that have already been updated to the naming convention but need to be tweaked a little bit.
        num_ = 2;%count(string(fullmeta.Deployment_ID(findmeta)),"_");
        Old_BirdName = findOBN(num_,nameSplit);
        findmeta = find(strcmp(fullmeta.Deployment_ID,Old_BirdName));
    end

    rename_table.Old_ID{id} = string(Old_BirdName);

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
        % rename = Dep_ID;
        rename = strcat(Dep_ID,'_',TagType,'_L0',ext); 
        % rename = strcat(Dep_ID,'_',TagType,'_L0_1_3_HeaderPos',ext); 
    elseif datalvl == 1
        rename = strcat(Dep_ID,'_',datatype,'_L1',ext);
    elseif datalvl == 2
        rename = strcat(Dep_ID,'_',datatype,'_L2',ext);
    elseif datalvl == 3
        % rename = strcat(Dep_ID,'_',datatype,'_L3',ext);
        rename = strcat(Dep_ID,'_',datatype,'_L3_600s',ext);
    else
        disp("Data level not found.")
        return
    end
    rename_table.New_fileName{id} = string(rename); 
end

% Check for duplicates
% unique_birds = unique(string(rename_table.Old_ID));
% for i = 1:length(unique_birds)
%     find_bird = find(strcmp(rename_table.Old_ID,unique_birds(i)));
%     if length(find_bird)>1
%         disp(strcat(unique_birds(i), "has duplicates."))
%         return
%     end
% end

disp("rename_table has been written. Check for duplicates and then check rename_table to make sure it's correct before continuing.")

%% Check for duplicates in files
unique_files = unique(string(rename_table.New_fileName));
for i = 1:length(unique_files)
    find_bird = find(strcmp(string(rename_table.New_fileName),unique_files(i)));
    if length(find_bird)>1
        disp(strcat(string(find_bird), " has duplicates."))
        return
    end
end

if i == length(unique_files)
    disp("There are no duplicate file names.")
end

%% Write rename file
mkdir rename_info
writetable(rename_table,strcat(directory,'/rename_info/rename_table.csv'),'delimiter',',');

%% Safety

% so that I don't accidentally run the rename section

%% RENAME FILES: Check to see that the file names look correct first!!!!
% Make sure that the tagtype and the data step is correct !!!!
for id = 1:height(rename_table)
    if ~strcmp(rename_table.Old_fileName{id},rename_table.New_fileName{id})
        movefile(rename_table.Old_fileName{id}, rename_table.New_fileName{id});
    end
end

disp("Files renamed.")

%%









%% Functions

% Find old bird name

function Old_BirdName = findOBN(num_,nameSplit)
    if num_ == 0
        Old_BirdName = nameSplit{1};
    elseif num_ == 1
        Old_BirdName = strcat(nameSplit{1},"_",nameSplit{2});
    elseif num_ == 2
        % Old_BirdName = strcat(nameSplit{1},"_",nameSplit{2},"_",nameSplit{3}(1:4));
        Old_BirdName = strcat(nameSplit{1},"_",nameSplit{2},"_",nameSplit{3});
    else
        disp("Cannot figure out Old_BirdName.")
    end
end

