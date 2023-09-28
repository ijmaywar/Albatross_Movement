%
% Check duplicate files
%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2020_2021';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "GPS"; % Options: 'AGM', 'Axy5', 'AxyAir', 'GPS'
computer = 'MBP'; % Options:'MBP', 'ThinkPad'

%% Set environment

% Matlab functions toolbox
addpath(genpath('/Volumes/LaCie/Functions_Toolboxes/Matlab/'))
addpath(genpath('/Users/ian/Documents/ThorneLab_FlightDynamics/IJM_src/'))

% Full_metadata sheet
fullmeta = readtable('/Volumes/LaCie/Full_metadata.xlsx','Sheet',location,'TreatAsEmpty',{'NA'});

% Specify the field season, location, and (Acc) tag type
if tagtype == "GPS"
    fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);
else
    fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location) & strcmp(fullmeta.ACC_TagType,tagtype),:);
end



% Find files
%directory = strcat('/Volumes/LaCie/L0/Bird_Island/Tag_Data/',szn,'/',tagtype,'/');
directory = '/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/GPS/GPS_Catlog/2020_2021/GPS_L1_2_buffer2km/';
cd(directory)
fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

%%
nfiles = height(fileList);
files_table = table(cell(nfiles,1),cell(nfiles,1),'VariableNames',{'fileName','bird'}); 

for i = 1:nfiles
    files_table.fileName{i} = fileList.name{i};
    
    [~, f,ext] = fileparts(fileList.name{i});
    nameSplit = strsplit(f,'_');
    % THIS NEEDS TO BE UPDATED DEPENDING ON THE DIRECTORY
    files_table.bird{i} = strcat(nameSplit{1});
end

%% Duplicates
unique_birds = unique(files_table.bird);
for i = 1:length(unique_birds)
    find_bird = find(strcmp(files_table.bird,unique_birds(i)));
    if length(find_bird)>1
        disp(strcat(unique_birds(i),' has duplicates.'))
        return
    end
end

if i == length(unique_birds)
    disp('No duplicates found.')
end

%%
file_s1 = '/Volumes/LaCie/L0/Bird_Island/Tag_Data/2020_2021/AGM/20210108-gps687-BBAL_agmfile_S1.csv';
s1 = readtable(file_s1);

%% s2
file_s2 = '/Volumes/LaCie/L0/Bird_Island/Tag_Data/2020_2021/AGM/20210108-gps687-BBAL_agmfile_S2.csv';
s2 = readtable(file_s2);