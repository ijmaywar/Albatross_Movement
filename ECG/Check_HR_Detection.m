%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Check a few detected peaks and make sure they look ok in the raw data.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2021_2022';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway', 'Wandering'

%% Set Environment

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/'))

% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
L0_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/2_ECG/");
L1_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/ECG/ECG_NRL/",szn,"/");
dt_break_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/datetime_breaks/");

GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_Season,szn) & strcmp(fullmeta.Location,location),:);

% Tag timings sheet
Tag_Timings = readtable(strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/Tag_Meta_Timings_",szn,".csv"),'Delimiter',',');

% Start indices sheet
idx_tbl = readtable(strcat(L0_dir,"start_stop_indices.csv"),'Delimiter',',');

% Allow figures to be visible
set(0,'DefaultFigureVisible','on')

% suppress annoying warnings when reading Acc_L0 files
warning('off','MATLAB:table:ModifiedAndSavedVarnames')

% load template, template is extracted from the high SNR data .
load('meanbeat');

%% Find dt_breaks
cd(dt_break_dir)
dt_break_fileList = dir('*.csv');
dt_break_fileList(startsWith({dt_break_fileList.name},'._')) = [];
dt_break_fileNames = string({dt_break_fileList.name});

%% Find L1 peak data
cd(L1_dir)
L1_fileList = dir('*.csv');
L1_fileList(startsWith({L1_fileList.name},'._')) = [];
L1_fileNames = string({L1_fileList.name});

%% Find data
cd(L0_dir)
L0_fileList = dir('*.txt');
L0_fileList(startsWith({L0_fileList.name},'._')) = [];
L0_fileNames = string({L0_fileList.name});


%% Loop thru and process birds

for i = 1:length(L0_fileNames)
    %% load data to be deteced.

    namesplit = strsplit(L0_fileNames(i),'_');
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    %% Find dt_break
    cd(dt_break_dir)
    birdDTBREAKname = strcat(dep_ID,"_dt_breaks.csv");
    findDTBREAK = find(strcmp(dt_break_fileNames,birdDTBREAKname));
    if isempty(findDTBREAK)
        % GPS file didn't write to there is no break_tbl so just load an
        % empty break_tbl...
        dt_break_tbl = readtable(strcat(GD_dir,"L0/Bird_Island/Tag_Data/2021_2022/Aux/NRL/L0_1_Decompressed/datetime_breaks/WAAL_20220123_OB00_dt_breaks.csv"));
    else
        dt_break_tbl = readtable(dt_break_fileNames(findDTBREAK));
    end

    %% Find L1 data
    cd(L1_dir)
    L1_name = strcat(dep_ID,"_L1.csv");
    find_L1 = find(strcmp(L1_fileNames,L1_name));
    if isempty(find_L1)
        disp(strcat("No L1 file for ",dep_ID))
    else
        L1_data = readtable(L1_fileNames(find_L1));
        disp(strcat("L1 file loaded for ",dep_ID))
    end


    %% Load ECG data
    cd(L0_dir)
    L0_data = table2array(readtable(L0_fileNames(i)));
    
    disp(strcat("Data loaded for ",dep_ID))


    %% Check a few heartbeats
    for j = 1:5
        % Extract random index from L1_data
        idx = L1_data.idx(randi(height(L1_data)));

        % Find other peaks around this index
        fig_min = idx-1000;
        fig_max = idx+1000;
        fig_L1_data = L1_data(L1_data.idx >= fig_min & L1_data.idx <= fig_max, :);
        
        % Plot the acc data around this index
        figure
        plot(fig_min:fig_max,L0_data(fig_min:fig_max))
        hold on
        y = L0_data(fig_L1_data.idx);
        plot(fig_L1_data.idx,y,'*');

    end
    

end
        