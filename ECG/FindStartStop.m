%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Attach DateTimes to NRL ECG and OtherSensor data
%
%   Reformat data to follow a uniform format (uniformat):
%       DateTime in GMT
%       Ax
%       Ay
%       Az
%       Pressure
%       Temperature
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
L1_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/ECG/",szn,"/");
GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_Season,szn) & strcmp(fullmeta.Location,location),:);

% Start indices sheet
idx_tbl = readtable(strcat(L0_dir,"start_stop_indices.csv"));

% Allow figures to be visible
set(0,'DefaultFigureVisible','on')

% suppress annoying warnings when reading Acc_L0 files
warning('off','MATLAB:table:ModifiedAndSavedVarnames')

% samples frequency
fs = 600;

%% Find data
cd(L0_dir)
L0_fileList = dir('*.txt');
L0_fileList(startsWith({L0_fileList.name},'._')) = [];
L0_fileNames = string({L0_fileList.name});

% Manually work thru all birds

    %% Load data
    namesplit = strsplit(L0_fileNames(i),'_');
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    L0_data = table2array(readtable(L0_fileNames(i)));
    
    % local detrend
    L0_data = locdetrend(L0_data,fs,[1 .1]);

    disp("Data loaded.")

    %% Find start_idx
    fig_start = 1;
    HFF = 8000000; % How Far Forward (How much of the head is plotted)
    figure
    plot(fig_start:fig_start+HFF-1,L0_data(fig_start:fig_start+HFF-1))
    ylim([-6 6])
    disp('Click where the usable data starts')
    [start_idx,~] = ginput(1);
    start_idx = round(start_idx);

    %% Find og_length
    og_length = height(L0_data)-fig_start+1;

    %% Find stop_idx
    stop_idx = fig_start+og_length-1; % stop_idx is the last data point unless user finds and clicks a better value.
    HFB = 4000000; % How Far Back (How much of the tail is plotted)
    figure
    plot(og_length-HFB:og_length,L0_data(og_length-HFB:og_length))
    ylim([-6 6])
    disp('Click where the usable data ends')
    [stop_idx,~] = ginput(1);
    stop_idx = round(stop_idx);

    %% Save start and stop idx

    tbl_idx = find(string(idx_tbl.bird) == dep_ID);

    if length(tbl_idx)==1
        idx_tbl.bird{tbl_idx} = dep_ID;
        idx_tbl.start(tbl_idx) = start_idx;
        idx_tbl.stop(tbl_idx) = stop_idx;
        idx_tbl.length(tbl_idx) = og_length;
    else
        disp("Manually set the index of the table in which this data should be recorded.")
    end

    disp("Table updated.")

    %% Save table, clear vars, and move on to the next bird

    writetable(idx_tbl,strcat(L0_dir,"start_stop_indices.csv"));
    disp("Table saved.")
    close all
    clearvars -except i idx_tbl L0_fileNames fs L0_dir
    i = i+1

