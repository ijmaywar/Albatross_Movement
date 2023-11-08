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

szn = '2019_2020';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway', 'Wandering'
computer = "MacMini";

%% Set Environment

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% set directories
GD_dir = findGD(computer);
L0_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/2_ECG/");
L1_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/ECG/",szn,"/");
GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

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
L0_fileList = struct2table(dir('*.txt'));
L0_fileNames = string(L0_fileList.name);

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
    HFF = 100000000; % How Far Forward (How much of the head is plotted)
    figure
    plot(fig_start:HFF,L0_data(fig_start:HFF))
    ylim([-6 6])
    disp('Click where the usable data starts')
    [start_idx,~] = ginput(1);
    start_idx = round(start_idx);

    %% Find stop_idx
    og_length = height(L0_data);
    stop_idx = og_length; % stop_idx is the last data point unless user finds and clicks a better value.
    HFB = 10000000; % How Far Back (How much of the tail is plotted)
    figure
    plot(og_length-HFB:og_length,L0_data(og_length-HFB:og_length))
    ylim([-6 6])
    disp('Click where the usable data ends')
    [stop_idx,~] = ginput(1);
    stop_idx = round(stop_idx);

    %% Save start and stop idx

    idx_tbl.bird{i} = dep_ID;
    idx_tbl.start(i) = start_idx;
    idx_tbl.stop(i) = stop_idx;
    idx_tbl.length(i) = og_length;

    disp("Table updated.")

    %% Save table, clear vars, and move on to the next bird

    writetable(idx_tbl,strcat(L0_dir,"start_stop_indices.csv"));
    disp("Table saved.")
    close all
    clear L0_data dep_ID start_idx stop_idx og_length
    i = i+1

