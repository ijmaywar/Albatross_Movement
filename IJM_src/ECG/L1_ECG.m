%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Attach DateTimes to HRL ECG and OtherSensor data
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
L0_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/HRL/L0_1_Decompressed/2_ECG/");
L1_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/ECG/",szn,"/");

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% Prevent figures from popping up when running in the background
set(0,'DefaultFigureVisible','off')

% suppress annoying warnings when reading Acc_L0 files
warning('off','MATLAB:table:ModifiedAndSavedVarnames')

%% Find data

cd(L0_dir)
L0_fileList = struct2table(dir('*.txt'));
L0_fileNames = string(L0_fileList.name);

%% Loop thru and process birds

for i = 1:height(L0_fileNames)
    
    %% load data to be deteced.

    namesplit = strsplit(L0_fileNames(i),'_');
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    % Find HRL_on time from metadata
    birdmeta = fullmeta(strcmp(fullmeta.Deployment_ID,dep_ID),:);
    ON_DateTime = strcat(string(birdmeta.AuxON_date_yyyymmdd), " ", string(birdmeta.AuxON_time_hhmmss));
    ON_DateTime = datetime(ON_DateTime, 'InputFormat','yyyyMMdd HHmmss');

    % Load data
    m = readtable(L0_fileNames(i));
    
    % Add DateTime column
    samplingRate = 1/600; % 600 Hz
    nSamples = height(m);
    
    m.DateTime = (ON_DateTime + seconds(0:samplingRate:(nSamples-1)*samplingRate))';

    % Format DateTime
    m.DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSS';
    if strcmp(location,"Bird_Island")
        m.DateTime.TimeZone = "GMT";
    elseif strcmp(loction,"Midway")
        m.DateTime.TimeZone = "Pacific/Midway";
        % convert to GMT
        m.DateTime.TimeZone = "GMT";
    else
        disp("can't find location.")
        return
    end

    % Format Column names
    if strcmp(intersect("EEG",m.Properties.VariableNames),"EEG")
        m = renamevars(m,"EEG","ECG");
    end

    m = m(:,["DateTime","ECG"]);

    writetable(m,strcat(L1_dir,dep_ID,'_HRL_L1.csv')) %write m data

end







