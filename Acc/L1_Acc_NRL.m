%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Attach DateTimes to NRL Sensor data
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
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway'

%% Set Environment

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/'))

% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
L0_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/1_SensorData/");
L1_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/Acc/Acc_NRL/",szn,"/");
GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_Season,szn) & strcmp(fullmeta.Location,location),:);

% Prevent figures from popping up when running in the background
set(0,'DefaultFigureVisible','off')

% suppress annoying warnings when reading Acc_L0 files
warning('off','MATLAB:table:ModifiedAndSavedVarnames')

%% Find data

cd(GPS_dir)
GPS_fileList = dir('*.csv');
GPS_fileList(startsWith({GPS_fileList.name},'._')) = [];
GPS_fileNames = string({GPS_fileList.name});
GPS_fileList = struct2table(GPS_fileList);

cd(L0_dir)
L0_fileList = dir('*.txt');
L0_fileList(startsWith({L0_fileList.name},'._')) = [];
L0_fileNames = string({L0_fileList.name});
L0_fileList = struct2table(L0_fileList);

CheckMetaGPSUnique(L0_fileList,GPS_fileList,fullmeta)

%% Loop thru and process birds

for i = 15:15 %:height(L0_fileList)
    
    
    namesplit = strsplit(L0_fileNames(i),'_');
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    
    %%
    meta = struct;
    meta.bird = dep_ID;
    meta.step = 0;
    parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));

    % Find NRL_on time from metadata
    birdmeta = fullmeta(strcmp(fullmeta.Deployment_ID,dep_ID),:);
    ON_DateTime = strcat(string(birdmeta.AuxON_date_yyyymmdd), " ", string(birdmeta.AuxON_time_hhmmss));
    ON_DateTime = datetime(ON_DateTime, 'InputFormat','yyyyMMdd HHmmss');

    % Read data
    cd(GPS_dir)
    birdGPSname = strcat(dep_ID,'_GPS_L1_2.csv');
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));
    GPSdata = readtable(GPS_fileNames(findGPS));  

    cd(L0_dir)
    m = readtable(L0_fileNames(i));

    %% s1
    
    samplingRate = 1/75; % 75 Hz

    % Add DateTime and DateTime corrected columns
    m.DateTime = (ON_DateTime + seconds(0:samplingRate:(height(m)-1)*samplingRate))';

    % Downsample to 25 Hz
    m = m(1:3:end,:);

    if sum(strcmp(m.Properties.VariableNames,'Var1'))==1
        % name columns if they were never named
        m.Properties.VariableNames = {'Ax','Ay','Az','Magx','Magy,','Magz','Gx','Gy','Gz','TempDegC','PRPa','DateTime'}
    end

    % Get rid of unecessary columns
    m = m(:,["DateTime","Ax","Ay","Az","PRPa","TempDegC"]);
    
    % Format DateTime
    m.DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';
    if strcmp(location,"Bird_Island")
        m.DateTime.TimeZone = "GMT";
    elseif strcmp(loction,"Midway")
        m.DateTime.TimeZone = "Pacific/Midway";
        % convert to GMT
        m.DateTime.TimeZone = "GMT";
    else
        disp("can't find location.")
    end

    % Rename columns
    m = renamevars(m,"PRPa","Pressure");
    m = renamevars(m,"TempDegC","Temperature");

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): s1 complete.'))

    %% s2

    [m,s2_meta] = s2_Acc(m,dep_ID,fullmeta);

    meta.s2 = s2_meta;
    
    %%
    if s2_meta.skip == 1
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): skipped at s2.'))
        meta.step = 2;
        parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): s2 complete.'))

    %% s3

    [m,s3_timetbl,s3_meta] = TripTrim(m,GPSdata,dep_ID);

    meta.s3 = s3_meta;
    meta.timetbl = s3_timetbl;
    
    %%     
    if ~isempty(find(s3_meta.skip == 1, 1))
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): skipped at s3.'))
        meta.step = 3;
        parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    %  Check to make sure the data is continuous
    if ~CheckContinuous(m,25)
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): data is not continuous after s3.'))
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): s3 complete.'))
        
    %% s4

    [m,Q,HasNaN] = s4_Acc(m,L1_dir,dep_ID);

    meta.HasNaN = HasNaN;
    meta.Q = Q;
    meta.step = 4;
    parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));

    writetable(m,strcat(L1_dir,dep_ID,'_Acc_L1.csv')) %write m data
    
    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): s4 complete.'))

    m = []; % to free up memory

end








