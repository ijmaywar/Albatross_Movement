
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This code finds breaks in the GPS data to help approximate DateTime for
% HRL data
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway', 'Wandering'

%%
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
Sensor_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/1_SensorData/");
ECG_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/2_ECG/");
GPS_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/GPS/GPS_Catlog/",szn,"/2_buffer2km/");
write_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/datetime_breaks_2/");

cd(Sensor_dir)
Sensor_fileList = dir("*.txt");
Sensor_fileList(startsWith({Sensor_fileList.name},'._')) = [];
Sensor_fileNames = string({Sensor_fileList.name});

cd(ECG_dir)
ECG_fileList = dir("*.txt");
ECG_fileList(startsWith({ECG_fileList.name},'._')) = [];
ECG_fileNames = string({ECG_fileList.name});

cd(GPS_dir)
GPS_fileList = dir("*.csv");
GPS_fileList(startsWith({GPS_fileList.name},'._')) = [];
GPS_fileNames = string({GPS_fileList.name});

fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});

nfiles = height(ECG_fileList);

%% Loop thru

for i = 1:length(ECG_fileList)

    cd(ECG_dir)
    namesplit = strsplit(ECG_fileNames(i),'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    birdmeta = fullmeta(strcmp(string(fullmeta.Deployment_ID), current_bird),:);

    %% Meta start datetime
    meta_startdatetime = strcat(string(birdmeta.AuxON_date_yyyymmdd), " ", string(birdmeta.AuxON_time_hhmmss));
    meta_startdatetime = datetime(meta_startdatetime, 'InputFormat','yyyyMMdd HHmmss');

    ECG_SR = 600; % ECG samples at 600 Hz
    Sensor_SR = 75; % Other NRL sensors sample at 75 Hz

    %% GPS
    birdGPSname = strcat(current_bird,"_GPS_L1_2.csv");
    findGPS = find(strcmp(GPS_fileNames,birdGPSname));
    if isempty(findGPS)
        disp(strcat("There is no GPS file for ", current_bird))
        continue
    end
    
    %% Load GPS file
    m_GPS = readtable(strcat(GPS_dir,GPS_fileNames(findGPS)));
    if strcmp(m_GPS.Properties.VariableNames{2}, 'Var2')
        disp("Can't load GPS data correctly.")
        return
    end

    GPS_diff_mins = minutes(diff(m_GPS.datetime));
    
    % the index for which the datetime jump between this value and the next
    % is greater than 4 minutes
    GPS_breaks = find(GPS_diff_mins > 4);

    %% Create break_tbl

    break_tbl = table();

    % loop thru GPS breaks
    for break_idx = 1:length(GPS_breaks)

        % This is the datetime at which the break starts
        break_start = m_GPS.datetime(GPS_breaks(break_idx));

        % This is the dt at which the break ends
        break_end = m_GPS.datetime(GPS_breaks(break_idx)+1);

        % This is the length (in mins) of the break
        break_length_mins = GPS_diff_mins(GPS_breaks(break_idx));

        % DateTime and sample difference bewteen break_start and NRL_ON
        mins_since_start = minutes(break_start - meta_startdatetime);
        ECG_sample_num = mins_since_start*(ECG_SR*60);
        Sensor_sample_num = mins_since_start*(Sensor_SR*60);

        % How many samples should the break be?
        ECG_sample_dur = break_length_mins*(ECG_SR*60);
        Sensor_sample_dur = break_length_mins*(Sensor_SR*60);

        % Add info to table
        newRow = table(break_start,break_end,break_length_mins,mins_since_start,ECG_sample_num,Sensor_sample_num,ECG_sample_dur,Sensor_sample_dur);
        break_tbl = [break_tbl; newRow];

    end

    %% save break_tbl
    writetable(break_tbl,strcat(write_dir,current_bird,"_dt_breaks.csv"));

end
