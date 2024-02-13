%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Use this code to explore the durations for which the HRL and GPS tags
% were recording data
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%%
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/";
Sensor_dir = "/Volumes/LaCie/L0/Bird_Island/Tag_Data/2021_2022/Aux/NRL/L0_1_Decompressed/1_SensorData/";
ECG_dir = "/Volumes/LaCie/L0/Bird_Island/Tag_Data/2021_2022/Aux/NRL/L0_1_Decompressed/2_ECG/";
GPS_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2021_2022/Pos/Catlog/";

cd(Sensor_dir)
Sensor_fileList = dir("*.txt");
Sensor_fileList = struct2table(Sensor_fileList);
Sensor_fileNames = string(Sensor_fileList.name);

cd(ECG_dir)
ECG_fileList = dir("*.txt");
ECG_fileList = struct2table(ECG_fileList);
ECG_fileNames = string(ECG_fileList.name);

cd(GPS_dir)
GPS_fileList = dir("*.csv");
GPS_fileList = struct2table(GPS_fileList);
GPS_fileNames = string(GPS_fileList.name);

fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});

nfiles = height(ECG_fileList);

%% Load timings table

% Timings = readtable("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/Aux/NRL/Tag_Meta_Timings.csv");

%% Loop thru

for i = 22:nfiles
    
    %% Make sure all files are present
    cd(ECG_dir)
    namesplit = strsplit(ECG_fileNames(i),'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    birdmeta = fullmeta(strcmp(string(fullmeta.Deployment_ID), current_bird),:);

    % Timings(i,:).dep_ID = {current_bird};
    % Timings(i,:).i = i;  

    % SensorData
    birdSensorname = strcat(current_bird,"_SensorData.txt");
    findSensor = find(strcmp(Sensor_fileNames,birdSensorname));
    if isempty(findSensor)
        disp(strcat("there is no SensorData file for ",current_bird))
    end

    % GPS
    birdGPSname = strcat(current_bird,"_Catlog_L0.csv");
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));
    if isempty(findGPS)
        disp(strcat("There is no GPS file for ", current_bird))
        return
    end
    
    %% Load ECG data
    ECG_data = readtable(ECG_fileNames(i));
   
    %% Load SensorData file
    if ~isempty(findSensor)
        Sensor_data = readtable(strcat(Sensor_dir,Sensor_fileNames(findSensor)));
    end

    %% Load GPS file
    GPS_data = readtable(strcat(GPS_dir,GPS_fileNames(findGPS)));
    if strcmp(GPS_data.Properties.VariableNames{2}, 'Var2')
        disp("Can't load GPS data correctly.")
        return
    end
    
    %% Find fullmeta info
    
    meta_startdatetime = strcat(string(birdmeta.AuxON_date_yyyymmdd), " ", string(birdmeta.AuxON_time_hhmmss));
    meta_startdatetime = datetime(meta_startdatetime, 'InputFormat','yyyyMMdd HHmmss');

    capture_dt = strcat(string(birdmeta.Capture_Date_yyyymmdd), " ", string(birdmeta.Capture_Time_hhmm));
    capture_dt = datetime(capture_dt,'InputFormat','yyyyMMdd HHmm');

    recapture_dt = strcat(string(birdmeta.Recapture_Date_yyyymmdd), " ", string(birdmeta.Recapture_Time_hhmm));
    recapture_dt = datetime(recapture_dt,'InputFormat','yyyyMMdd HHmm');
    
    %% Find ECG duration 

    ECG_SR = 600; % ECG sampling rate is 600 hz
    ECG_dur_hrs = height(ECG_data) / (ECG_SR*60*60);

    final_ECG_dt = meta_startdatetime + hours(ECG_dur_hrs);

    %% Find SensorData duration

    if ~isempty(findSensor)
        Sensor_SR = 75; % Sensor sampling rate is 75 Hz
        Sensor_dur_hrs = height(Sensor_data) / (Sensor_SR*60*60);
        final_Sensor_dt = meta_startdatetime + hours(Sensor_dur_hrs);
    end

    %% Find GPS duration and min/max diff
    
    GPS_DateTime = strcat(string(GPS_data.Date), " ", string(GPS_data.Time));
    GPS_DateTime = datetime(GPS_DateTime,'InputFormat','dd-MMM-yyyy HH:mm:ss');

    % GPS_ON_idx = find(GPS_DateTime >= meta_startdatetime,1);
    % GPS_DateTime = GPS_DateTime(GPS_ON_idx:length(GPS_DateTime));

    first_GPS_dt = GPS_DateTime(1);
    final_GPS_dt = GPS_DateTime(end);
    
    min_diff_GPS_mins = minutes(min(diff(GPS_DateTime)));
    max_diff_GPS_mins = minutes(max(diff(GPS_DateTime)));

    % GPS_dur_hrs = hours(GPS_DateTime(end) - GPS_DateTime(1));

    %% Write table

    Timings.dep_ID(i) = {current_bird};
    Timings.i(i) = i;
    Timings.meta_start(i) = meta_startdatetime;
    Timings.first_GPS(i) = first_GPS_dt;
    Timings.capture(i) = capture_dt;
    Timings.recapture(i) = recapture_dt;
    Timings.final_ECG(i) = final_ECG_dt;

    if ~isempty(findSensor)
        Timings.final_Sensor(i) = final_Sensor_dt;
    end

    Timings.final_GPS(i) = final_GPS_dt;
    Timings.min_diff_GPS_mins(i) = min_diff_GPS_mins;
    Timings.max_diff_GPS_mins(i) = max_diff_GPS_mins;

    % Timings = table({current_bird},i,meta_startdatetime,first_GPS_dt,capture_dt,recapture_dt,final_ECG_dt,final_Sensor_dt,final_GPS_dt,min_diff_GPS_mins,max_diff_GPS_mins, ...
        % 'VariableNames',["dep_ID","i","meta_start","first_GPS","capture","recapture","final_ECG","final_Sensor","final_GPS","min_diff_GPS_mins","max_diff_GPS_mins"]);
    
    %% Save meta tbl
    writetable(Timings,"/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2021_2022/Aux/NRL/Tag_Meta_Timings.csv") %write m data
  
    % %% Find time elapsed per break
    % 
    % % difference_hrs = hours(final_ECG_dt - GPS_DateTime(end))
    % difference_hrs = hours(final_ECG_dt - temp)
    % mins_per_restart = (difference_hrs*60) / tot_restarts
    % 
    % Timings(i,:).difference_hrs = difference_hrs;
    % Timings(i,:).mins_per_restart = mins_per_restart;
    % 
    % %% GPS Figures
    % 
    % GPS_Fig = figure;
    % plot(minutes(diff(GPS_DateTime)))

    %% Clear variables for next loop
    clearvars -except ECG_dir ECG_fileList ECG_fileNames fullmeta GD_dir GPS_dir GPS_fileList GPS_fileNames nfiles Sensor_dir Sensor_fileList Sensor_fileNames Timings i

end
