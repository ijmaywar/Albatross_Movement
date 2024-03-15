%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert .dat file to .txt for NRL files
%
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2021_2022';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway', 'Wandering'

%% Set Environment

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/NEW_STRUCTURE/";
HD_dir = "/Volumes/LaCie/";
L0_0_dir = strcat(HD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_0_Raw/");
L0_1_dir = strcat(HD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/");

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

cd(L0_0_dir)
fileList = dir('*.dat');
% Remove artifacts that start with '._' (which are created by using an external HD)
fileList(startsWith({fileList.name},'._')) = [];

%% Unpack Raw Neurologger .dat files

for i = 1:length(fileList)
    
    %%
    namesplit = strsplit(fileList(i).name,'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    % bird = fileList(i).name(1:8);
    %     
    % set parameters-----------------------------------------------------------
    Parameters.NChannels = 1;  % number of electrophysiological channels
    Parameters.sps = 600;     % Hz , was 1600
    Parameters.Ku = 166.6;    % input range +/-6 mV
    Parameters.Coef = 2000/Parameters.Ku/1024;    %2V, 10 bit probably +/-0.5mV range
    
    
    % Import data--------------------------------------------------------------
    [Calibration, ECG_t, Accel_t, Pressure_t, Temp_t, Gyro_t, Magn_t, Sync_t,HeaderPosition] = Datalogger4ChConverter_MARG_Altimeter_final(fileList(i).name, Parameters);
    
    % Write header positions in the text file-----------------------------------
    filenamei = strcat(L0_1_dir, "3_HeaderPos/", current_bird, '_HeaderPos.txt');
    writematrix(HeaderPosition', filenamei);
    
    %Get tempterature and pressure---------------------------------------------
    [ T, P ] = GetCalibTemperaturePressureBMP388( Temp_t, Pressure_t, Calibration );  %Celsius, Pascals
    
    % Extract sensor data------------------------------------------------------
    ECG_t = ECG_t(:,1);
    ECG = single(cast(ECG_t,'uint16'))*Parameters.Coef;
    ECG = ECG - repmat(median(ECG),size(ECG,1),1);
    
    plott(ECG, 600)
    
    Accel = zeros(size(Accel_t),'int16');
    for I_t = 1:3
      Accel(:,I_t) = typecast(Accel_t(:,I_t),'int16');  
    end    
    Accel = single(Accel)/256/128*16; %now +/-16g, was+/-8g
    
    Gyro = zeros(size(Gyro_t),'int16');
    for I_t = 1:3
      Gyro(:,I_t) = typecast(Gyro_t(:,I_t),'int16');  
    end    
    Gyro = single(Gyro)/256/128*2000*pi/180; %2000 degrees/s -> rad/s
    
    Magn = zeros(size(Magn_t),'int16');
    for I_t = 1:3
      Magn(:,I_t) = typecast(Magn_t(:,I_t),'int16');  
    end    
    Magn = single(Magn)/256/128*49.152; %49.152 Gauss
    
    %% rotate acc+gyr axes to align with mag axes (mag frame is aligned with bird frame in 2019-2020 tag configuration)
    % to see schematics of sensor orientations, see neurologger_sensor_frame_log.pdf 
        ax_rotated = Accel(:,2).*-1;
        ay_rotated = Accel(:,1).*-1;
        az_rotated = Accel(:,3);
        
        gx_rotated = Gyro(:,2).*-1;
        gy_rotated = Gyro(:,1).*-1;
        gz_rotated = Gyro(:,3);
        
        Accel_r = [ax_rotated, ay_rotated, az_rotated];
        Gyro_r  = [gx_rotated, gy_rotated, gz_rotated];
    
    %% save sensor files and ECG file separately
    
    SensorMat = [Accel_r, Magn, Gyro_r, T, P]; % 75 Hz
    % ECG = ECG; % 600 Hz
    
    sensorfilename = strcat(L0_0_dir, "output_mat/", current_bird,'_sensormatraw75hz.mat');
    save(sensorfilename, 'SensorMat', '-v7.3')

    eegfilename = strcat(L0_0_dir, "output_mat/", current_bird,'_eegmatraw600hz.mat');
    save(eegfilename,'ECG','-v7.3')
    
    writematrix(ECG, strcat(L0_1_dir, "2_ECG/", current_bird, "_ECG.txt")); % 'delimiter',',')
    writematrix(SensorMat,strcat(L0_1_dir, "1_SensorData/", current_bird, "_SensorData.txt")); % 'delimiter',',')
    

    %%
    clearvars -except fileList GD_dir L0_0_dir L0_1_dir location szn i

end

