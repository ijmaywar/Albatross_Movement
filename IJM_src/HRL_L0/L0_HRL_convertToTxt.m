%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Convert .dat file to .txt for HRL files
%
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway', 'Wandering'
computer = "MacMini";

%% Set Environment

% set directories
GD_dir = findGD(computer);
L0_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/HRL/L0_0_Raw_Compressed_dat/");
L1_Acc_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/Acc/Acc_HRL/",szn,"/");
L1_ECG_dir = strcat(GD_dir,"L1/",location,"/Tag_Data/ECG/",szn,"/");

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

cd(L0_dir)
fileList = dir('*.dat'); 

%% Unpack Raw Neurologger .dat files

for i= 1:length(fileList)
    
    namesplit = strsplit(fileList(i).name,'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    % FileOutputName = [pwd filesep 'OutputFolder' filesep FileName];
    % bird = fileList(i).name(1:8);
    %     
    % set parameters-----------------------------------------------------------
    Parameters.NChannels = 1;  %number of electrophysiological channels
    Parameters.sps = 600;     % Hz , was 1600
    Parameters.Ku = 166.6;    %input range +/-6 mV
    Parameters.Coef = 2000/Parameters.Ku/1024;    %2V, 10 bit probably +/-0.5mV range
    
    
    % Import data--------------------------------------------------------------
    [Calibration, EEG_t, Accel_t, Pressure_t, Temp_t, Gyro_t, Magn_t, Sync_t,HeaderPosition] = Datalogger4ChConverter_MARG_Altimeter_final([FileName '.dat'], Parameters);
    
    
    %Write header positions in the text file-----------------------------------
    % fileID = fopen([FileOutputName '_HeaderPos.txt'],'w');
    % for I_t = 1:length(HeaderPosition)
    %   fprintf(fileID,'%d\n',HeaderPosition(I_t));  
    % end
    % fclose(fileID);
    
    %Get tempterature and pressure---------------------------------------------
    [ T, P ] = GetCalibTemperaturePressureBMP388( Temp_t, Pressure_t, Calibration );  %Celsius, Pascals
    
    % Extract sensor data------------------------------------------------------
    
    EEG_t = EEG_t(:,1);
    
    EEG = single(cast(EEG_t,'uint16'))*Parameters.Coef;
    EEG = EEG - repmat(median(EEG),size(EEG,1),1);
    
    plott(EEG, 600)
    
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
    
    %% save sensor files and EEG file separately
    
    SensorMat = [Accel_r, Magn, Gyro_r, T, P]; % 75 Hz
    EEG = EEG; % 600 Hz
    
    sensorfilename = [bird,'_sensormatraw75hz.mat'];
    eegfilename = [bird,'_eegmatraw600hz.mat'];
    save(sensorfilename, 'SensorMat', '-v7.3')
    save(eegfilename,'EEG','-v7.3')
    
    writematrix(EEG, ['/Volumes/GoogleDrive/My Drive/Thorne Lab/THORNE_LAB/Data!/Conners_Bird_Island/2019_2020/Tag_Data/L0_raw-data/Neurologgers/L0_1_Raw_Decompressed_txt/2_EEG/',FileName, '_EEG.txt'], 'delimiter',',')
    writematrix(SensorMat,['/Volumes/GoogleDrive/My Drive/Thorne Lab/THORNE_LAB/Data!/Conners_Bird_Island/2019_2020/Tag_Data/L0_raw-data/Neurologgers/L0_1_Raw_Decompressed_txt/1_SensorData/',FileName, '_SensorData.txt'], 'delimiter',',')
    
    
    clearvars -except OutputFolder fileList i CurrentPath
end

%%
