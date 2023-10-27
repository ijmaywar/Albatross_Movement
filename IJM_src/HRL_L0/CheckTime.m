%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Use this code to explore the durations for which the HRL and GPS tags
% were recording data
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%%
GD_dir = findGD("MacMini");
ECG_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/Aux/HRL/L0_1_Decompressed/1_SensorData/";
GPS_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/GPS/GPS_Catlog/2019_2020/1_onbird/";
HPos_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/Aux/HRL/L0_1_Decompressed/3_HeaderPos/";
inf_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/Aux/HRL/L_meta_setup_Inf_files/";

cd(ECG_dir)
ECG_fileList = dir("*.txt");

cd(GPS_dir)
GPS_fileList = dir("*.csv");
GPS_fileList = struct2table(GPS_fileList);

cd(HPos_dir)
HPos_fileList = dir("*.txt");
HPos_fileList = struct2table(HPos_fileList);

cd(inf_dir)
inf_fileList = dir("*.inf");
inf_fileList = struct2table(inf_fileList);

fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});

nfiles = height(ECG_fileList);
% meta table
meta_tbl = table(cell(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1), ...
    'VariableNames', {'dep_ID','i','tot_restarts','difference_hrs','mins_per_restart','min_diff_GPS_mins','max_diff_GPS_mins'});  

%% Loop thru

for i = 2:4 %height(ECG_fileList)
    
    %% Load ECG data
    cd(ECG_dir)
    namesplit = strsplit(ECG_fileList(i).name,'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    birdmeta = fullmeta(strcmp(string(fullmeta.Deployment_ID), current_bird),:);

    ECG_data = readtable(ECG_fileList(i).name);

    meta_tbl(i,:).dep_ID = {current_bird};
    meta_tbl(i,:).i = i;
    
    %% Locate and load GPS file
    birdGPSname = strcat(current_bird,"_Catlog_L0.csv");
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));

    if isempty(findGPS)
        disp(strcat("There is no GPS file for ", current_bird))
        return
    elseif length(findGPS)>1
        disp(strcat("There are multiple GPS files for ", current_bird))
        return
    else
        GPS_data = readtable(strcat(GPS_dir,GPS_fileList.name(findGPS)));
    end

    %% Locate and load HeaderPos file
    birdHPosname = strcat(current_bird,"_HRL_L0_1_3_HeaderPos.txt");
    findHPos = find(strcmp(string(HPos_fileList.name),birdHPosname));

    if isempty(findHPos)
        disp(strcat("There is no HPos file for ", current_bird))
        return
    elseif length(findHPos)>1
        disp(strcat("There are multiple HPos files for ", current_bird))
        return
    else
        HPos_data = readtable(strcat(HPos_dir,HPos_fileList.name(findHPos)));
    end

    %% Locate and load inf file
    birdinfname = strcat(current_bird,"_HRL_L0.inf");
    findinf = find(strcmp(string(inf_fileList.name),birdinfname));

    if isempty(findinf)
        disp(strcat("There is no inf file for ", current_bird))
        return
    elseif length(findinf)>1
        disp(strcat("There are multiple inf files for ", current_bird))
        return
    else
        FName = strcat(inf_dir,inf_fileList.name(findinf));
        PatternLength = 9; %bytes
        fid = fopen(FName, 'r');
        A = fread(fid,[PatternLength inf],'uint8=>uint8');
        fclose(fid);
            
        %Structure of 9-byte record (firmware >= 7.02)
        %Byte 1: RefOverSHT
        %Byte 2: Channels
        %Byte 3: FileNum
        %Byte 4: aclkDiv_H
        %Byte 5: aclkDiv_L
        %Byte 6: SerNum_H
        %Byte 7: SerNum_L
        %Byte 8: StDelay_H
        %Byte 9: StDelay_L
        
        % NSectors = size(A,2);
        % 
        % f1 = figure(1); clf;
        % plot(A(3,:));   %Session number
        % box off
        % xlabel('Sector number');
        % ylabel('Cumulative restarts');
        % xlim([0 NSectors]);

        tot_restarts = double(A(3,end));

        % %%
        % V = [1 2 4:9];
        % A1 = A-repmat(A(:,1),1,NSectors); 
        % f2 = figure(2); clf; %other parameters
        % plot(A1(V,:)');   %Session number
        % box off
        % xlabel('Sector number');
        % ylabel('Parameters');
        % xlim([0 NSectors]);
        % 
        % for I_t = 1:9
        %   disp([num2str(I_t) ' ' dec2bin(A(I_t,1),8)]);  
        % end    
    end

    meta_tbl(i,:).tot_restarts = tot_restarts;

    %% Find ECG on datetime from fullmeta
    
    meta_startdate = num2str(birdmeta.AuxON_date_yyyymmdd);
    meta_startdate = strcat(extractBefore(meta_startdate,5),"-",extractBetween(meta_startdate,5,6),"-",extractAfter(meta_startdate,6));
    meta_starttime = birdmeta.AuxON_time_hhmmss;
    if isa(meta_starttime,'double')
        meta_starttime = sprintf('%06d',meta_starttime);
    end
    meta_starttime = strcat(extractBefore(meta_starttime,3),":",extractBetween(meta_starttime,3,4),":",extractAfter(meta_starttime,4));
    meta_startdatetime = strcat(meta_startdate," ",meta_starttime);
    meta_startdatetime = datetime(meta_startdatetime,'InputFormat','yyyy-MM-dd HH:mm:ss');
    
    %% Find ECG duration 

    ECG_SR = 75; % ECG sampling rate is 75 hz
    ECG_dur_hrs = height(ECG_data) / (ECG_SR*60*60);

    %% Find GPS duration and min/max diff
    
    GPS_DateTime = strcat(string(GPS_data.Date), " ", string(GPS_data.Time));
    GPS_DateTime = datetime(GPS_DateTime,'InputFormat','dd-MMM-yyyy HH:mm:ss');
    
    meta_tbl(i,:).min_diff_GPS_mins = minutes(min(diff(GPS_DateTime)));
    meta_tbl(i,:).max_diff_GPS_mins = minutes(max(diff(GPS_DateTime)));

    GPS_dur_hrs = hours(GPS_DateTime(end) - GPS_DateTime(1));

    %% Find time elapsed per break

    difference_hrs = (GPS_dur_hrs - ECG_dur_hrs)
    mins_per_restart = (GPS_dur_hrs - ECG_dur_hrs) / tot_restarts * 60

    meta_tbl(i,:).difference_hrs = difference_hrs;
    meta_tbl(i,:).mins_per_restart = mins_per_restart;

    %% GPS Figures

    GPS_Fig = figure;
    plot(minutes(diff(GPS_DateTime)))

    %% Clear variables for next loop

    clearvars -except ECG_dir ECG_fileList fullmeta GD_dir GPS_dir GPS_fileList HPos_dir HPos_fileList inf_dir inf_fileList meta_tbl nfiles i

end
