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
GPS_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/Pos/Catlog/";
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

%% Set up meta table
meta_tbl = table(cell(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1),zeros(nfiles,1), ...
    'VariableNames', {'dep_ID','i','tot_restarts','difference_hrs','mins_per_restart','min_diff_GPS_mins','max_diff_GPS_mins'});  

%% Loop thru

for i = 15:3:27%height(ECG_fileList)
    
    %% Make sure all files are present
    cd(ECG_dir)
    namesplit = strsplit(ECG_fileList(i).name,'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    birdmeta = fullmeta(strcmp(string(fullmeta.Deployment_ID), current_bird),:);

    meta_tbl(i,:).dep_ID = {current_bird};
    meta_tbl(i,:).i = i;  

    % GPS
    birdGPSname = strcat(current_bird,"_Catlog_L0.csv");
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));
    if isempty(findGPS)
        disp(strcat("There is no GPS file for ", current_bird))
        continue
    elseif length(findGPS)>1
        disp(strcat("There are multiple GPS files for ", current_bird))
        return
    end

    % HeaderPos
    birdHPosname = strcat(current_bird,"_HRL_L0_1_3_HeaderPos.txt");
    findHPos = find(strcmp(string(HPos_fileList.name),birdHPosname));

    if isempty(findHPos)
        disp(strcat("There is no HPos file for ", current_bird))
        return
    elseif length(findHPos)>1
        disp(strcat("There are multiple HPos files for ", current_bird))
        return
    end

    % inf
    birdinfname = strcat(current_bird,"_HRL_L0.inf");
    findinf = find(strcmp(string(inf_fileList.name),birdinfname));

    if isempty(findinf)
        disp(strcat("There is no inf file for ", current_bird))
    elseif length(findinf)>1
        disp(strcat("There are multiple inf files for ", current_bird))
        return
    end  
    
    %% Load ECG data
    ECG_data = readtable(ECG_fileList(i).name);
    
    %% Load GPS file
    GPS_data = readtable(strcat(GPS_dir,GPS_fileList.name(findGPS)));
    if strcmp(GPS_data.Properties.VariableNames{2}, 'Var2')
        disp("Can't load GPS data correctly.")
        continue
    end

    %% Load HeaderPos file
    HPos_data = readtable(strcat(HPos_dir,HPos_fileList.name(findHPos)));

    %% Load inf file

    if ~isempty(findinf)
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
    
    else
        tot_restarts = 99999999;

    end

    meta_tbl(i,:).tot_restarts = tot_restarts;
    
    %% Find ECG on datetime from fullmeta
    
    meta_startdatetime = strcat(string(birdmeta.AuxON_date_yyyymmdd), " ", string(birdmeta.AuxON_time_hhmmss));
    meta_startdatetime = datetime(meta_startdatetime, 'InputFormat','yyyyMMdd HHmmss');
    
    %% Find ECG duration 

    ECG_SR = 74.9850; % ECG sampling rate is 75 hz
    ECG_dur_hrs = height(ECG_data) / (ECG_SR*60*60);

    final_ECG_dt = meta_startdatetime + hours(ECG_dur_hrs);

    %% Find GPS duration and min/max diff
    
    GPS_DateTime = strcat(string(GPS_data.Date), " ", string(GPS_data.Time));
    GPS_DateTime = datetime(GPS_DateTime,'InputFormat','dd-MMM-yyyy HH:mm:ss');

    % GPS_ON_idx = find(GPS_DateTime >= meta_startdatetime,1);
    % GPS_DateTime = GPS_DateTime(GPS_ON_idx:length(GPS_DateTime));
    
    meta_tbl(i,:).min_diff_GPS_mins = minutes(min(diff(GPS_DateTime)));
    meta_tbl(i,:).max_diff_GPS_mins = minutes(max(diff(GPS_DateTime)));

    % GPS_dur_hrs = hours(GPS_DateTime(end) - GPS_DateTime(1));

    %% Find time elapsed per break

    % difference_hrs = hours(final_ECG_dt - GPS_DateTime(end))
    difference_hrs = hours(final_ECG_dt - temp)
    mins_per_restart = (difference_hrs*60) / tot_restarts

    meta_tbl(i,:).difference_hrs = difference_hrs;
    meta_tbl(i,:).mins_per_restart = mins_per_restart;

    %% GPS Figures

    GPS_Fig = figure;
    plot(minutes(diff(GPS_DateTime)))

    %% Save meta tbl
    writetable(meta_tbl,strcat("/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2019_2020/Aux/HRL/",'ECG_timings_meta.csv')) %write m data
    
    %% Clear variables for next loop
    clearvars -except ECG_dir ECG_fileList fullmeta GD_dir GPS_dir GPS_fileList HPos_dir HPos_fileList inf_dir inf_fileList meta_tbl nfiles i

end
