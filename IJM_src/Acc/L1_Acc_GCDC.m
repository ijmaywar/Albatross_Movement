%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Peform all L1 steps starting from data in s1_Uniformat (s2-s4)
% I'm doing this for GCDC tags because the process of getting GCDC from
% L0->L1 is exremely finnicky and convoluted.
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2018_2019';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "GCDC"; % Options: 'AGM', 'Axy5', 'AxyAir', 'GCDC'
computer = 'MBP'; % Options: 'MBP', 'ThinkPad'

%% Set envrionment

% Computer
if strcmp(computer,'MBP')
    comp_dir = '/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/';
    l0_dir = strcat('/Volumes/LaCie/L0/Bird_Island/Tag_Data/',szn,'/',tagtype,'/');
    s1_dir = strcat('/Volumes/LaCie/L1/Bird_Island/Tag_Data/Accelerometer/Acc_GCDC/',szn,'/s1_Uniformat/');
    s4_dir = strcat('/Volumes/LaCie/L1/Bird_Island/Tag_Data/Accelerometer/Acc_GCDC/',szn,'/s4_Trimmed_Untilted/');
    GPS_dir = strcat('/Volumes/LaCie/L1/Bird_Island/Tag_Data/GPS/GPS_iGotU/',szn,'/GPS_L1_2_buffer2km/');

elseif strcmp(computer,'ThinkPad')
    % NOT SURE WHAT THIS IS - fill in when using ThinkPad%
    % comp_dir='/Volumes/GoogleDrive/';
    l0_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2021_2022/Acc_',tagtype,'/csv/');
    s1_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s1_Uniformat/');
    s2_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s2_AnalysisReady/');
    s3_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s3_Trimmed/');
    s4_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s4_Trimme_Untilted/');
    GPS_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/GPS/GPS_Catlog/',szn,'/GPS_L1_2_buffer2km/');

else
    disp('computer not found');
    return
end

% Matlab functions toolbox
addpath(genpath('/Volumes/LaCie/Functions_Toolboxes/Matlab/'))
addpath(genpath('/Users/ian/Documents/ThorneLab_FlightDynamics/IJM_src/'))

% Full_metadata sheet
fullmeta = readtable('/Volumes/LaCie/Full_metadata.xlsx','Sheet',location,'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% GPS file List
cd(GPS_dir)
GPS_fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

% Set directory to tag type q
cd(s1_dir)
s1_fileList = exFAT_aux_remove(struct2table(dir('*.txt')));

%% Initial checks

% Make sure that all birds have metadata and GPS data
for i = 1:height(s1_fileList)
    namesplit = strsplit(s1_fileList.name{i},'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    bird_names{i} = current_bird;
    
    % meta
    findmeta = find(strcmp(fullmeta.Deployment_ID,current_bird));
    if isempty(findmeta)
        disp(strcat(current_bird," cannot be found in metadata"))
        return
    elseif length(findmeta)>1
        disp(strcat("There are multiple metadata entries for ",current_bird))
        return
    end

    % GPS
    birdGPSname = strcat(current_bird,"_GPS_L1_2.csv");
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));

    if isempty(findGPS)
        disp(strcat("There is no GPS file for ", current_bird))
        return
    elseif length(findGPS)>1
        disp(strcat("There are multiple GPS files for ", current_bird))
        return
    end
end

% check that there's only one file per bird
uniquebirds = unique(bird_names);

if length(uniquebirds) ~= height(s1_fileList)
    disp("There is a bird with multiple files.")
    return
end

disp('Metadata and GPS data found for each bird')
start_i=1;

%% How far did I get?
cd(strcat(s4_dir,"meta_structures/"))
meta_fileList = exFAT_aux_remove(struct2table(dir('*.mat')));

for i = 1:height(s1_fileList)

    namesplit = strsplit(s1_fileList.name{i},'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    metastructname = strcat(current_bird,'_meta.mat');
    findmetastruct = find(strcmp(string(meta_fileList.name),metastructname));

    if isempty(findmetastruct)
        disp(strcat(current_bird,' does not have a metadata struct file. Begin at start_i'))
        start_i = i;
        return
    elseif length(findmetastruct)>1
        disp(strcat(current_bird, ' has multiple metadata structures. Begin at start_i'))
        start_i = i;
        return
    else
        load(char(meta_fileList.name(findmetastruct)))
        if meta.step == 0
            start_i = i;
            disp("you're not done. Begin at 'start_i'")
            break
        end
    end
end

if i == height(s1_fileList)
    disp("you're done.")
end

%% Loop through each unique bird
loop_Start = tic;
% parfor(i = start_i:height(s1_fileList),4)
for i = 1:1%height(l0_fileList)
    %% Load data (s1 data)
    
    meta = struct;

    namesplit = strsplit(s1_fileList.name{i},'_');
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    dep_ID = current_bird;
    meta.bird = dep_ID;
    meta.step = 1;
    parsave(meta,strcat(s4_dir,'meta_structures/',dep_ID,'_meta.mat'));
    
    % Load L0 Acc data
    cd(s1_dir)
    s1_m = readtable(s1_fileList.name{i},'Delimiter',',','ReadVariableNames',true,'TreatAsEmpty',{'NA'});

    % Load GPS data
    cd(GPS_dir)
    birdGPSname = strcat(current_bird,'_GPS_L1_2.csv'); % This will be changed to not include the explainer at the end.
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));
    GPSdata = readtable(char(GPS_fileList.name(findGPS)),'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'});  

    % Load metadata
    findmeta = find(strcmp(fullmeta.Deployment_ID,current_bird));
    birdmeta = fullmeta(findmeta,:);

    %% s2

    [s2_m,s2_meta] = s2_Acc(s1_m,dep_ID,fullmeta);
    
    s1_m = []; % to free up memory

    % writetable(s2_m,strcat(s2_dir,dep_ID,'_Acc_L1_s2_AnalysisReady.txt'))
    meta.s2 = s2_meta;

    if s2_meta.skip == 1
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(s1_fileList)), '): skipped at s2.'))
        meta.step = 2;
        parsave(meta,strcat(s4_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(s1_fileList)), '): s2 complete.'))

    %% s3

    [s3_m,s3_timetbl,s3_meta] = s3_Acc(s2_m,GPSdata,dep_ID);
    
    s2_m = []; % to free up memory

    % writetable(s3_m,strcat(s3_dir,dep_ID,'_Acc_L1_s3_Trimmed','.txt'));
    meta.s3 = s3_meta;
    meta.timetbl = s3_timetbl;
    
    if ~isempty(find(s3_meta.skip == 1, 1))
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(s1_fileList)), '): skipped at s3.'))
        meta.step = 3;
        parsave(meta,strcat(s4_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    %  Check to make sure the data is continuous
    if ~CheckContinuous(s3_m,25)
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(s1_fileList)), '): data is not continuous after s3.'))
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(s1_fileList)), '): s3 complete.'))
        
    %% s4

    [s4_m,Q,HasNaN] = s4_Acc(s3_m,s4_dir,dep_ID);

    s3_m = []; % to free up memory

    meta.HasNaN = HasNaN;
    meta.Q = Q;
    meta.step = 4;
    parsave(meta,strcat(s4_dir,'meta_structures/',dep_ID,'_meta.mat'));

    writetable(s4_m,strcat(s4_dir,dep_ID,'_Acc_L1_s4.txt')) %write m data
    
    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(s1_fileList)), '): s4 complete.'))

    s4_m = []; % to free up memory
end
loop_End = toc(loop_Start);
disp('All files processed.')
disp(strcat('time to complete loop:', string(loop_End)))


