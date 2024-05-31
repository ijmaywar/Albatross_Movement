%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Peform all L1 steps (s1-s4) for Technosmart Acc tags
%
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway'
tagtype = "AxyTrek"; % Options: 'AGM', 'Axy5', 'AxyAir', 'AxyTrek'

%% Timezone
if strcmp(location,"Midway") && (strcmp(szn,"2018_2019") || (strcmp(szn,"2022_2023") && strcmp(tagtype,"Axy5")))
    written_local = false;
else
    written_local = true;
end

% Some Midway files are written in GMT rather than local time.
% This is the case for .csv files sent to us from 2018_2019 Midway. 
% The files I converted from that szn were written in local time:
% (BFAL_20190130_1, LAAL_20190203_345, LAAL_20190203_R142)
% 2022_2023 Midway Axy5 tags are also in GMT.

%% Set envrionment

% turn off annoying warning everytime I import a table
warning('off','MATLAB:table:ModifiedAndSavedVarNames')

% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";

if strcmp(tagtype,"AxyTrek")
    L0_dir = strcat(GD_dir,'L0/',location,'/Tag_Data/',szn,'/AxyTrek/');
    GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_AxyTrek/',szn,'/2_buffer2km/');
    L1_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Acc/Acc_Technosmart/',szn,'/AxyTrek/');
else
    L0_dir = strcat(GD_dir,'L0/',location,'/Tag_Data/',szn,'/Aux/',tagtype,'/');
    GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');
    L1_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Acc/Acc_Technosmart/',szn,'/');
end



% Sometimes a select few individuals have been written in a different
% timezone than the rest of the files in a given field szn. This is the
% case for three files in Midway 2018_2019 AxyAir which were written in local
% time, not GMT.
L0_split = strsplit(L0_dir,"_");
if strcmp(L0_split(end),"local/")
    written_local = true;
elseif strcmp(L0_split(end),"GMT/")
    written_local = false;
end

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_Season,szn) & strcmp(fullmeta.Location,location),:);

% GPS L1_2_buffer2km file List
cd(GPS_dir)
GPS_fileList = dir('*.csv');
GPS_fileList(startsWith({GPS_fileList.name},'._')) = [];
GPS_fileNames = string({GPS_fileList.name});
GPS_fileList = struct2table(GPS_fileList);

% Acc L0 file LIst
cd(L0_dir)
L0_fileList = dir('*.csv');
L0_fileList(startsWith({L0_fileList.name},'._')) = [];
L0_fileNames = string({L0_fileList.name});
L0_fileList = struct2table(L0_fileList);

% Prevent figures from popping up when running in the background
set(0,'DefaultFigureVisible','off')

% suppress annoying warnings when reading Acc_L0 files
warning('off','MATLAB:table:ModifiedAndSavedVarnames')

%% Initial checks

CheckMetaGPSUnique(L0_fileList,GPS_fileList,fullmeta)

%% Loop through each unique bird
loop_Start = tic;
% parfor(i = 1:height(L0_fileList))
for i = 1:height(L0_fileList)
    %% Load data
    
    meta = struct;
    
    if height(L0_fileList) == 1
        namesplit = strsplit(L0_fileList.name,'_');
    else
        namesplit = strsplit(L0_fileList.name{i},'_');
    end
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    meta.bird = dep_ID;
    meta.step = 0;
    parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
    
    % Load L0 Acc data
    cd(L0_dir)
    if height(L0_fileList) == 1
        m = readtable(L0_fileList.name,'Delimiter',',','ReadVariableNames',true,'TreatAsEmpty',{'NA'});
    else
        m = readtable(L0_fileList.name{i},'Delimiter',',','ReadVariableNames',true,'TreatAsEmpty',{'NA'});
    end 
    
    % Load GPS data
    cd(GPS_dir)
    birdGPSname = strcat(dep_ID,'_GPS_L1_2.csv'); % This will be changed to not include the explainer at the end.
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));
    GPSdata = readtable(char(GPS_fileList.name(findGPS)),'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'});  

    % Load metadata
    findmeta = find(strcmp(fullmeta.Deployment_ID,dep_ID));
    birdmeta = fullmeta(findmeta,:);

    %% s1
    
    [m,s1_meta] = s1_Acc(m,dep_ID,birdmeta,written_local);

    % Write a seperate s1 for AxyTrek...

    meta.s1 = s1_meta;

    if s1_meta.skip == 1
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): skipped at s1.'))
        meta.step = 1;
        parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): s1 complete.'))

    %% s2

    [m,s2_meta] = s2_Acc(m,dep_ID,fullmeta);

    meta.s2 = s2_meta;
  
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

    % writetable(m,strcat(L1_dir,dep_ID,'_Acc_L1.csv')) %write m data
    writetable(m,strcat(L1_dir,'AxyTrek',dep_ID,'_Acc_L1.csv')) %write m data
    
    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(L0_fileList)), '): s4 complete.'))

    m = []; % to free up memory

end
loop_End = toc(loop_Start);
disp('All files processed.')
disp(strcat('time to complete loop:', string(loop_End)))
