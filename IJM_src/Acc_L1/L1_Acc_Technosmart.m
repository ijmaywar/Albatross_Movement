%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Peform all L1 steps (s1-s4) for Technosmart Acc tags
%
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2022_2023';
location = "Midway"; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "Axy5"; % Options: 'AGM', 'Axy5', 'AxyAir', 'GCDC'

%% Timezone
if strcmp(location,"Midway")
    if strcmp(szn,"2018_2019")
        written_local = false;
    elseif strcmp(szn,"2022_2023") && strcmp(tagtype,"Axy5")
        written_local = false;
    else
        written_local = true;
    end
elseif strcmp(location,"Bird_Island")
    written_local = true;
else
    disp("Location not found.")
    return
end

% Some Midway files are written in GMT rather than local time.
% This is the case for .csv files sent to us from 2018_2019 Midway. 
% The files I converted from that szn were written in local time.
% 2022_2023 Midway Axy5 tags are also in GMT. 

%% Set envrionment

% turn off annoying warning everytime I import a table
warning('off','MATLAB:table:ModifiedAndSavedVarNames')

% set directories
GD_dir = '/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/';
l0_dir = strcat(GD_dir,'L0/',location,'/Tag_Data/',szn,'/',tagtype,'/');
L1_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'/');
GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'Sheet',location,'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% GPS L1_2_buffer2km file List
cd(GPS_dir)
GPS_fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

% Acc L0 file LIst
cd(l0_dir)
l0_fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

% Prevent figures from popping up when running in the background
set(0,'DefaultFigureVisible','off')

%% Initial checks

% Make sure that all birds have metadata and GPS data
for i = 1:height(l0_fileList)

    if height(l0_fileList) == 1
        namesplit = strsplit(l0_fileList.name,'_');
    else
        namesplit = strsplit(l0_fileList.name{i},'_');
    end
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

if length(uniquebirds) ~= height(l0_fileList)
    disp("There is a bird with multiple files.")
    return
end

disp('Metadata and GPS data found for each bird')
start_i=1;

%% Loop through each unique bird
loop_Start = tic;
parfor(i = start_i:height(l0_fileList))
%for i = 1:height(l0_fileList)
    %% Load data
    
    meta = struct;
    
    if height(l0_fileList) == 1
        namesplit = strsplit(l0_fileList.name,'_');
    else
        namesplit = strsplit(l0_fileList.name{i},'_');
    end
    current_bird = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    dep_ID = current_bird;
    meta.bird = dep_ID;
    meta.step = 0;
    parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
    
    % Load L0 Acc data
    cd(l0_dir)
    if height(l0_fileList) == 1
        m = readtable(l0_fileList.name,'Delimiter',',','ReadVariableNames',true,'TreatAsEmpty',{'NA'});
    else
        m = readtable(l0_fileList.name{i},'Delimiter',',','ReadVariableNames',true,'TreatAsEmpty',{'NA'});
    end 
    
    % Load GPS data
    cd(GPS_dir)
    birdGPSname = strcat(current_bird,'_GPS_L1_2.csv'); % This will be changed to not include the explainer at the end.
    findGPS = find(strcmp(string(GPS_fileList.name),birdGPSname));
    GPSdata = readtable(char(GPS_fileList.name(findGPS)),'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'});  

    % Load metadata
    findmeta = find(strcmp(fullmeta.Deployment_ID,current_bird));
    birdmeta = fullmeta(findmeta,:);

    %% s1
    
    [m,s1_meta] = s1_Acc(m,dep_ID,birdmeta,written_local);

    meta.s1 = s1_meta;

    if s1_meta.skip == 1
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): skipped at s1.'))
        meta.step = 1;
        parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): s1 complete.'))

    %% s2

    [m,s2_meta] = s2_Acc(m,dep_ID,fullmeta);

    meta.s2 = s2_meta;
  %%
    if s2_meta.skip == 1
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): skipped at s2.'))
        meta.step = 2;
        parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): s2 complete.'))

    %% s3

    [m,s3_timetbl,s3_meta] = s3_Acc(m,GPSdata,dep_ID);

    meta.s3 = s3_meta;
    meta.timetbl = s3_timetbl;
  %%  
    if ~isempty(find(s3_meta.skip == 1, 1))
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): skipped at s3.'))
        meta.step = 3;
        parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));
        continue
    end

    %  Check to make sure the data is continuous
    if ~CheckContinuous(m,25)
        disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): data is not continuous after s3.'))
        continue
    end

    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): s3 complete.'))
        
    %% s4

    [m,Q,HasNaN] = s4_Acc(m,L1_dir,dep_ID);

    meta.HasNaN = HasNaN;
    meta.Q = Q;
    meta.step = 4;
    parsave(meta,strcat(L1_dir,'meta_structures/',dep_ID,'_meta.mat'));

    writetable(m,strcat(L1_dir,dep_ID,'_Acc_L1.csv')) %write m data
    
    disp(strcat(dep_ID,'(',num2str(i),'/',num2str(height(l0_fileList)), '): s4 complete.'))

    m = []; % to free up memory

end
loop_End = toc(loop_Start);
disp('All files processed.')
disp(strcat('time to complete loop:', string(loop_End)))
