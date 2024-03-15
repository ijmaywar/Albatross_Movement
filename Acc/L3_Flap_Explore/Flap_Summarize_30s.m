%
%   Summarize flaps on the order of 30 second chunks
%
%% clearvars
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
assume_loctz = true; % Options: true, false. 

%% Set envrionment

L2_dir = strcat('/Volumes/LaCie/L2/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'/AccDataWithFlaps/');
HMM_dir = ['/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/' ...
    'THORNE_LAB/Data/Albatross/Conners_Analysis/HMM/data/w3_MovementMetrics_HMM-State_30s_Merged/2019-2020/'];
old_L2_dir = ['/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/' ...
    'THORNE_LAB/Data/Albatross/Conners_Analysis/HMM/data/s4_Acc_multi-years_BBALGHAL_AnalysisReady'];

% Matlab functions toolbox
addpath(genpath('/Volumes/LaCie/Functions_Toolboxes/Matlab/'))
addpath(genpath('/Users/ian/Documents/ThorneLab_FlightDynamics/IJM_src/'))

% Full_metadata sheet
fullmeta = readtable('/Volumes/LaCie/Full_metadata.xlsx','Sheet',location,'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

cd(L2_dir)
L2_fileList = exFAT_aux_remove(struct2table(dir('*.txt')));

cd(HMM_dir)
HMM_fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

cd(old_L2_dir)
old_L2_fileList = exFAT_aux_remove(struct2table(dir('*.txt')));

disp(CheckMeta(fullmeta,L2_fileList,3,"Deployment_ID"))

% Find local timezone
if strcmp(location,"Midway")
    local_tz = "Pacific/Midway";
elseif strcmp(location,"Bird_Island")
    local_tz = "UTC-2";
else
    disp("Can't find location for timezone.")
    return
end

%% Loop thru birds
parfor j = 6:10%height(L2_fileList)
%for j = 2:2%height(L2_fileList)
    
    %% Read Acc data

    cd(L2_dir)
    birdsplit = strsplit(L2_fileList.name{j},"_");
    bird = strcat(birdsplit{1},"_",birdsplit{2},"_",birdsplit{3});
    birdmeta = fullmeta(fullmeta.Deployment_ID == bird,:);
    
    Acc = readtable(L2_fileList.name{j},'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'});
    Acc.DateTime.TimeZone = "GMT";

    %% Read old Acc data
    
    % % old_L2name = strcat(birdmeta.GPS_OG_ID,"_2019-2020_AnalysisReady_trimmed_untilted.txt");
    % % if isempty(find(strcmp(old_L2name,old_L2_fileList.name), 1))
    % %     disp(strcat("There is no old L2 file for ",bird))
    % % else
    % %     cd(old_L2_dir)
    % %     old_L2 = readtable(old_L2name,'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'});
    % % end
    % % old_L2.DateTime.TimeZone = local_tz;

    %% Read HMM data
    
    HMMname = strcat(birdmeta.GPS_OG_ID,"_30s_feat_wSTATE_2021-09-20.csv");
    if isempty(find(strcmp(HMMname,HMM_fileList.name), 1))
        disp(strcat("There is no HMM file for ",bird))
        continue
    else
        cd(HMM_dir)
        HMM = readtable(HMMname,'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'});
    end
    
    if assume_loctz
        % Assign loctz to datetime
        HMM.mid_window_datetime.TimeZone = local_tz;
        % Convert to GMT
        HMM.mid_window_datetime.TimeZone = "GMT";
    else
        % Assign GMT tz to datetime
        HMM.mid_window_datetime.TimeZone = "GMT";
    end

    %% Create 30s flap summary table
    nchunks = height(HMM);
    Flap_summary = table(zeros(nchunks,1),NaN(nchunks,1),zeros(nchunks,1), ...
        'VariableNames', {'mid_window_datetime','nflaps','state'});

    mid_DTs = HMM.mid_window_datetime;

    chunk_start = mid_DTs - minutes(15);
    chunk_end = mid_DTs + minutes(15);

    flap_data = Acc(Acc.Flap~=0,:);

    for i = 1:nchunks
        Flap_summary.mid_window_datetime = mid_DTs;
        Flap_summary.state = HMM.state;
        
        if chunk_start(i) < Acc.DateTime(1)
            Flap_summary.nflaps(i) = nan;
        elseif chunk_end(i) > Acc.DateTime(height(Acc))
            Flap_summary.nflaps(i) = nan;
        else
            % Summarize flaps
            Flap_summary.nflaps(i) = sum(flap_data.DateTime>=chunk_start(i) & flap_data.DateTime<chunk_end(i));
        end
    end

    %% Save flap summary table
    writetable(Flap_summary,strcat("/Volumes/LaCie/Flap_Summaries/",bird,'.csv')) %write m data

end




