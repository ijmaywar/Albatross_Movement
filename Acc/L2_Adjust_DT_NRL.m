
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% L2 Acc data DateTime adjustment based on datetime_breaks
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% clearvars
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway'

%% Set envrionment

% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
L2_dir = strcat(GD_dir,'L2/',location,'/Tag_Data/Acc/',szn,'/NRL_Not_Adjusted/');
break_dir = strcat(GD_dir,'L0/',location,'/Tag_Data/',szn,'/Aux/NRL/L0_1_Decompressed/datetime_breaks/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_Season,szn) & strcmp(fullmeta.Location,location),:);

% Create L2 file list
cd(L2_dir)
L2_fileList = dir('*.csv');
L2_fileList(startsWith({L2_fileList.name},'._')) = [];
L2_fileNames = string({L2_fileList.name});
L2_fileList = struct2table(L2_fileList);

%% Loop thru birds 

for i = 1:height(L2_fileList)
    %%
    clear corrected_DT

    namesplit = strsplit(L2_fileNames(i),'_');
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});
    break_tbl = readtable(strcat(break_dir,dep_ID,'_dt_breaks.csv'));
    m = readtable(L2_fileNames(i));
    if height(break_tbl) ~= 0
        cumulative_correction = [0;cumsum(break_tbl.break_length_mins)];
        for acc_idx = 1:height(m)
            idx_fit = find(break_tbl.break_start >= m.DateTime(acc_idx), 1, 'first');
            if isempty(idx_fit)
                % In this case the idx is found after all breaks
                idx_fit = height(break_tbl)+1;
            end
            corrected_DT(acc_idx) = m.DateTime(acc_idx) + minutes(cumulative_correction(idx_fit));
        end
    else
        corrected_DT = m.DateTime';
    end
    
    % Change m and save m
    m.DateTime_PreCorrection = m.DateTime;
    m.DateTime = corrected_DT';
    m.DateTime_PreCorrection.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';
    m.DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';
    m.DateTime_PreCorrection = string(m.DateTime_PreCorrection);
    m.DateTime = string(m.DateTime);

    writetable(m,strcat(GD_dir,'L2/',location,'/Tag_Data/Acc/',szn,'/',dep_ID,'_Acc_L2.csv')); 
end







