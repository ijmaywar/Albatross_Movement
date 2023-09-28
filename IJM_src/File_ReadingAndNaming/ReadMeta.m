%% Clear variables
clearvars
 
%% USER INPUTED VALUES

szn = '2019_2020';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "AGM"; % Options: 'AGM', 'Axy5'
computer = 'MBP'; % Options:'MBP', 'ThinkPad'

%% Set environment

% Matlab functions toolbox
addpath(genpath('/Volumes/LaCie/Functions_Toolboxes/Matlab/'))
addpath(genpath('/Users/ian/Documents/ThorneLab_FlightDynamics/IJM_src/'))

% Full_metadata sheet
fullmeta = readtable('/Volumes/LaCie/Full_metadata.xlsx','Sheet',location,'TreatAsEmpty',{'NA'});
% Specify the field season, location, and Acc tag type
% fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location) & strcmp(fullmeta.ACC_TagType,tagtype),:);
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

%% How far did I get?

s4_dir = strcat('/Volumes/LaCie/L1/',location,'/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'/s4_Trimmed_Untilted/');
cd(s4_dir)
s4_fileList = exFAT_aux_remove(struct2table(dir('*.txt')));

metastruct_dir = strcat('/Volumes/LaCie/L1/',location,'/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'/s4_Trimmed_Untilted/meta_structures/');
cd(metastruct_dir)
metastruct_fileList = exFAT_aux_remove(struct2table(dir('*.mat')));

%% Create table
nBirds = height(fullmeta);
meta_table = table(cell(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1), ...
    'VariableNames',{'Deployment_ID','NumFiles','Step','HasNaN','findMeta_idx','s4_file'}); 

for i = 1:nBirds
    current_bird = fullmeta.Deployment_ID{i};
    meta_table.Deployment_ID{i} = current_bird;

    % Check to see if there is an s4 file for this bird
    s4_name = strcat(current_bird,"_Acc_L1_s4.txt");
    finds4 = find(strcmp(s4_fileList.name,s4_name));

    if ~isempty(finds4)
        meta_table.s4_file(i) = length(finds4);
    end

    % Check the metastruct to see how far we are into preprocessing
    metastruct_name = strcat(current_bird,'_meta.mat');
    findmeta = find(strcmp(metastruct_fileList.name,metastruct_name));

    if ~isempty(findmeta)
        meta_table.NumFiles(i) = length(findmeta);
        if length(findmeta)==1
            meta_table.findMeta_idx(i) = findmeta;
            load(metastruct_fileList.name{findmeta})
            
            % Write information in table
            meta_table.Deployment_ID{i} = current_bird;
            meta_table.Step(i) = meta.step;
            if meta.step==4
                meta_table.HasNaN(i) = meta.HasNaN;
            end
        end
    end
end

%% List files that have NaN
NaN_only = meta_table(meta_table.HasNaN~=0,:);

%% Find out why files weren't fully preprocessed
for i = 1:height(meta_table)
    if meta_table.NumFiles(i)>0 && meta_table.Step(i)<4
        load(metastruct_fileList.name{meta_table.findMeta_idx(i)})
        meta
        if meta.step == 1
            meta.s1
        elseif meta.step == 2
            meta.s2
        elseif meta.step == 3
            meta.s3
        end
        pause;
    end
end

%% Look for discrepencies between fullmeta, metastruct, and the presence of an s4 file
for i = 1:height(meta_table)
    if meta_table.s4_file(i) > 0 && (meta_table.Step(i) < 4 || fullmeta.Acc_L1_s4(i) ~= 1) 
        disp(strcat("Why is there an s4 file for ", meta_table.Deployment_ID(i), "?"))
        pause;
    end

    if meta_table.s4_file(i) == 0 && (meta_table.Step(i) == 4 || fullmeta.Acc_L1_s4(i) == 1) 
        disp(strcat("There is no s4 file for ", meta_table.Deployment_ID(i), "."))
        pause;
    end

    if meta_table.Step(i) == 4 && fullmeta.Acc_L1_s4(i) ~= 1
        disp(strcat("Why isn't ", meta_table.Deployment_ID(i), " marked as preprocessed."))
        pause;
    end

end
