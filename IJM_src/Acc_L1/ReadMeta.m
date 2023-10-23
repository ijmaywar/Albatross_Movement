%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This Code reads the meta structures created when running the L0->L1 code.
% It finds out why some files weren't able to be processed and if there are
% any discrepancies between the files processed and progress indicated on
% Full_metadata
%
% IJM
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars
 
%% USER INPUTED VALUES

szn = '2021_2022';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
computer = 'MacMini'; % Options: "MacMini," "MacBookPro"
datalvl = "L1";
tagtype = "AGM";
datatype = "Accelerometer";

%% Set environment

% set directories
GD_dir = findGD(computer);
L1_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Acc/Acc_Technosmart/',szn,'/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
% fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location) & strcmp(fullmeta.Aux_TagType,tagtype),:);

%% How far did I get?

cd(L1_dir)
L1_fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

metastruct_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Acc/Acc_Technosmart/',szn,'/meta_structures/');
cd(metastruct_dir)
metastruct_fileList = exFAT_aux_remove(struct2table(dir('*.mat')));

%% Create table
nBirds = height(fullmeta);
meta_table = table(cell(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1), ...
    'VariableNames',{'Deployment_ID','NumFiles','Step','HasNaN','findMeta_idx','L1_file'}); 

for i = 1:nBirds
    current_bird = fullmeta.Deployment_ID{i};
    meta_table.Deployment_ID{i} = current_bird;

    % Check to see if there is an L1 file for this bird
    L1_name = strcat(current_bird,"_Acc_L1.csv");
    findL1 = find(strcmp(L1_fileList.name,L1_name));

    if ~isempty(findL1)
        meta_table.L1_file(i) = length(findL1);
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

%% Look for discrepencies between fullmeta, metastruct, and the presence of an L1 file
for i = 1:height(meta_table)
    if meta_table.L1_file(i) > 0 && (meta_table.Step(i) < 4 || fullmeta.Acc_L1(i) ~= 1) 
        disp(strcat("Why is there an L1 file for ", meta_table.Deployment_ID(i), "?"))
        pause;
    end

    if meta_table.L1_file(i) == 0 && (meta_table.Step(i) == 4 || fullmeta.Acc_L1(i) == 1) 
        disp(strcat("There is no L1 file for ", meta_table.Deployment_ID(i), "."))
        pause;
    end

    if meta_table.Step(i) == 4 && fullmeta.Acc_L1(i) ~= 1
        disp(strcat("Why isn't ", meta_table.Deployment_ID(i), " marked as preprocessed."))
        pause;
    end

    if fullmeta.Acc_L1 + fullmeta.Acc_Fix + fullmeta.Acc_Skip ~= 1
        disp(stract("L1 should be marked as either complete, 'Fix', or 'Skip' for ", meta_table.Deployment_ID(i)))
        pause;
    end

end
