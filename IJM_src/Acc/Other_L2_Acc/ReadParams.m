
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read .mat parameter files and consolidate into a single table.
% Ian Maywar
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% clearvars
clearvars

%% USER INPUTED VALUES

szn = '2021_2022';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
computer = 'MacMini'; % Options: "MacMini," "MacBookPro"
AccType = 'Technosmart';

%% Set envrionment

% set directories
GD_dir = findGD(computer);
param_dir = strcat(GD_dir,'L2/',location,'/Tag_Data/Acc/',szn,'/Parameters/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% Create L2 file list
cd(param_dir)
param_fileList = struct2table(dir('*.mat'));

% Make sure that every L1 file can be found in Full_metadata.xlsx
disp(CheckMeta(fullmeta,param_fileList,3,"Deployment_ID"))

%% Create table
nBirds = height(param_fileList);
meta_table = table(cell(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1),zeros(nBirds,1), ...
    'VariableNames',{'Deployment_ID','th','m','minPk','num_flaps','data_duration_hrs','fph'}); 

for i = 1:height(param_fileList)
    load(param_fileList.name{i})
    meta_table.Deployment_ID{i} = meta.bird;
    meta_table.th(i) = meta.th;
    meta_table.m(i) = meta.m;
    meta_table.minPk(i) = meta.minPk;
    meta_table.num_flaps(i) = meta.num_flaps;
    meta_table.data_duration_hrs(i) = meta.data_duration_hrs;
    meta_table.fph(i) = meta.fph;
end




