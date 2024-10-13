%% USER INPUTED VALUES

szn = '2019_2020';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway'
AccType = 'Technosmart'; % Options: 'Technosmart', 'NRL'

%%
% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
L1_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Acc/Acc_',AccType,'/',szn,'/');
L2_dir = strcat(GD_dir,'L2/',location,'/Tag_Data/Acc/',szn,'/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_Season,szn) & strcmp(fullmeta.Location,location),:);

% Create L1 file list
cd(L1_dir)
L1_fileList = dir('*.csv');
L1_fileList(startsWith({L1_fileList.name},'._')) = [];
L1_fileList = struct2table(L1_fileList);

% Make sure that every L1 file can be found in Full_metadata.xlsx
disp(CheckMeta(fullmeta,L1_fileList,3,"Deployment_ID"))

% Get monitor dimensions to display figures
p = get(0, "MonitorPositions");

% Create Parameters folder
mkdir(strcat(L2_dir,"Parameters/"))

%% read file
j = 27;
bird = strsplit(L1_fileList.name{j},"_");
bird = strcat(bird{1},"_",bird{2},"_",bird{3});
Acc = readtable(L1_fileList.name{j},'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'}); % read bird i's file
Acc.DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';

%% Enter datetimes (copy and paste this from R code) and duration
DS_dt = "2020-01-16 19:04:02";
WSS_dt = "2020-01-17 05:04:02";
dur = minutes(2);

% filter Acc data for DS and WSS
DS_acc = Acc(Acc.DateTime >= DS_dt & Acc.DateTime <= DS_dt+dur);
WSS_acc = Acc(Acc.DateTime >= WSS_dt & Acc.DateTime <= WSS_dt+dur);

%% Plot soaring behaviors

figure
subplot(2,1,1)
plot(DS_acc.DateTime,DS_acc.___)
hold on
plot(DS_acc.DateTime,DS_acc.___)
hold on 
plot(DS_acc.DateTime,DS_acc.___)

subplot(2,1,2)
plot(WSS_acc.DateTime,WSS_acc.___)
hold on
plot(WSS_acc.DateTime,WSS_acc.___)
hold on 
plot(WSS_acc.DateTime,WSS_acc.___)


