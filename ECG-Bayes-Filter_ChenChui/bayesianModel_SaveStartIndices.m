 clear all;
 % Author : Chen Cui
 % Data : 02/2021
 % This code is used to detect the heart beat
 % IMPORTANT : When running the main function, this function requires 
 % the chronux toolbox to be on the search path.
 % To improve the accuracy of detection, you can choose to INPUT the initialization
 % of the detection.Set the Ini = 1 to input by hand, otherwise set Ini = 0; 
 % Defult Ini = 0, detect the first two peaks auto.IF Ini =1, you need to iput twice.

 %% Set environment
 addpath(genpath("~/Dropbox/Academia/SUNY/Project_Components/BIRD_ISLAND/Analyses/Functions_Toolboxes/Matlab/SensorFunctionToolboxes/"))
 addpath(genpath("~/Dropbox/Academia/SUNY/Project_Components/BIRD_ISLAND/Analyses/Functions_Toolboxes/Matlab/ECG_ChenChui/chronux_2_12/"))
 addpath(genpath("~/Dropbox/Academia/SUNY/Project_Components/BIRD_ISLAND/Analyses/Energetics/ECG_to_HeartBeat/ECG-Bayes-Filter_ChenChui/"))
 datadir = '//Volumes/GoogleDrive/My Drive/Thorne Lab/THORNE_LAB/Data/Conners_Bird_Island/2019_2020/Tag_Data/L0_raw-data/Neurologgers/L0_1_Raw_Decompressed_txt/2_EEG/';
 dropdir = '/Volumes/GoogleDrive/My Drive/Thorne Lab/THORNE_LAB/Data/Conners_Analysis/ECG_HeatRate/meta/'
 cd(datadir)
 files=dir('*.txt');
 
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Routine needed to clear ghost files if working in Seagate Hard Drive
% Ignore except in very specific cases where ghost files are created and
% then read by the function dir.
skipx=[];
for k = 1:length(files)
skipx(k)=~startsWith(files(k).name,'._','IgnoreCase',true);
end

files = files(find(skipx));

%% Birdlist and Initialize DataFrame
birdi={};
for i = 1:length(files)
   tmp = split(files(i).name,"_");
   birdi{i}=strcat(tmp{1},"_",tmp{2});
end
clear i
birdarray = string(birdi);

% Initialize table
df = table(birdarray', zeros(length(files),1));
df.Properties.VariableNames = {'bird','start'};

%% Plot Files and Store Starting Index for Detection
i = i+1 % Manual
m= readtable(strcat(datadir,files(i).name));
plot(m.Var1(1:20000000))
% plot(m.EEG)

[s1,e1] = ginput();

start = round(s1(1)); % start index 
df.start(i) = start;

clearvars -except i files datadir dropdir df

writetable(df, strcat(dropdir, '2019-2020_HRL_start-detection-times.txt'), 'Delimiter',',')
