
% Setup
exist('/Users/eheywood/Dropbox/3_BirdIsland/Analyses/Functions_Toolboxes/Matlab','dir')

% Add path to necessary toolboxes
addpath(genpath("/Users/eheywood/Dropbox/3_BirdIsland/Analyses/Functions_Toolboxes/Matlab"))



% Dir for expanded immersion logger data
immersiondir = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mz4wiprZFgV9h0akSSPfVABY_vpmeDfb/Wandering Albatross Data/Tag_Data/L0_Raw_Data/2018-2019_brood-guard/WA_GLS_Br_2019/DEG_FILES_AS_TXT'
cd(immersiondir)
immfiles = dir("*.mat")
immfnames = {immfiles.name};
immfnames = immfnames(:);


% Dir for GCDC WAAL Brood-guard accelerometer data
accdir = '/Volumes/GoogleDrive/.shortcut-targets-by-id/1mz4wiprZFgV9h0akSSPfVABY_vpmeDfb/Wandering Albatross Data/Tag_Data/L1_Tag_Data/2018-2020_ACC_Uniformat'
cd(accdir);
% List relevant WAAL brood data
accfiles = dir('*.txt');
fnames = {accfiles(:).name};
fnames = fnames(:);
idx = find(contains(fnames, 'brood-guard'));

accfiles = accfiles(idx);

% set up "fake" for loop, 
% type i = 1 into the command window for initial file
i = i+1


% Load accelerometer L1 data (uniformat)
cd(accdir);
acc = readtable(accfiles(i).name);
% Isolate accelerometer data and separate dynamic and static acceleration
A = [acc.Ax,acc.Ay,acc.Az];

% Find corresponding immersion logger file
birdi = strsplit(accfiles(i).name, '_');
birdi = char(birdi(2));
idx = find(contains(immfnames, birdi));

% load corresponding immersion logger data (expanded)
cd(immersiondir);
imm = load(immfiles(idx).name);
imm = imm.immexpand;

% Plot side-by-side
%0 = wet, 1 = dry
% identify the same long stretch of "wet" in the acc data as the immersion data 
subplot(2,1,1);
plot(acc.DateTime, A);
subplot(2,1,2);
plot(imm.DateTime, imm.State);
ylim([-0.1,1.1]);

% Extract acc time of first long bout on water
figure; 
plot(acc.DateTime, A);
ax = gca;
[x,y]=ginput(1); % click location in acc data first, targeting the start of the on water "wet" bout, then click the corresponding region in immersion data
% The ginput function returns the location as numeric values. Use num2ruler to determine the date value that is equivalent to the numeric value returned for x.
acc_date = num2ruler(x,ax.XAxis);

% Extract immersion time of first long bout on water
figure; 
plot(imm.DateTime, imm.State);
ylim([-0.1,1.1]);
ax = gca;
[x,y]=ginput(1); % click location in immersion logger last, targeting the corresponding region in immersion data
% The ginput function returns the location as numeric values. Use num2ruler to determine the date value that is equivalent to the numeric value returned for x.
imm_date = num2ruler(x,ax.XAxis);

% calculate discrepancy between immersion date (correct datetime) and acc
% datetime in seconds

dt_diff = hours(diff([imm_date;acc_date]));

% Write these out to metadata table
N = table({birdi}, acc_date, imm_date, dt_diff,'VariableNames', {'birdID', 'accDateTime', 'immDateTime', 'Dur_Diff_Hours'});
cd(accdir);
if i==1
    writetable(N,'TimeDifferences_metadata.csv');
else
writetable(N,'TimeDifferences_metadata.csv','WriteMode','append');
end

clearvars -EXCEPT i accdir accfiles immfiles immersiondir immfnames

