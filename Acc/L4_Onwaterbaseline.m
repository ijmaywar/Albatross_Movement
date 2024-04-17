%% Clear variables
clearvars

%% User Inputted Values
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway'
szn = '2019_2020';

%% Set directory paths
GD_dir = '/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/';
Acc_dir = strcat(GD_dir, 'L1/', location, '/Tag_Data/Acc/Acc_Technosmart/', szn, '/');
GLS_dir = strcat(GD_dir, 'L1/', location, '/Tag_Data/Immersion/GLS/', szn, '/');
write_dir = strcat(GD_dir, 'L4/', location, '/Tag_Data/Acc/');

cd(Acc_dir);
acc_files = dir('*.csv');

% Define onwaterBaseline (times 2 in MATLAB code)
onwaterBaseline = 2.5e-04;
%%
for i = 1:length(acc_files)
    %%
    m = readtable(acc_files(i).name);
    
    %% Find variance using 2 min moving window
    % Assuming Az column exists in the CSV file
    Mvar_Az = movvar(m.Az, 2*(60*25)); % Using movvar for moving variance calculation
    
    % Perform further analysis or processing here
    
end
