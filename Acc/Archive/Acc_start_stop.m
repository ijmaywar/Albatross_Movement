%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% This Code reads the meta structures created when running the L0->L1 code.
% It finds the datetime at which the acc tags start and stop recording.
%
% IJM
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars
 
%% USER INPUTED VALUES

szn = '2022_2023';
location = 'Midway'; % Options: 'Bird_Island', 'Midway'
tagtype = "Technosmart";

%% Set environment

% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/Thorne Lab Shared Drive/Data/Albatross/";
L1_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Acc/Acc_',tagtype,'/',szn,'/');
GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
% fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);
% fullmeta = fullmeta(strcmp(fullmeta.Field_Season,szn) & strcmp(fullmeta.Location,location) & strcmp(fullmeta.Aux_TagType,tagtype),:);

%% How far did I get?

cd(L1_dir)
L1_fileList = dir('*.csv');
L1_fileList(startsWith({L1_fileList.name},'._')) = [];
L1_fileNames = string({L1_fileList.name});

metastruct_dir = strcat(L1_dir,'meta_structures/');
cd(metastruct_dir)
metastruct_fileList = dir('*.mat');
metastruct_fileList(startsWith({metastruct_fileList.name},'._')) = [];
metastruct_fileNames = string({metastruct_fileList.name});

cd(GPS_dir)
GPS_fileList = dir('*.csv');
GPS_fileList(startsWith({GPS_fileList.name},'._')) = [];
GPS_fileNames = string({GPS_fileList.name});

%% Create table
nBirds = length(L1_fileNames);
meta_table = table(cell(nBirds,1),cell(nBirds,1),cell(nBirds,1),...
    'VariableNames',{'Deployment_ID','start_delay','stop_early'}); 

cd(L1_dir)
for i = 1:length(L1_fileNames)
    current_bird = extractBefore(L1_fileNames{i},length(L1_fileNames{i})-10);
    meta_table.Deployment_ID{i} = current_bird;
    
    metastruct_name = strcat(current_bird,'_meta.mat');
    findmeta = find(strcmp(metastruct_fileNames,metastruct_name));

    if length(findmeta)==1
        load(strcat('meta_structures/',metastruct_fileNames{findmeta}))

        % We only care about birds that were fully processed
        if meta.step == 4
            % If there is no Warning_start message for any trip
            if ~all(cellfun(@(x) isempty(x) && isequal(size(x), [0 0]), meta.s3.Warning_start))
                for start_i = 1:height(meta.s3.Warning_start)
                    if ~isempty(meta.s3.Warning_start{start_i})
                        pattern = '\d{1,4}:\d{2}:\d{2}';
                        matches = regexp(string(meta.s3.Warning_start{start_i}), pattern, 'match');
                        if ~isempty(matches)
                            extractedTime = matches{1};
                            meta_table.start_delay{i} = extractedTime;
                        else
                            disp('No time pattern found in the Warning_start message.');
                            break
                        end
                    end
                end
            end
            
            % If there is no Warning_stop message for any trip
            if ~all(cellfun(@(x) isempty(x) && isequal(size(x), [0 0]), meta.s3.Warning_end))
                for stop_i = 1:height(meta.s3.Warning_end)
                    if ~isempty(meta.s3.Warning_end{stop_i})
                        pattern = '\d{1,4}:\d{2}:\d{2}';
                        matches = regexp(string(meta.s3.Warning_end{stop_i}), pattern, 'match');
                        if ~isempty(matches)
                            extractedTime = matches{1};
                            meta_table.stop_early{i} = extractedTime;
                        else
                            disp('No time pattern found in the Warning_end message.');
                            break
                        end
                    end
                end

                % Figure out the datetime at which the acc ended (if it ended
                % early)
                GPS_name = strcat(current_bird,'_GPS_L1_2.csv');
                findGPS = find(strcmp(GPS_fileNames,GPS_name));
                if length(findGPS)==1
                    GPS_m = readtable(strcat(GPS_dir,GPS_name));
                    GPS_end_dt = GPS_m.datetime(end);
                    meta_table.Acc_end_dt(i) = GPS_end_dt - extractedTime;
                end
            end
        end
    end
end
             
%% Write CSV file
writetable(meta_table, strcat(GD_dir,'Analysis/Maywar/Acc_start_stop/Acc_start_stop_',location,'_',szn,'_',tagtype,'.csv'));
