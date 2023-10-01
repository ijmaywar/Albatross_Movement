%%% This code expands the immersion logger data to create a timeseries of
%%% when the bird was on the water. Initially written by Ellie for WAAL
%%% timeshift, modified by Melinda to create datetime indices for on-water
%%% IJM edited code and fit it into the updated pipeline.

%% clearvars
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "GLS"; % Options: 'AGM', 'Axy5', 'AxyAir', 'Catlog', 'iGotU', 'GLS'
datatype = "GLS"; % Options: "Accelerometer", "GPS", "GLS", "Magnetometer", "EEG"
computer = "MacMini"; % Options: "MacMini", "MacBookPro"

%% Set envrionment

% set directories

L0_dir = NavigateGD(computer,"L0",location,szn,tagtype,datatype);
L0_dir = strcat(L0_dir,"driftadj/");
L1_dir = NavigateGD(computer,"L1",location,szn,tagtype,datatype);

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% First, bulk rename .deg files as .txt (did this in R with the
% bulk_rename.R script. Much easier to read in .txt files then try to
% figure out how to tell Matlab to deal with .deg file. 

% Set dir for immersion logger data and list immersion wet / dry files
% wet / dry immersion data are contained in the *.deg files
% There are 19 rows of initial "header" metadata
% Column names for wet / dry data start on line 20
cd(L0_dir)
L0_files = dir("*.txt")


%% 1. Read in immersion data 
    % wet / dry sensors are sampled every 6 seconds and duration column is
    % in seconds
    % Desired output: a dataframe of:
    % startIx, endIx for each on-water portion.     
    
for i=3:length(L0_files)
    filesplit = strsplit(L0_files(i).name, '_');
    fileid = strcat(char(filesplit(1)),"_",char(filesplit(2)), "_" ,char(filesplit(5)));      % grab glsid

    L0_data = readtable(L0_files(i).name, 'HeaderLines', 19);
    L0_data.Properties.VariableNames = {'datetime','dur_secs','wet_dry'};
    
    if height(L0_data) < 3
        
    else
        % Make N by 2 matrix of fieldname + value type
        variable_names_types = [["starttime", "datetime"];["endtime", "datetime"]; ["state", "string"]];
    % Make table using fieldnames & value types from above
        df = table('Size',[0,size(variable_names_types,1)],'VariableNames', variable_names_types(:,1),...
            'VariableTypes', variable_names_types(:,2));

    for j=1:length(L0_data.dur_secs)
        statedursecs = seconds(L0_data.dur_secs(j));
        adjsecs = statedursecs-seconds(1);
        df.starttime(j) = L0_data.datetime(j)-adjsecs;
        df.endtime(j) = L0_data.datetime(j);
        df.state(j) = L0_data.wet_dry(j);
 
    end % end loop throughe each row of L0_data
    
    filenamei = strcat(L1_dir, fileid, '_wetdry.txt');
    writetable(df, filenamei);
    
    
    end

    % clearvars -except i immfiles L1_dir immersiondir
    
end


