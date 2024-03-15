%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GCDC tags L0->L1
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2018_2019';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'
tagtype = "GCDC";
computer = 'MBP'; % Options: 'MBP', 'ThinkPad'

%% Set envrionment

% Computer
if strcmp(computer,'MBP')
    comp_dir = '/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/';
    l0_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/',location,'/Tag_Data/',szn,'/GCDC/');
    s1_dir = strcat('/Volumes/LaCie/L1/Bird_Island/Tag_Data/Accelerometer/Acc_GCDC/',szn,'/s1_Uniformat/');
    s4_dir = strcat('/Volumes/LaCie/L1/Bird_Island/Tag_Data/Accelerometer/Acc_GCDC/',szn,'/s4_Trimmed_Untilted/');
    GPS_dir = strcat('/Volumes/LaCie/L1/Bird_Island/Tag_Data/GPS/GPS_iGotU/',szn,'/GPS_L1_2_buffer2km/');

elseif strcmp(computer,'ThinkPad')
    % NOT SURE WHAT THIS IS - fill in when using ThinkPad%
    % comp_dir='/Volumes/GoogleDrive/';
    l0_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L0/Bird_Island/Tag_Data/2021_2022/Acc_',tagtype,'/csv/');
    s1_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s1_Uniformat/');
    s2_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s2_AnalysisReady/');
    s3_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s3_Trimmed/');
    s4_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/2021_2022/s4_Trimme_Untilted/');
    GPS_dir = strcat(comp_dir,'.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/L1/Bird_Island/Tag_Data/GPS/GPS_Catlog/',szn,'/GPS_L1_2_buffer2km/');

else
    disp('computer not found');
    return
end

% Matlab functions toolbox
addpath(genpath('/Volumes/LaCie/Functions_Toolboxes/Matlab/'))
addpath(genpath('/Users/ian/Documents/ThorneLab_FlightDynamics/IJM_src/'))

% Full_metadata sheet
fullmeta = readtable('/Volumes/LaCie/Full_metadata.xlsx','Sheet',location,'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% GPS file List
cd(GPS_dir)
GPS_fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

% Set directory to tag type
cd(l0_dir)
l0_fileList = struct2table(dir(l0_dir));
% get rid of aux files
l0_fileList = l0_fileList(strcmp(extractBefore(l0_fileList.name,2),'.')==false,:);

%% Check that each bird has metadata

for i = 1:height(l0_fileList)
    current_bird = l0_fileList.name{i};
    
    % meta
    findmeta = find(strcmp(fullmeta.Acc_OG_ID,current_bird));
    if isempty(findmeta)
        disp(strcat(current_bird," cannot be found in metadata"))
        return
    elseif length(findmeta)>1
        disp(strcat("There are multiple metadata entries for ",current_bird))
        return
    end
end
if i==height(l0_fileList)
    disp("Metadata found for every bird.")
end

        %% Append all files

for i = 1:1%height(l0_fileList)
    current_bird = l0_fileList.name{i};
    findmeta = find(strcmp(fullmeta.Acc_OG_ID,current_bird));
    birdmeta = fullmeta(findmeta,:);
    dep_ID = string(birdmeta.Deployment_ID);
    cd(strcat(l0_dir,current_bird,"/"))
    files_ToAppend = exFAT_aux_remove(struct2table(dir('*.csv')));
    nfiles = height(files_ToAppend);

    % If there are no files to append skip to the next bird
    if nfiles == 0
        continue
    end
    
    % CREATE TABLE FOR THIS DEPLOYMENT TO STORE SOME HELPFUL DATA (e.g. FS)
    T = table(zeros(nfiles,1), NaT(nfiles,1), zeros(nfiles,1), zeros(nfiles,1), 'VariableNames', {'FileNum','StartDateTime','FS', 'NumSamples'});
    
    % LOOP THROUGH EACH FILE IN THE DEPLOYMENT
    for j = 1:nfiles % loop through all files in folder i and append 1:j files
        
        % READ IN THE DATA: 
        % imports matrix of 
        % 1.    Interval (not actual time but the interval
        %       time. Time of logger start was recorded in the field and is
        %       stored in metadata) ***NOTE: These intervals cannot be
        %       trusted. Read up on issue with these tags. 
        % 2. Ax
        % 3. Ay
        % 4. Az
        
        % If we are dealing the the last file in a deployment, there is one
        % line at the end of the file that we do not want as it does not
        % contain accel data, it contains metadata for the elapsed time
        % which we have already extracted above / outside of this files_ToAppend
        % forloop.
        
        if j==nfiles
            dataj=readmatrix(files_ToAppend.name{j},'TreatAsEmpty',{'NA'}, 'NumHeaderLines',8);
            dataj=dataj(1:length(dataj)-1,:); 
        else 
            dataj=readmatrix(files_ToAppend.name{j},'TreatAsEmpty',{'NA'}, 'NumHeaderLines',8);  
        end
        
        % CATCH EMPTY FILES WITHOUT EXITING THE SCRIPT
        % If data j is empty
  
        if isempty(dataj)
            continue;% Skip to bottom of loop and continue with the loop
        end
        
   
        % Fixes infrequent files that import with an extra column of NAs:
        % EIH: encountered a 5th column that contained one numeric value
        % for WAAL brood-guard gcdc tag UA70, file 'DATA-050.CSV'
        % this resulted in the (:,all(~isnan(dataj))) call to return all zeros and the entire dataframe was emptied 
        if size(dataj,2)>4 % If number of columns is greater than 4 there's a problem.
            if any(isnan(dataj(:,5)))
                dataj = dataj(:,1:4);
            else
            dataj = dataj(:,all(~isnan(dataj))); % remove extra column of NAs
            end
        end
        
        
        
        
        
        % Solution from Alex (GCDC) to recreate the time vector:
        % The error can be corrected by resampling the time stamps using the
        % file start_time of each file and and the mode of the differences 
        % in sampling intervals for each file.  

        % The method above caused the final sample in a given file to occur
        % after the first sample in the next file. IJM is testing mean
        % step time instead of the mode of the step time.

        % mean step time didn't work - IJM is forcing everything to 25 hz
        % (0.04 seconds).

        % Forcing everything to 25 hz didn't work. Setting aside GCDC
        % preprocessing for now.
        
        
        % CACULATE THE FS FOR THIS FILE (using mean steptime)
        nsamp = length(dataj);
        % fs = mean(diff(dataj(:,1)));
        fs = 0.04;
        
        T.FileNum(j) = j;
        T.FS(j) = fs;
        T.NumSamples(j) = nsamp;
        
        % If fs is not close to 0.04, print a warning
        if round(fs, 2) ~= 0.04
           
            warning(['The sampling rate is not close to 0.04 (within rounding error): ', 'bird = ', birdid])
           
        end
    

        % GET INITIAL TIMESTART INFORMATION FOR EACH FILE HEADER LINE
      
        fid = fopen(files_ToAppend.name{j});
        a = textscan(fid,'%s',15);
        fclose(fid);
        
        datestart = a{1, 1}{14, 1};     % extract date
        datestart = datestart(1:end-1); % remove trailing comma
        timestart = a{1, 1}{15, 1};     % extract time
        
        datetime_start = [datestart, ' ',timestart];

        t = datestr(datetime_start,'YYYY-mm-DD HH:MM:ss.fff');
        dt = datetime(t, 'Format', 'yyyy-MM-dd HH:mm:ss.SSS'); % 24 HOUR start time in Matlab's datetime format, preserving milliseconds
        T.StartDateTime(j) = dt;
        
        % CALCULATE OUR DATETIME VECTOR
        % DateTime Vector = start_time + (sample number) * (sample period)
        %Multiply (1:nsamp) by the mean sampling rate (fs) of the current
        %file. This should give an elapsed time in seconds. 
        % Add this elapsed time to the start time of the jth file (dt).
        elapsed_time_vec_seconds = seconds(fs*(1:nsamp));
        elapsed_time_vec_seconds = elapsed_time_vec_seconds(:); % reshape
        DateTime = dt + elapsed_time_vec_seconds;  
        
        % ADD TAG ID
       tagid_vec = repmat({dep_ID},nsamp,1);
    % Make new dataframe (data_x) that adds dt_vec column to dataj
      % Desired output across all tagtypes:
      %  TagID
      %  DateTime
      %  Ax
      %  Ay
      %  Az
      %  Mx (NA if acc only), do not add for GCDC
      %  My (NA if acc only), do not add for GCDC
      %  Mz (NA if acc only), do not add for GCDC
      %  Pressure, do not add for GCDC
      %  Temperature, do not add for GCDC
      %  Activity (NA if axyair)
   
    data_x = table(tagid_vec,DateTime,dataj(:,2),dataj(:,3),dataj(:,4), 'VariableNames', {'TagID','DateTime','Ax','Ay','Az'});
  
        % Append file for this bird to data_all (which will contain all data for this bird):
        
        if j==1
            data_all = data_x;
        else
            data_all = [data_all; data_x]; % row bind this file to data_all;
        
        end
        
        
    end
        
    % WRITE TABLE WITH FS METADATA
    mkdir CheckLog
    writetable(T,strcat(dep_ID,'SamplingPeriodCheckLog.txt'))

    %%%% WRITE FILE
    if exist('data_all', 'var')
        writetable(data_all, strcat(s1_dir,dep_ID,'_Acc_L1_s1.txt'))
        % back to home directory
        cd(l0_dir)
        clearvars -except i homedir dfolders writedir year stage species
    else
        cd(l0_dir)
        clearvars -except i homedir dfolders writedir year stage species
        continue 
    end
end
