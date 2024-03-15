%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Attach DateTimes to NRL ECG and OtherSensor data
%
%   Reformat data to follow a uniform format (uniformat):
%       DateTime in GMT
%       Ax
%       Ay
%       Az
%       Pressure
%       Temperature
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2021_2022';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway', 'Wandering'

%% Set Environment

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% set directories
GD_dir = "/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/My Drive/NEW_STRUCTURE/";
HD_dir = "/Volumes/LaCie/";
L0_dir = strcat(HD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/2_ECG/");
L1_dir = strcat(HD_dir,"L1/",location,"/Tag_Data/ECG/",szn,"/");
dt_break_dir = strcat(HD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/L0_1_Decompressed/datetime_breaks/");

GPS_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/GPS/GPS_Catlog/',szn,'/2_buffer2km/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% Tag timings sheet
Tag_Timings = readtable(strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/NRL/Tag_Meta_Timings_",szn,".csv"),'Delimiter',',');

% Start indices sheet
idx_tbl = readtable(strcat(L0_dir,"start_stop_indices.csv"),'Delimiter',',');

% Allow figures to be visible
set(0,'DefaultFigureVisible','on')

% suppress annoying warnings when reading Acc_L0 files
warning('off','MATLAB:table:ModifiedAndSavedVarnames')

% load template, template is extracted from the high SNR data .
load('meanbeat');

%% parameter 
% change Ini = 1 to input the initialization by hand
% IF the detection performance is not good, change the Ini = 1 to improve
% the performance. WE SHOULD ALWAYS BE DOING THIS BY HAND. MUCH
% MORE ACCURATE.
Ini = 1;

% samples frequency
fs = 600; 

% L The length of the template, we do not recommend you change the length
% of the template, this length has the best performance when 10 < L < 40
param.L = 31;

%  N number of noise sample account into consideration when calculate the probability
param.N = 75;

% Number of the models
% M for the detection from left to right (constant)
% M for the detection from right to left  (constant)
% MV is variable, will change according to the data as time goes on
param.M = 30;
param.MV = 30;

% the low probbility peaks that you want to delete
param.probthres = 0.2; 

%% Find dt_breaks
cd(dt_break_dir)
dt_break_fileList = dir('*.csv');
dt_break_fileList(startsWith({dt_break_fileList.name},'._')) = [];
dt_break_fileNames = string({dt_break_fileList.name});

%% Find data
cd(L0_dir)
L0_fileList = dir('*.txt');
L0_fileList(startsWith({L0_fileList.name},'._')) = [];
L0_fileNames = string({L0_fileList.name});

%% Loop thru and process birds

for i = 13:length(L0_fileNames)
    %% load data to be deteced.

    namesplit = strsplit(L0_fileNames(i),'_');
    dep_ID = strcat(namesplit{1},'_',namesplit{2},'_',namesplit{3});

    % Make sure this data is usable based on Tag_Timings
    Usable = Tag_Timings(strcmp(Tag_Timings.dep_ID,dep_ID),:).Usable; 

    %%
    % if Usable == 0
    %     disp(strcat(dep_ID, " is unusable."))
    %     continue
    % end
        
    %% Find dt_break
    cd(dt_break_dir)
    birdDTBREAKname = strcat(dep_ID,"_dt_breaks.csv");
    findDTBREAK = find(strcmp(dt_break_fileNames,birdDTBREAKname));
    if isempty(findDTBREAK)
        % GPS file didn't write to there is no break_tbl so just load an
        % empty break_tbl...
        dt_break_tbl = readtable("/Volumes/LaCie/L0/Bird_Island/Tag_Data/2021_2022/Aux/NRL/L0_1_Decompressed/datetime_breaks/WAAL_20220123_OB00_dt_breaks.csv");
    else
        dt_break_tbl = readtable(dt_break_fileNames(findDTBREAK));
    end

    %% Load data
    cd(L0_dir)
    L0_data = table2array(readtable(L0_fileNames(i)));
    
    disp("Data loaded.")

    %% Find ON_DateTime
    birdmeta = fullmeta(strcmp(fullmeta.Deployment_ID,dep_ID),:);
    ON_DateTime = strcat(string(birdmeta.AuxON_date_yyyymmdd), " ", string(birdmeta.AuxON_time_hhmmss));
    ON_DateTime = datetime(ON_DateTime, 'InputFormat','yyyyMMdd HHmmss');

    %% Trim and detrend data

    % trim data
    start_idx = idx_tbl(strcmp(string(idx_tbl.bird),dep_ID),:).start;
    stop_idx = idx_tbl(strcmp(string(idx_tbl.bird),dep_ID),:).stop;
    L0_trimmed = L0_data(start_idx:stop_idx,:);
    
    %% Trim data even more if necessary
    extra_trim = 550;
    new_start = extra_trim + start_idx;
    idx_tbl(strcmp(string(idx_tbl.bird),dep_ID),:).start = new_start;

    writetable(idx_tbl,strcat(L0_dir,"start_stop_indices.csv"));
    disp ("IDX tbl updated.")

    %% Local detrend
    
    x = locdetrend(L0_trimmed,fs,[1 .1]);

    disp("Data trimmed and detrend-ed.")

    %% Differenced data 
    [x1_1,x1_2,s1_1,s1_2] = Diff_Down(x,s);

    disp("Data differenced.")

    %% test statistic for first several peaks 
    % make template length = L0  differenced data
    s1_1(param.L+1:end) = [];
    s1_2(param.L+1:end) = [];
    if Ini == 0
        %get the frist two peaks  differenced data
        [peak_left,P] = detFirstTwoPeakdiff(param.L,s1_1,x1_1);
        
        [peak_left,noPeakDet] = firstTwoPeak(peak_left,P);
        
        if(noPeakDet)
            disp('The first two peak are not clearly detected')
            figure
            plot(x1_1(1:5000))
            peak_left = input('Input the first two peaks in the figure:e.x.:[120 200]');
        end
    else
        figure
        plot(x1_1(1:1000))

        % Use findpeaks to find indices of local maxima
        PeakThreshold = input('Input the threshold for the first two peaks: ');                  
        [pks, locs] = findpeaks(x1_1(1:1000),'MinPeakHeight',PeakThreshold)
        % peak_left = locs(1:2)'

        peak_left = input('Input the first two peaks (e.x.[120 200]): ');
        close all
    end

    %% detect by the differenced data from left to right

    T(1) = peak_left(2)-peak_left(1);
    % result from sequence 1
    % peak_left_s1 : peak detected from sequence 1
    % prob_left_s1 : probability of the peaks
    % interval_left_s1 : the interval between peaks
    [peak_left_s1,prob_left_s1,interval_left_s1] = MatchDetection(peak_left,T,param,x1_1,s1_1);
    
    % result from sequence 2
    [peak_left_s2,prob_left_s2,interval_left_s2] = MatchDetection(peak_left,T,param,x1_2,s1_2);
    
    % median Interval that will be used as a parameter in the following code
    param.medianInter1 = round(median([ interval_left_s1 interval_left_s2 ] ));
    
    % combine the two sets of peaks 
    [peak_left,prob_left] = compareTwoSetPeaks(peak_left_s1,peak_left_s2,prob_left_s1,prob_left_s2,s1_1,x1_1,param);

    disp("peak left detected.")
    
    %% flip the data to re-detect from right to left and get the first two peaks=

    % flip x1_1
    for j = 1:length(x1_1)
        x2_1(j,1) = x1_1(end-j+1);
    end
    
    % flip s1_1
    s2_1 = fliplr(s1_1);
    
    
    if Ini ==0
        [peak_right,P] = detFirstTwoPeakdiff(param.L,s2_1,x2_1);
        
        [peak_right,noPeakDet] = firstTwoPeak(peak_right,P);
        
        if(noPeakDet)
            disp('The first two peaks are not clearly detected')
            figure
            plot(x2_1(1:5000))
            peak_right = input('Input the first two peaks:e.x.:[120 200]');
        end
        clear peak_2 P noPeakDet
    else 
        figure
        plot(x2_1(1:1000))

        % Use findpeaks to find indices of local maxima
        PeakThreshold = input('Input the threshold for the first two peaks: ');
        [pks, locs] = findpeaks(x2_1(1:1000),'MinPeakHeight',PeakThreshold)
        % peak_right = locs(1:2)'
        peak_right = input('Input the first two peaks (e.x.[120 200]): ');
        close all
    end

    %% detect by the differenced data from right to left
    
    T(1) = peak_right(2)-peak_right(1);
    
    % results from right to left sequence
    [peak_right,prob_right,interval_right] = MatchDetection2(peak_right,T,param,x2_1,s2_1);
    
    % update the median interval after the second time detection
    param.medianInter1 = median(interval_right);

    disp("Peak right detected.")
    
    %% combine the two results
    
    % combine the left-right & right-left result
    [peakCom,probCom] = compareTwoSetPeaks(peak_left,peak_right,prob_left,prob_right,s1_1,x1_1,param);

    disp("Peaks combined.") 

    %% Visualization of the result

    % Folder to save figures
    mkdir(strcat(L1_dir,"Figures/",dep_ID))
    
    % original data and peak
    peak = peakCom*2 - 14; % peak in original data
    probability = probCom; % probability 
    figure
    plot(x)
    hold on
    y = x(peak);
    plot(peak,y,'*');
    legend('original data','Detected peak')
    title('Data and peaks')
    saveas(gcf,strcat(L1_dir,"Figures/",dep_ID,"/",dep_ID,"_Original.png"))
    
    % differenced data and peaks
    figure;
    plot(x1_1)
    hold on 
    y = x1_1(peakCom);
    plot(peakCom,y,'*')
    title('Difference data and peaks')
    saveas(gcf,strcat(L1_dir,"Figures/",dep_ID,"/",dep_ID,"_Differenced.png"))
    
    % %Histogram of the interval
    f_hist = figure;
    histogram(peak(2:end)-peak(1:end-1));
    title('Histogram of the interval')
    saveas(gcf,strcat(L1_dir,"Figures/",dep_ID,"/",dep_ID,"_Histogram.png"))

    % %% Save date of the result peak and probability for each peak
    % date = datestr(now);
    % save(strcat(L1_dir,dep_ID,'_detectedPeak.mat'),'date','peak','probability');
    % 
    %% Find datetimes of peaks
    
    % Find the idx of the peaks when the data trimmed from the start is
    % added back on
    og_idx = peak+start_idx-1;
    corrected_idx = og_idx;

    if height(dt_break_tbl) ~= 0
        cumulative_correction = [0;cumsum(dt_break_tbl.ECG_sample_dur)];
        % find index with breaks included
        for og_i = 1:length(og_idx)
            idx_fit = find(dt_break_tbl.ECG_sample_num >= og_idx(og_i), 1, 'first');
            if isempty(idx_fit)
                % In this case the idx is found after all breaks
                idx_fit = height(dt_break_tbl)+1;
            end
            
            corrected_idx(og_i) = og_idx(og_i) + cumulative_correction(idx_fit);
            
        end
    end

    DateTime = (ON_DateTime + seconds((corrected_idx-1)*(1/fs)));        %seconds(0:samplingRate:(nSamples-1)*samplingRate))';
    DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';

    %% Save data
    df_HeartBeats = cell2table(cell(length(peak),3));
    df_HeartBeats.Properties.VariableNames = {'DateTime','idx','probability'};
    
    df_HeartBeats.DateTime = DateTime';
    df_HeartBeats.idx = og_idx';
    df_HeartBeats.corrected_idx = corrected_idx';
    df_HeartBeats.probability = probability';

    writetable(df_HeartBeats,strcat(L1_dir,'/',dep_ID,'_L1.csv')); 

    disp("File saved.")

    %% clear things

    close all
    clearvars -except dt_break_dir dt_break_fileNames fs fullmeta GD_dir GPS_dir idx_tbl Ini L0_dir L0_fileList L0_fileNames L1_dir location param s szn Tag_Timings i

end
        


%% plot start of differenced data

figure
plot(x1_1(1:1000000))
ylim([-1.5 1.5])

%% reverse data

for j = 1:length(x1_1)
    x2_1(j,1) = x1_1(end-j+1);
end


%% plot differenced data (in reverse)

figure
plot(x2_1(1:1000000))
ylim([-1.5 1.5])


% flip x1_1
        



        %% plot start
        figure
        plot(1:10000000,L0_trimmed(1:10000000))
         
        %% plot end 
        figure
        x_end = length(L0_trimmed);
        plot(x_end-10000000:x_end,L0_trimmed(x_end-10000000:x_end))



