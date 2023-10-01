
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Flap Detection LULU
% This code detects flaps using the L1 Acc data.
% Output: metadata regarding the parameters of the LULU filter and
% threshold. Also an L2 file that records the datetime of every flap and
% some stats.
% Ian Maywar
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% clearvars
clearvars

%% USER INPUTED VALUES

szn = '2022_2023';
location = 'Midway'; % Options: 'Bird_Island', 'Midway', 'Wandering'

%% Set envrionment

% set directories
GD_dir = '/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/.shortcut-targets-by-id/1-mLOKt79AsOpkCFrunvcUj54nuqPInxf/THORNE_LAB/Data/Albatross/NEW_STRUCTURE/';
L1_dir = strcat(GD_dir,'L1/',location,'/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'/');
L2_dir = strcat(GD_dir,'L2/',location,'/Tag_Data/Accelerometer/',szn,'/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% Full_metadata sheet
fullmeta = readtable(strcat(GD_dir,'metadata/Full_metadata.xlsx'),'Sheet',location,'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

% Create L1 file list
cd(L1_dir)
L1_fileList = exFAT_aux_remove(struct2table(dir('*.csv')));

% Make sure that every L1 file can be found in Full_metadata.xlsx
disp(CheckMeta(fullmeta,L1_fileList,3,"Deployment_ID"))

% Get monitor dimensions to display figures
p = get(0, "MonitorPositions");

% Create Parameters folder
mkdir(strcat(L2_dir,"Parameters/"))

%% Loop thru birds
parfor j = 1:height(L1_fileList)
% for j=1:1

    %% Read bird 
    bird = strsplit(L1_fileList.name{j},"_");
    bird = strcat(bird{1},"_",bird{2},"_",bird{3});
    Acc = readtable(L1_fileList.name{j},'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'}); % read bird i's file
    %%% read the accelerometer 'Az' data as a list of values
    raw = [Acc.Az];
   
    figdir = strcat('Figures/',bird,'/');
    mkdir(strcat(L2_dir,figdir))

    meta = struct;
    
    %% Choosing values
    m = 8; % Should be 25/3 so that it's 3 Hz. But this is equivalent to m = 8 (can't get finer than one sample).
    k = 0; % Remove downward signals of one sample.
    minPk = 4; % Guess that peaks 4 samples or less apart are not flaps. Kind of arbitrary but most movingslope almost always returns 4.

    filtered = Ukm(raw,k,m); % 1 is 25 Hz and (25/3) is 3 Hz.
    % temp = Ukm(raw,1,25); % 1 is 25 Hz and 25 is 1 second.

    %% Parameter search  
    
    % find th

    th_range = (-1.5:0.05:-0.5);
    num_flaps_th = [];
    mean_pk_th = [];

    for i = 1:length(th_range)
        th_i = th_range(i);

        pks = findpeaks(-filtered,'MinPeakHeight',-th_i,'MinPeakDistance',minPk);
        pks = -pks; % Flip the peaks upsidedown
    
        num_flaps = length(pks);
        num_flaps_th(i) = num_flaps;
    end

    mvSlp_th = abs(movingslope(num_flaps_th));
    [~,th_idx] = min(mvSlp_th);
    th = th_range(th_idx);

    % find m

    m_range = (5:13); % Flaps typically appear to be 8-10 samples.
    num_flaps_m = [];
    mean_pk_m = [];

    for i = 1:length(m_range)
        m_i = m_range(i);
        filtered = Ukm(raw,k,m_i);
        pks = findpeaks(-filtered,'MinPeakHeight',-th,'MinPeakDistance',minPk);
        pks = -pks; % Flip the peaks upsidedown
    
        num_flaps = length(pks);
        num_flaps_m(i) = num_flaps;
    end

    mvSlp_m = abs(movingslope(num_flaps_m));
    [~,m_idx] = min(mvSlp_m);
    m = m_range(m_idx);

    % % Search for MinPeakDistance

    % minPk_range = (k:7); % k is the lower end because downward pulses k and smaller are annihilated. 
    % num_flaps_minPk = [];
    % mean_pk_minPk = [];
    % 
    % parfor i = 1:length(minPk_range)
    %     minPk_i = minPk_range(i);
    %     filtered = Ukm(raw,k,m);
    %     pks = findpeaks(-filtered,'MinPeakHeight',-th,'MinPeakDistance',minPk_i);
    %     pks = -pks; % Flip the peaks upsidedown
    % 
    %     num_flaps = length(pks);
    %     num_flaps_minPk(i) = num_flaps;
    % end
    % 
    % mvSlp_minPk = abs(movingslope(num_flaps_minPk));
    % [~,minPk_idx] = min(mvSlp_minPk);
    % minPk = minPk_range(minPk_idx);

    %% Plot figures and save

    % th

    nFlaps_movslp_th = figure('visible','off');
    yyaxis left
        scatter(th_range,num_flaps_th)
        hold on
        scatter(th_range(th_idx),num_flaps_th(th_idx),'LineWidth',5)
        ylabel('Number of flaps')
    yyaxis right
        scatter(th_range,mvSlp_th)
        hold on
        scatter(th_range(th_idx),mvSlp_th(th_idx),'LineWidth',5)
        ylabel('movingslope')
    xlabel('threshold')
    saveas(nFlaps_movslp_th,strcat(L2_dir,figdir,bird,'_search_th.fig'))

    % m

    nFlaps_movslp_m = figure('visible','off');
    yyaxis left
        scatter(m_range,num_flaps_m)
        hold on 
        scatter(m_range(m_idx),num_flaps_m(m_idx),'LineWidth',5)
        ylabel('Number of flaps')
    yyaxis right
        scatter(m_range,mvSlp_m)
        hold on
        scatter(m_range(m_idx),mvSlp_m(m_idx),'LineWidth',5)
        ylabel('movingslope')
    xlabel('m')
    saveas(nFlaps_movslp_m,strcat(L2_dir,figdir,bird,'_search_m.fig'))

    % % minPkDistance
    % 
    % nFlaps_movslp_minPk = figure;
    % yyaxis left
    %     scatter(minPk_range,num_flaps_minPk)
    %     hold on 
    %     scatter(minPk_range(minPk_idx),num_flaps_minPk(minPk_idx),'LineWidth',5)
    %     ylabel('Number of flaps')
    % yyaxis right
    %     scatter(minPk_range,mvSlp_minPk)
    %     hold on
    %     scatter(minPk_range(minPk_idx),mvSlp_minPk(minPk_idx),'LineWidth',5)
    %     ylabel('movingslope')
    % xlabel('Minimum Peak Distance')
    % saveas(nFlaps_movslp_minPk,strcat(L2_dir,figdir,bird,'_search_minPk.fig'))

    %% Final calculation

    filtered = Ukm(raw,k,m);
    [pks,pk_locs,pk_w,pk_p] = findpeaks(-filtered,'MinPeakHeight',-th, 'MinPeakDistance',minPk);
    pks = -pks; % Flip the peaks upsidedown

    num_flaps = length(pks);

    pk_nearest = nearestPeak(pk_locs);

    %% Save filtered figure (first 2000 samples)

    filt_fig = figure('visible','off');
    plot(raw(1:2000))
    hold on
    plot(filtered(1:2000))
    yline(th)
    % xline(pk_locs(1:30))
    saveas(filt_fig,strcat(L2_dir,figdir,bird,'_filtered.fig'))

    %% Save flapping data and parameters

    meta.bird = bird;
    meta.th = th;
    meta.m = m;
    meta.minPk = minPk;
    meta.num_flaps = num_flaps;
    meta.data_duration_hrs = hours(Acc.DateTime(end) - Acc.DateTime(1));
    meta.fph = num_flaps/hours(Acc.DateTime(end) - Acc.DateTime(1));
    parsave(meta,strcat(L2_dir,'Parameters/',bird,'_Params.mat'));

    % Save a file that gives flap statistics for every flap
    flap_tbl = table(Acc.DateTime(pk_locs),pks,pk_locs,pk_w,pk_p,pk_nearest,'VariableNames', {'DateTime','pks','pk_locs','pk_w','pk_p','pk_nearest'});
    writetable(flap_tbl,strcat(L2_dir,bird,"_Acc_L2.csv"))

    disp(strcat(bird,'(',num2str(j),'/',num2str(height(L1_fileList)), '): L2 complete.'))

end


























%% Extra code for comparison of peaks
% 
% diffPk_locs0 = diff(pk_locs0);
% Pk_dists0(1) = diffPk_locs0(1);
% for i=2:length(diffPk_locs0)
%     Pk_dists0(i) = max(diffPk_locs0(i),diffPk_locs0(i-1));
% end
% Pk_dists0(end+1) = diffPk_locs0(end);
% 
% diffPk_locs1 = diff(pk_locs1);
% Pk_dists1(1) = diffPk_locs1(1);
% for i=2:length(diffPk_locs1)
%     Pk_dists1(i) = max(diffPk_locs1(i),diffPk_locs1(i-1));
% end
% Pk_dists1(end+1) = diffPk_locs1(end);
% 
% % Histogram of peak distances and heights
% 
% figdir = strcat('Figures/',bird,'/');
% 
% peak_dist_fig = figure;
% subplot(3,2,1)
%     h0 = histogram(Pk_dists0,'BinEdges',(5:1:35));
%     h0Bins = h0.BinCounts;
%     bar(5:1:34,h0Bins)
%     title('k=0')
%     ylabel('num flaps')
%     xlabel('distance to closest peak')
%     YL = get(gca, 'YLim');
%     ylim(YL)
% subplot(3,2,3)
%     h1 = histogram(Pk_dists1,'BinEdges',(5:1:35));
%     h1Bins = h1.BinCounts;
%     bar(5:1:34,h1Bins)
%     title('k=1')
%     ylabel('num flaps')
%     xlabel('distance to closest peak')
%     ylim(YL)
% subplot(3,2,5)
%     bar(5:1:34,(h0Bins-h1Bins)./h0Bins);
%     title('comaprison')
%     ylabel('% change')
%     xlabel('distance to closest peak')
%     ylim([-1 1])
% subplot(3,2,2)
%     h0 = histogram(-pks0,'BinEdges',(1:.2:4));
%     h0Bins = h0.BinCounts;
%     bar(1:0.2:3.8,h0Bins)
%     title('k=0')
%     ylabel('num flaps')
%     xlabel('peak height')
%     YL = get(gca, 'YLim');
%     ylim(YL)
% subplot(3,2,4)
%     h1 = histogram(-pks1,'BinEdges',(1:.2:4));
%     h1Bins = h1.BinCounts;
%     bar(1:0.2:3.8,h1Bins)
%     title('k=1')
%     ylabel('num flaps')
%     xlabel('peak height')
%     ylim(YL)
% subplot(3,2,6)
%     bar(1:0.2:3.8,(h0Bins-h1Bins)./h0Bins);
%     title('comparison')
%     ylabel('% change')
%     xlabel('peak height')
%     ylim([-1 1])
% 
% saveas(peak_dist_fig,strcat(L2_dir,figdir,bird,'_FlapChanges.fig'))

