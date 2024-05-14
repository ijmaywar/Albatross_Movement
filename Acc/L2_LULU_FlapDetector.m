
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

szn = '2019_2020';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway'
AccType = 'Technosmart'; % Options: 'Technosmart', 'NRL'

%% Set envrionment

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

%% Loop thru birds
for j = 15:15%1:height(L1_fileList)
% for j = 4:7

    %% Read bird 
    bird = strsplit(L1_fileList.name{j},"_");
    bird = strcat(bird{1},"_",bird{2},"_",bird{3});
    Acc = readtable(L1_fileList.name{j},'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'}); % read bird i's file
    Acc.DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';
    
    %%% read the accelerometer 'Az' data as a list of values
    raw = [Acc.Az];
   
    figdir = strcat('Figures/',bird,'/');
    mkdir(strcat(L2_dir,figdir))

    meta = struct;
    
    %% Choosing values
    m = 8; % Should be 25/3 so that it's 3 Hz. But this is equivalent to m = 8 (can't get finer than one sample).
    k = 0; % 0 or 1. If set to 1 we can remove single point errors but data might be too coarse to do this. Using 0 for now.
    minPk = 4; % Guess that peaks 4 samples or less apart are not flaps. Kind of arbitrary but most movingslope almost always returns 4.

    filtered = Ukm(raw,k,m); % 1 is 25 Hz and (25/3) is 3 Hz.
    % temp = Ukm(raw,1,25); % 1 is 25 Hz and 25 is 1 second.

    %% Parameter search  
    
    % find th

    th_range = (-1.0:0.05:-0.3);
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

    % The threshold can get stuck where barely any "flaps" are being
    % detected. Push past the local maximum to find the true th.
    % if th_idx == 1
    %     loc_max = find(islocalmax(mvSlp_th),1,'first');
    %     [~,th_idx] = min(mvSlp_th(loc_max:length(mvSlp_th)));
    %     th_idx = loc_max + th_idx - 1;
    % end

    th = th_range(th_idx);

    % find m

    m_range = (7:11); % Flaps typically appear to be 8-10 samples.
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

    %% Plot figures and save

    % th

    nFlaps_movslp_th = figure('visible','on');
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

    nFlaps_movslp_m = figure('visible','on');
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


%% Plot data

plot(filtered(1:10000))

%% Plot data
    
    range = 251:500;
   %  tiledlayout(1,2)
%%
    nexttile
    plot(Acc.DateTime(range),raw(range))
    ylabel('z-axis acceleration (g)')
    xlabel('datetime')
    ylim([-0.5,2.5])
    fontsize(20,"points")
%%
    nexttile
    plot(Acc.DateTime(range),filtered(range))
    hold on
    ylabel('filtered z-axis acceleration')
    xlabel('datetime')
    ylim([-2.1,0.1])
    yline(th)
    fontsize(20,"points")