%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Flap Detection LULU
% Ian Maywar
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% clearvars
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = 'Bird_Island'; % Options: 'Bird_Island', 'Midway', 'Wandering'

%% Set envrionment

s4_dir = strcat('/Volumes/LaCie/L1/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'/s4_Trimmed_Untilted/');
L2_dir = strcat('/Volumes/LaCie/L2/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'/');
%L2_dir = strcat('/Volumes/LaCie/L2/Bird_Island/Tag_Data/Accelerometer/Acc_Technosmart/',szn,'_update/');

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/ThorneLab_FlightDynamics/'))
addpath(genpath('/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/Other computers/My MacBook Pro (1)/ThorneLab_FlightDynamics/IJM_src'))
addpath(genpath('/Users/ian/Library/CloudStorage/GoogleDrive-ian.maywar@stonybrook.edu/Other computers/My MacBook Pro (1)/ThorneLab_FlightDynamics/3rd_party_functions'))

% Full_metadata sheet
fullmeta = readtable('/Volumes/LaCie/Full_metadata.xlsx','Sheet',location,'TreatAsEmpty',{'NA'});
% Specify the field season and location you are interested in
fullmeta = fullmeta(strcmp(fullmeta.Field_season,szn) & strcmp(fullmeta.Location,location),:);

cd(s4_dir)
s4_fileList = exFAT_aux_remove(struct2table(dir('*.txt')));

disp(CheckMeta(fullmeta,s4_fileList,3,"Deployment_ID"))

% Get monitor dimensions to display figures
p = get(0, "MonitorPositions");

% parameter table
param_tbl = readtable(strcat(L2_dir,"ParameterSearch.csv"));

% Prevent figures from popping up when running in the background
set(0,'DefaultFigureVisible','off')

%% Loop thru birds
for j=1:5
%parfor j= 6:10%height(s4_fileList)

    %% Read bird 
    bird = strsplit(s4_fileList.name{j},"_");
    bird = strcat(bird{1},"_",bird{2},"_",bird{3});
    Acc = readtable(s4_fileList.name{j},'Delimiter',',','ReadVariableNames',true,'Format','auto','TreatAsEmpty',{'NA'}); % read bird i's file
    %%% read the accelerometer 'Az' data as a list of values
    raw = [Acc.Az];
%%
  for k=0:1
%%    
    figdir = strcat('Figures/',bird,'/k_',string(k),'/');
    mkdir(strcat(L2_dir,figdir))
    
    %% Choosing values
    m = 8; % Should be 25/3 so that it's 3 Hz. But this is equivalent to m = 8 (can't get finer than one sample).
    % k = 0; % Remove downward signals of one sample.
    minPk = 4; % Guess that peaks 4 samples or less apart are not flaps. Kind of arbitrary but most movingslope almost always returns 4.

    filtered = Ukm(raw,k,m); % 1 is 25 Hz and (25/3) is 3 Hz.
    % temp = Ukm(raw,1,25); % 1 is 25 Hz and 25 is 1 second.

    %% Parameter search  
    
    % find th

    th_range = (-1.5:0.05:-0.5);
    num_flaps_th = [];
    mean_pk_th = [];

    parfor i = 1:length(th_range)
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

    parfor i = 1:length(m_range)
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

    % Search for MinPeakDistance

    minPk_range = (k:7); % k is the lower end because downward pulses k and smaller are annihilated. 
    num_flaps_minPk = [];
    mean_pk_minPk = [];

    parfor i = 1:length(minPk_range)
        minPk_i = minPk_range(i);
        filtered = Ukm(raw,k,m);
        pks = findpeaks(-filtered,'MinPeakHeight',-th,'MinPeakDistance',minPk_i);
        pks = -pks; % Flip the peaks upsidedown
    
        num_flaps = length(pks);
        num_flaps_minPk(i) = num_flaps;
    end

    mvSlp_minPk = abs(movingslope(num_flaps_minPk));
    [~,minPk_idx] = min(mvSlp_minPk);
    minPk = minPk_range(minPk_idx);

    %% Plot figures and save

    % th

    nFlaps_movslp_th = figure;
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

    nFlaps_movslp_m = figure;
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

    % minPkDistance

    nFlaps_movslp_minPk = figure;
    yyaxis left
        scatter(minPk_range,num_flaps_minPk)
        hold on 
        scatter(minPk_range(minPk_idx),num_flaps_minPk(minPk_idx),'LineWidth',5)
        ylabel('Number of flaps')
    yyaxis right
        scatter(minPk_range,mvSlp_minPk)
        hold on
        scatter(minPk_range(minPk_idx),mvSlp_minPk(minPk_idx),'LineWidth',5)
        ylabel('movingslope')
    xlabel('Minimum Peak Distance')
    saveas(nFlaps_movslp_minPk,strcat(L2_dir,figdir,bird,'_search_minPk.fig'))

    %% Final calculation

    filtered = Ukm(raw,k,m);
    [pks,pk_locs,pk_w,pk_p] = findpeaks(-filtered,'MinPeakHeight',-th, 'MinPeakDistance',minPk);
    pks = -pks; % Flip the peaks upsidedown

    num_flaps = length(pks);

    %% Save filtered figure (first 2000 samples)

    filt_fig = figure;
    plot(raw(1:2000))
    hold on
    plot(filtered(1:2000))
    yline(th)
    % xline(pk_locs(1:30))
    saveas(filt_fig,strcat(L2_dir,figdir,bird,'_filtered.fig'))

    %% Save parameters

    if k==0
        th0 = th;
        m0 = m;
        minPk0 = minPk;
        num_flaps0 = num_flaps;

        filtered0 = filtered;
        pks0 = pks;
        pk_locs0 = pk_locs;
        pk_w0 = pk_w;
        pk_p0 = pk_p;

    else
        th1 = th;
        m1 = m;
        minPk1 = minPk;
        num_flaps1 = num_flaps;

        filtered1 = filtered;
        pks1 = pks;
        pk_locs1 = pk_locs;
        pk_w1 = pk_w;
        pk_p1 = pk_p;
    end
  end

    %% Seperation of peaks

    diffPk_locs0 = diff(pk_locs0);
    Pk_dists0(1) = diffPk_locs0(1);
    for i=2:length(diffPk_locs0)
        Pk_dists0(i) = max(diffPk_locs0(i),diffPk_locs0(i-1));
    end
    Pk_dists0(end+1) = diffPk_locs0(end);

    diffPk_locs1 = diff(pk_locs1);
    Pk_dists1(1) = diffPk_locs1(1);
    for i=2:length(diffPk_locs1)
        Pk_dists1(i) = max(diffPk_locs1(i),diffPk_locs1(i-1));
    end
    Pk_dists1(end+1) = diffPk_locs1(end);

    %% Histogram of peak distances and heights

    peak_dist_fig = figure;
    subplot(3,2,1)
        h0 = histogram(Pk_dists0,'BinEdges',(5:1:35));
        title('k=0')
        YL = get(gca, 'YLim');
        ylim(YL)
    subplot(3,2,3)
        h1 = histogram(Pk_dists1,'BinEdges',(5:1:35));
        title('k=1')
        ylim(YL)
    subplot(3,2,5)
        histogram('BinCounts',h0.BinCounts ./ h1.BinCounts,'BinEdges',(5:1:35));
        title('ratio')
        YL = get(gca, 'YLim');
        ylim([1 YL(2)])
    subplot(3,2,2)
        h0 = histogram(-pks0,'BinEdges',(1:.2:4));
        title('k=0')
        YL = get(gca, 'YLim');
        ylim(YL)
    subplot(3,2,4)
        h1 = histogram(-pks1,'BinEdges',(1:.2:4));
        title('k=1')
        ylim(YL)
    subplot(3,2,6)
        histogram('BinCounts',h0.BinCounts ./ h1.BinCounts,'BinEdges',(1:.2:4));
        title('ratio')


    % saveas(peak_dist_fig,strcat(L2_dir,figdir,bird,'_PeakDists.fig'))

    %% Save th, m , MinPeakDistance

    param_tbl.bird(param_tbl.j==j) = {bird};
    param_tbl.th(param_tbl.j==j) = th;
    param_tbl.m(param_tbl.j==j) = m;
    param_tbl.minPk(param_tbl.j==j) = minPk;
    param_tbl.num_flaps(param_tbl.j==j) = num_flaps;
    param_tbl.k1_PecFlapsLost(param_tbl.j==j) = (num_flaps-num_flaps_k1)/num_flaps;
    param_tbl.data_duration_hrs(param_tbl.j==j) = hours(Acc.DateTime(end) - Acc.DateTime(1));
    param_tbl.flaps_per_hour(param_tbl.j==j) = num_flaps/hours(Acc.DateTime(end) - Acc.DateTime(1));

    writetable(param_tbl,strcat(L2_dir,"ParameterSearch.csv"))
        
end




% % %
% % 
% % figure
% % plot(Acc.Ax)
% % hold on 
% % plot(Acc.Ay)
% % plot(Acc.Az)
% % 
% % %
% % 
%%
figure
xaxis = (6347000:6349000);
plot(xaxis,raw(xaxis))
hold on
plot(xaxis,filtered0(xaxis))
yline(th0)
xline(6347335)
% % 
% % 
% % %
% % 
% % filtered = Ukm(raw,1,8);
% % filtered = U_n(raw,2);
% % figure
% % plot(raw(1:2000))
% % hold on
% % xline()
% % plot(filtered(1:2000))
% % 
% % 
% % % Some stats
% % max(diff(pk_locs))/(25*60) % max time between flaps in minutes.


