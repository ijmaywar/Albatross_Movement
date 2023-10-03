% SCRIPT TO SHIFT TIMES of WAAL BROOD DATA 
% based on time discrepancies calculated in s4_plot_calculate_timeshift.m


% Setup
exist('/Users/eheywood/Dropbox/3_BirdIsland/Analyses/Functions_Toolboxes/Matlab','dir')

% Add path to necessary toolboxes
addpath(genpath("/Users/eheywood/Dropbox/3_BirdIsland/Analyses/Functions_Toolboxes/Matlab"))

% Dir for GCDC WAAL Brood-guard accelerometer data to correct
% We are correcting data s4_Acc_AnalysisReady_trimmed_untiled
accdir = '/Volumes/GoogleDrive/My Drive/THORNE_LAB/Data!/Conners_Analysis/HMM/data/s4_Acc_AnalysisReady_trimmed_untilted'
cd(accdir);
% List relevant WAAL brood data
accfiles = dir('WAAL*untilted.txt');

% Load metadata containing timeshift info
cd('/Volumes/GoogleDrive/.shortcut-targets-by-id/1mz4wiprZFgV9h0akSSPfVABY_vpmeDfb/Wandering Albatross Data/Tag_Data/L1_Tag_Data/2018-2020_ACC_Uniformat')
md = readtable('TimeDifferences_metadata.csv');

% cd back to acc files
cd(accdir);

for i=1:length(accfiles)
   bid = strsplit(accfiles(i).name, '_'); 
   bid = bid{2};
    
   idx = find(strcmp(bid, md.birdID));
    
   if isempty(idx)
       continue
   end
    
   % get corresponding times
   Adt = md.accDateTime(idx);
   Idt = md.immDateTime(idx);
   
   % Calculate difference in these timestamps
   % subtract Adt from Idt and then add this difference to our datetime
   % from A
   
   tdiff = Idt-Adt;
   diff([Idt;Adt])
   
   A = readtable(accfiles(i).name); 
   
   dt = A.DateTime;
   
   dtCorrected = dt + tdiff;
   
   % Replace
   A.DateTime = dtCorrected;
   
   % Write out
   fname = strcat('WAAL_',bid,'_brood-guard_AnalysisReady_trimmed_untilted_timecorrected.txt');
   writetable(A, fname);
    
end


