 clear all;
 % Author : Chen Cui
 % Data : 02/2021
 % This code is used to detect the heart beat
 % IMPORTANT : When running the main function, this function requires 
 % the chronux toolbox to be on the search path.
 % To improve the accuracy of detection, you can choose to INPUT the initialization
 % of the detection.Set the Ini = 1 to input by hand, otherwise set Ini = 0; 
 % Defult Ini = 0, detect the first two peaks auto.IF Ini =1, you need to iput twice.

 %% load data
 % load data to be deteced.
 x= load('medsnr4.txt');
 
 % load template, template is extracted from the high SNR data .
 load('meanbeat');

 %% parameter 
 % change Ini = 1 to input the initialization by hand
 % IF the detection performance is not good, change the Ini = 1 to improve
 % the performance
  Ini = 0;
 
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

    % local detrend
 x = locdetrend(x,fs,[1 .1]);
 
% the low probbility peaks that you want to delete
param.probthres = 0.2; 
 
 %% Differenced data 
[x1_1,x1_2,s1_1,s1_2] = Diff_Down(x,s);
%% test statistic for first several peaks 
% make template length = L1  differenced data
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
        peak_left = input('please input the first two peaks in the figure:e.x.:[120 200]');
    end
else 
    figure
    plot(x1_1(1:5000))
    peak_left = input('please input the first two peaks:e.x.:[120 200]');
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
%% flip the data to re-detect from right to left and get the first two peaks

% flip x1_1
for i = 1:length(x1_1)
    x2_1(i,1) = x1_1(end-i+1);
end

% flip s1_1
s2_1 = fliplr(s1_1);


if Ini ==0
    [peak_right,P] = detFirstTwoPeakdiff(param.L,s2_1,x2_1);

    [peak_right,noPeakDet] = firstTwoPeak(peak_right,P);

    if(noPeakDet)
        disp('The first two peak are not clearly detected')
        figure
        plot(x2_1(1:5000))
        peak_right = input('please input the first two peaks:e.x.:[120 200]');
    end
    clear peak_2 P noPeakDet
else 
    figure
    plot(x2_1(1:5000))
    peak_right = input('please input the first two peaks in the figure:e.x.:[120 200]');
end
%% detect by the differenced data from right to left

T(1) = peak_right(2)-peak_right(1);

% results from right to left sequence
[peak_right,prob_right,interval_right] = MatchDetection2(peak_right,T,param,x2_1,s2_1);

% update the median interval after the second time detection
param.medianInter1 = median(interval_right);

%% combine the two results

% combine the left-right & right-left result
[peakCom,probCom] = compareTwoSetPeaks(peak_left,peak_right,prob_left,prob_right,s1_1,x1_1,param);


%% Vitialization of the result
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

% differenced data and peaks
figure
plot(x1_1)
hold on 
y = x1_1(peakCom);
plot(peakCom,y,'*')
title('Difference data and peaks')

% %Histogram of the interval
% figure
% histogram(peak(2:end)-peak(1:end-1));
% title('Histogram of the interval')
%% Save date of the result peak and probability for each peak
% date = datestr(now);
% save ('detectedPeak.mat','date','peak','probability');

