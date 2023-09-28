function [] = WaveletAnalysis(data_Set,startT,endT,Fs)
% This function is used to plot the wavelet analysis
% please DO NOT make the segment of the data for analysis to large,
% otherwise the figure is not easy to read.
% example : WaveletAnalysis('testdata.mat',76000,78000,～)
% data_Set is the name of the data set (The data contains ONLY the z axis of the ACC)
% startT is the start of the data you are going to do wavelet analysis and
% visualize (here refer to samples)
% endT is the end of the data(also refer to samples)
% Fs is the sampling rate(default is 25)

if nargin == 3
    Fs =  25;
end
raw = data_Set.Az;
if size(raw,1)==1
    raw = raw';
end 
for i = 1:floor((endT-startT+1)/Fs) 
    xl{i} = num2str(i);
end
x = raw(startT:endT);
[wt,wf] = cwt(x,'amor',Fs);
figure
subplot(2,1,1)
plot(x)
title('Time Domain')
xlim([0,endT-startT+1])
xlabel('Time (sec)')
xticks([26:Fs:endT-startT])
xticklabels(xl)

subplot(2,1,2)
pcolor([1:length(x)],wf,abs(wt));
title('Wavelet')
ylabel('Frequency(Hz)')
xlabel('Time (sec)')
xticks([Fs+1:Fs:endT-startT])
xticklabels(xl)
shading interp
CM = max(max(abs(wt)));
caxis([0,CM])
colorbar('southoutside')
end