function [x1_1,x1_2,s1_1,s1_2] = Diff_Down(x,s)
%This function computation the differenced of the data 
% and then downsample the data by 2

% INPUT:
% x: data need to be differenced and downsampled
% s: template
% OUTPUT:
% x1_1: sequence 1 of x 
% x1_2: sequence 2 of x
% s1_1: sequence 1 of s
% s1_2: sequence 2 of s 

x1 = x(2:end)-x(1:end-1);
x1 = filter(lowpassfilter,x1);
x1_1 = downsample(x1,2);
x1_2 = downsample([0; x1],2);

s1 = s(2:end)-s(1:end-1); 
s1 = filter(lowpassfilter,s1);
s1_1 = downsample(s1,2);
s1_2 = downsample([0 s1],2);
end

