function [t1k,t2k] = timeind(centerpoint,M,L)
%this function return the sample index of the data that will be fo through
% INPUT : 
% centerpoint : tao + T
% M : 2*M+1 number of models that will be compare
% L : length of the signal
% OUTPUT : 
% t1k-t2k: sample index
     t1k = centerpoint - M - (L-1)/2;
     t2k = centerpoint + M + (L-1)/2;
end

