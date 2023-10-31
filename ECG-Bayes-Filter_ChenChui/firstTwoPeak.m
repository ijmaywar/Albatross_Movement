function [peak1out,noPeakDet] = firstTwoPeak(peak1,prob1)
% This function is used to find out the first two peaks 
%   
    for i = 1:13
        if(prob1(i)>0.9 && prob1(i+1)>0.9 && (peak1(i+1)-peak1(i))>75 && (peak1(i+1)-peak1(i))<320)
            peak1out(1) = peak1(i);
            peak1out(2) = peak1(i+1);
            noPeakDet =0;
            break
        else 
            noPeakDet = 1;
            peak1out=[];
        end
    end
end

