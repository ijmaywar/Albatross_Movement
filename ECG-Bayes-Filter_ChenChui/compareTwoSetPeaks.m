function [peakCom,probCom] = compareTwoSetPeaks(peak_left,peak_right,prob_left,prob_right,s1_1,x1_1,param)
% this function is used to combine the two sets of data
% INPUT : 
% peak_left , peak_right : the peaks sequence that need to be combined
% prob_left, prob_right : the probability of the two sets of peaks
% x : data
% s : template

medianInter = param.medianInter1;
% the peak that have been detected in both direction
peakCom = peak_left( ismember(peak_left,peak_right)==1 ); 
probCom = prob_left( ismember(peak_left,peak_right)==1);

% find out different peaks from two detection
peakAll = sort(unique([peak_left peak_right]));
differentPeak = setdiff(peakAll,peakCom);
i = 1;
[~,smax1] = max(s1_1);              % peak of the template

if(isempty(differentPeak)~=1)
    while 1
        if(length(differentPeak)>=2)
            if (differentPeak(i+1)-differentPeak(i)<0.5*medianInter)        
                Corr_s1 = sum(s1_1* x1_1(differentPeak(i)-(smax1-1):differentPeak(i)+length(s1_1)-smax1));
                Corr_s2 = sum(s1_1* x1_1(differentPeak(i+1)-(smax1-1):differentPeak(i+1)+length(s1_1)-smax1));
                if Corr_s1>Corr_s2
                    differentPeak(i+1) = [];
                else 
                    differentPeak(i) = [];                
                end 
            else 
                i = i+1;
            end
        if(i+1>length(differentPeak))
            break
        end
        else 
            break
        end
    end
end
    % pick high probability peaks as Peakcom
     n = 1;
     deletePeak = [];
if(isempty(differentPeak)~=1)
    for  i = 1:length(differentPeak)
        if(ismember(differentPeak(i),peak_left))
            prob = prob_left(find(differentPeak(i)==peak_left));
        else 
            prob = prob_right(find(differentPeak(i)==peak_right));
        end
        if(prob>0.95)
            for jj = 1:length(peakCom)-1
                if (differentPeak(i)-peakCom(jj))>0.5*medianInter && peakCom(jj+1) - differentPeak(i) > 0.5*medianInter
                    peakCom = [peakCom(1:jj) differentPeak(i) peakCom(jj+1:end)];
                    probCom = [probCom(1:jj) prob probCom(jj+1:end)];
                end
            end
            deletePeak(n) = i; 
            n = n+1;
        end
    end
if(isempty(deletePeak)~=1)
        differentPeak(deletePeak) = [];
        deletePeak = [];
end
end


% go through all the peaks 
if(isempty(differentPeak)~=1)
n = 1;
for  i = 1:length(differentPeak)
        if(ismember(differentPeak(i),peak_left))
            prob = prob_left(differentPeak(i)==peak_left);
        else 
            prob = prob_right(differentPeak(i)==peak_right);
        end
        if prob<param.probthres
            deletePeak(n) =  i;
            n = n+1;
        elseif (prob<0.95 && prob>=param.probthres)
            for j = 1:length(peakCom)-1
               if(peakCom(j)<differentPeak(i) && peakCom(j+1)>differentPeak(i)) 
                   if (differentPeak(i)-peakCom(j)<0.5*medianInter || peakCom(j+1)-differentPeak(i)<0.5*medianInter )
                       deletePeak(n) = i;
                       n = n+1;           
                   end 
               end
            end           
        end
end 
    if(isempty(deletePeak)~=1)
        differentPeak(deletePeak) = [];
    end
end
if(isempty(differentPeak)~=1)
        for i = 1:length(differentPeak)
            if(find(differentPeak(i)==peak_left))
                prob = prob_left(differentPeak(i)==peak_left);
            else 
                prob = prob_right(differentPeak(i)==peak_right);
            end
            if(differentPeak(i)<=peakCom(end))
                for j = 1:length(peakCom)-1
                    if(differentPeak(i)>peakCom(j) && differentPeak(i)<peakCom(j+1))
                        peakCom = [peakCom(1:j) differentPeak(i) peakCom(j+1:end)];
                        probCom = [probCom(1:j) prob probCom(j+1:end)];
                    end
                end
            else
                peakCom = [peakCom differentPeak(i)];
                probCom = [probCom prob];
            end
        end
end
end

