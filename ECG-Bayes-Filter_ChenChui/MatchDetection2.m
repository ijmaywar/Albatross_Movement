function [peak_diff_2,prob_diff_2,interval] = MatchDetection2(peak_diff_2,T,param,x2,s2)
% This function is used to detect the peaks
% INPUT : 
% peak_diff_2 : the first two peaks of the sequence 
% T : the interval of the first two peaks 
% param : parameter 
% x2 : sequence that need to be detected 
% s2 : the template extracted from the data 
L2 = param.L;
MV = param.MV;
N2 = param.N;
M2 = param.M;
medianInter1 = param.medianInter1;
k = 3;
i = 1;
while 1
     % constant M
     tao = peak_diff_2(end);
     [t1k,t2k]=timeind(tao+T(i)+(L2-1)/2,MV,L2);
     if(t2k > length(x2))
            break
     end
     [peak_v(k),prob_v(k),mkl1(k)] = Probability(t1k,t2k,MV,L2,N2,x2,s2);
     
     % variable MV
     [t1k,t2k]=timeind(tao+T(i)+(L2-1)/2,M2,L2);
     if(t2k > length(x2))
            break
     end
     [peak_diff_2(k),prob_diff_2(k),mkl2(k)] = Probability(t1k,t2k,M2,L2,N2,x2,s2);
     
     % compare two peaks
     if((abs(peak_v(k)-peak_diff_2(k))>5))
%           [t1,t2]=timeind(peak_v(k),M2,L2);
%           [~,prob_v(k),~] = Probability(t1,t2,M2,L2,N2,x2,s2);
         peak_diff_2(k) = (mkl2(k)>=mkl1(k))*peak_diff_2(k) + (mkl2(k)<mkl1(k))*peak_v(k);
         prob_diff_2(k) = (mkl2(k)>=mkl1(k))*prob_diff_2(k) + (mkl2(k)<mkl1(k))*prob_v(k);
     end  
     i = i+1;
     T(i) = peak_diff_2(k)-peak_diff_2(k-1);
     MV = ceil(T(i)/2);
     
     % control the update of the T 
     if(T(i)>1.5*medianInter1||T(i)<0.5*medianInter1)
          T(i) = medianInter1;
          MV = ceil((T(i)-2)/2);
     end
     
         k = k+1;
end
 interval = T;
 peak_diff_2_ = peak_diff_2;
peak_diff_2 = [];
for i = 1:length(peak_diff_2_)
    peak_diff_2(i) = length(x2)+1 - peak_diff_2_(i);
end
peak_diff_2 = fliplr(peak_diff_2);
prob_diff_2 = fliplr(prob_diff_2);
end

