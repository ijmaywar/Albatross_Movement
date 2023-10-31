function [peak_diff,prob_diff,interval] = MatchDetection(peak_diff,T,param,x1,s1)
% This function is used to detect the peaks
% INPUT : 
% peak_diff : the first two peaks of the sequence 
% T : the interval of the first two peaks 
% param : parameter 
% x1 : sequence that need to be detected 
% s1 : the template extracted from the data 
L1 = param.L;
MV = param.MV;
N1 = param.N;
M1 = param.M;
k = 3;
i = 1;
while 1
     % by using a variable MV, we can catch vary large variation
     tao = peak_diff(end);
     [t1k,t2k]=timeind(tao+T(i)+(L1-1)/2,MV,L1);
     if(t2k > length(x1))
            break
     end
     [peak_v(k),prob_v(k),mkl1(k)] = Probability(t1k,t2k,MV,L1,N1,x1,s1);
     
     % by using a constant M, the algorithm will be more stable
     [t1k,t2k]=timeind(tao+T(i)+(L1-1)/2,M1,L1);
     if(t2k > length(x1))
            break
     end
     [peak_diff(k),prob_diff(k),mkl2(k)] = Probability(t1k,t2k,M1,L1,N1,x1,s1);
     
     % compare the two peak
     if((abs(peak_v(k)-peak_diff(k))>5))
%           [t1,t2]=timeind(peak_v(k),M1,L1);
%           [~,prob_v(k),~] = Probability(t1,t2,M1,L1,N1,x1,s1);
         peak_diff(k) = (mkl2(k)>=mkl1(k))*peak_diff(k) + (mkl2(k)<mkl1(k))*peak_v(k);
         prob_diff(k) = (mkl2(k)>=mkl1(k))*prob_diff(k) + (mkl2(k)<mkl1(k))*prob_v(k);
     end  
     i = i+1;
     T(i) = peak_diff(k)-peak_diff(k-1);
     MV = ceil(T(i)/2);
     
     % control updated T, make sure T will not go very high or very low 
     if(T(i)>2*median(T)||T(i)<0.4*median(T)||T(i)<50||T(i)>350)
          T(i) = round(median(T));
          MV = ceil((T(i)-2)/2);
     end
         k = k+1;
end
 interval = T;
end

