function [peak_diff,prob] = detFirstTwoPeakdiff(L1,s1,x1)
%UNTITLED 此处显示有关此函数的摘要
for i = L1+1:2500
    T(i) = sum(s1'.*x1(i-L1:i-1));
end 
n=1;
peak_diff = [];
[~,I] = max(s1);
for i = 61:length(T)-61
     if(T(i)>T(i-60:i-1)&T(i)>T(i+1:i+60))
              peak_diff(n) = i-(L1-(I-1));
              n = n+1;
     end
end
    peak_diff(15:end) = [];
    prob = [];
    
        
        M = 45;
        N = 75;

    for i =1:length(peak_diff)

        t1k = peak_diff(i)-45;
        t2k = peak_diff(i)+45+L1-1;
        if(t1k<1)
            continue
        end
        if(peak_diff(i)-N-M<1)
            continue
        end

        [~,prob(i),~] = Probability(t1k,t2k,M,L1,N,x1,s1);
    end
end

