function [peak,prob,mMkl] = Probability(t1k,t2k,M,L,N,x,s)
% This fuction return the peaks with highest probability
%   
%INPUT : 
    % t1k-t2k : models that are included
    % M : 2*M+1 the number of the models
    % L : length of the signal
    % N : length of the noise 
    % x : data
    % s : signal

% OUTPUT:
    % peak : the detected peak with highest probability
    % prob : the probability of the peak
    % mMkl : stiled*y
            [~,i] = sort(s,'descend');
             smax = i(1);
            y_t12(1,:) = x(t1k:t2k);
%             y_t12(find(y_t12<0.05))=-0.05;
            for l = 1 : 2*M+1
                stilde = zeros(1,2*M+L);
                stilde(1,l:l+L-1) = s;
                M_kl(l) = sum(stilde.*y_t12); 
                c_kl(l) = (stilde*stilde')^(-1)*M_kl(l); 
            end
            [mMkl,I] = max(M_kl);
            peak = t1k+(I(1)-1) + (smax-1);
            % get the probability
             ytilde(1,:) = x(peak-N-smax:peak-smax);
     for l = 1 : 2*M+1
             p(l) = (-(2*M+L+N-1)/2)*log( ((ytilde*ytilde'+y_t12*y_t12'-c_kl(l)*M_kl(l))/2));
     end
     p = exp(p - max(p));
     P = sum(p);
     p = p/P;   
     prob = max(p);
end

