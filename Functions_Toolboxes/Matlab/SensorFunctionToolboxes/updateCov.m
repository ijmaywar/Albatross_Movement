function [CHatNew,falseUpdate] = updateCov(indForC,dataSlotColor)
    global L WS
        indForNoise = [];
        breakPoint = [];
        CHatNew = zeros(L,L);
        a = []; % the index to extact data from the original data 
        b = []; % same as above
        falseUpdate = 0;
          if length(indForC) == WS
            %estimate C from current noise 
            for i = 1:WS/L 
                CHatNew = CHatNew + dataSlotColor((i-1)*L+1:i*L)*dataSlotColor((i-1)*L+1:i*L)';
            end 
            CHatNew = CHatNew/(WS/L);
          else % no noise for updating 
            % estimate C from current noise 
            indForNoise = indForC(2:end) - indForC(1:end-1);
            breakPoint = find(indForNoise~=1);
            breakPoint = unique([0,breakPoint,length(indForNoise)]);
            n = 0;
            noiseForC = {};
            for i = 1:length(breakPoint)-1
                if (breakPoint(i+1)-breakPoint(i))> 3*L 
                    n = n+1;
                    a(n) = indForC(breakPoint(i)+1)+ L/2 + L;
                    b(n) = indForC(breakPoint(i+1))+ L/2 - L;
                    noiseForC{n} = dataSlotColor(a(n):b(n));
                end 
            end
            if ~isempty(noiseForC) % if not empty
                noiseLength = [];
                for k = 1:n
                    noiseLength(k) = floor(length(noiseForC{k})/L);
                    for i = 1:noiseLength(k)
                        CHatNew = CHatNew + noiseForC{k}((i-1)*L+1:i*L)*noiseForC{k}((i-1)*L+1:i*L)';
                    end     
                end
                CHatNew = CHatNew/sum(noiseLength);  
            else 
                    falseUpdate = 1;
            end
          end
end 