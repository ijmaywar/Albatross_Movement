function AccelStat = getOdba2(CCAnim, ODBA_METHOD, TimeWindow)


    if ~exist('ODBA_METHOD', 'var') || isempty(ODBA_METHOD)
        ODBA_METHOD = 'total';
        if nargin < 1
            help getSegNew
        end
    end    

    %%
    if nargin < 1
        help getOdba;
    end
    %%
    AccelStat = [];
    %%
    accelTagOrig = CCAnim.Afinal;
    sampleFreq = CCAnim.hz;
    timeHour = CCAnim.Hour;

    totalAccel = accelTagOrig; % accel for averaging

    AVRG_WINDOW = sampleFreq*TimeWindow;
    
    % get mean static/dynamic acc
    staticAccel = lowpassFilt(totalAccel, 'moveAvrg' , AVRG_WINDOW);
    dynAccel = totalAccel - staticAccel;
    
    % get max and std dynamic acc
    maxbaY = slidefun(@max, AVRG_WINDOW, dynAccel(:,1)) ;
    maxbaX = slidefun(@max, AVRG_WINDOW, dynAccel(:,2)) ;
    maxbaZ = slidefun(@max, AVRG_WINDOW, dynAccel(:,3)) ;
    
%     maxQ = slidefun(@max, AVRG_WINDOW, Q);
    
    stdbaY = slidefun(@std, AVRG_WINDOW, dynAccel(:,1));
    stdbaX = slidefun(@std, AVRG_WINDOW, dynAccel(:,2)); 
    stdbaZ = slidefun(@std, AVRG_WINDOW, dynAccel(:,3)); 
%     stdQ = slidefun(@std, AVRG_WINDOW, Q);
    
    % get ODBA
    if strcmpi(ODBA_METHOD, 'wilson')
        odba = abs(dynAccel(:,1)) + abs(dynAccel(:,2)) + abs(dynAccel(:,3));        
    elseif strcmpi(ODBA_METHOD, 'norm')
        odba = sqrt(sum(dynAccel.^2, 2)); % 0.11        
    elseif strcmpi(ODBA_METHOD, 'total')
        odba = abs(sqrt(sum(totalAccel.^2, 2)) - 1); % 0.4        
    end
    
    
        
    %%
    
    AccelStat.totalAccel = totalAccel;
    AccelStat.staticAccel = staticAccel;
    AccelStat.dynAccel = dynAccel;
    AccelStat.maxDBA = [maxbaY,maxbaX,maxbaZ];
    AccelStat.stdDBA = [stdbaY, stdbaX, stdbaZ];
    AccelStat.odba = odba;
    AccelStat.maxQ = maxQ;
    AccelStat.stdQ = stdQ;
    
end
