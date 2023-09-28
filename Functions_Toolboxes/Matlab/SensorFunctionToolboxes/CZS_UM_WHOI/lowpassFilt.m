function filtData = lowpassFilt(rawData, method, option)
% filtData = lowpassFilt(rawData, method, option)
%
%   Currently, the function only supports 'moveAvrg' method, a moving
%   averaging filter.

if nargin < 2 || isempty(method)
    method = 'moveAvrg';
end
filtData = nan;

if strcmpi(method, 'moveAvrg')
    if nargin < 3; option = 10; end
    AVRG_WINDOW = option; % Number of data points that are summed and averaged
    
    % after 'filter', the filtData and rawData would have phase shift of
    % DELAY_IND_NUM, as it would not do the averaging for the middle index
    % of a interval and store the filtered value at the same index. So I
    % add dummy raw data as constant value of the mean of last interval.
    DELAY_IND_NUM = floor((AVRG_WINDOW-1)/2); % compensate for the delay in filtered data
    avrgLastSeg = mean(rawData(end-DELAY_IND_NUM+1:end, :));
    rawDataDummy = [rawData; repmat(avrgLastSeg, DELAY_IND_NUM, 1)];
    
    % do the filter for dummy rawData
    COEFF_VEC = ones(1, AVRG_WINDOW)/AVRG_WINDOW; % The coeff in filter
    filtDataDummy = filter(COEFF_VEC, 1, rawDataDummy);
    filtData = filtDataDummy(1+DELAY_IND_NUM:end, :);
end

end