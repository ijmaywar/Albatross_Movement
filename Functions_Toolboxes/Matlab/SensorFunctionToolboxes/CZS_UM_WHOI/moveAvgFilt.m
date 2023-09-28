function Xout = moveAvgFilt(Xin, windowLength)
% Moving average filter. Compensated for delay. Using original data
% directly to fill the ending half window of the outputed data and the 
% beginning half window of the outputed data. 
% Xin: m by n input data matrix, filtering performed column wise.
% windowLenght: the window lenght of the moving average filter.
% Xout: 1 dimensional column vector gives output data.

% Check input dimension.
[m, n] = size(Xin);
if m < n    % Not Collumn vectors, error.
    error('Invalid input. Filtering is performed on each column of data matrix.')
    return
end
Xout = zeros(m, n);
windowLength = round(windowLength);
for nn = 1:n
    Xvec = Xin(:,nn);
    indX = 1:m;
    weights = ones(1, windowLength)/windowLength;
    Xfilt = filter(weights, 1, Xvec);
    fDelay = ((windowLength-1)/2);
    indXDelay = indX - fDelay;
    % Interp undelayed data at exact index positions.
    Xundelay = interp1(indXDelay, Xfilt, indX);
    indNan = isnan(Xundelay);
    % Use original data to directly replace nan data.
    Xundelay(indNan) = Xvec(indNan);
    Xundelay([1:round(windowLength/2)]) = Xvec([1:round(windowLength/2)]);
    Xout(:,nn) = Xundelay';
end