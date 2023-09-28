% Example script for evaluating the gyro-mag Matlab tools
%
% Copyright (C) 2016, Lucia Martina Martin Lopez & Mark Johnson
% This is free software: you can redistribute it and/or modify it under the
% terms of the GNU General Public License as published by the Free Software 
% Foundation, either version 3 of the License, or any later version.
% See <http://www.gnu.org/licenses/>.
%
% This software is distributed in the hope that it will be useful, but 
% WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
% or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License 
% for more details.
%
% last modified: 22 June 2016

X=dlmread('gyromagdata.csv');	% make sure file is in the matlab working directory
Aw=X(:,1:3);		% extract animal frame acceleration readings
Mw=X(:,4:6);		% extract animal frame magnetic field readings
Gw=X(:,7:9);		% extract animal frame gyroscope readings

fs=25;      % sampling rate of the data in Hz
fc=0.28;    % filter cut-off frequency in Hz - about 0.4 x fluking rate

pryMag = magnet_rot(Mw,fs,fc) ;     % predict BR using magnetometer method
saMag = acc_sa(Aw,pryMag,fs,fc) ;   % predict SA using magnetometer BRs
pryGyro = gyro_rot(Gw,fs,fc) ;      % predict BR using gyroscope method
saGyro = acc_sa(Aw,pryGyro,fs,fc) ; % predict SA using gyroscope BRs

subplot(211),plot([pryGyro pryMag]),grid
subplot(212),plot([saGyro saMag]),grid

% Note: in this data extract, the gyroscope scale factor differs from calibrated 
% value due to a temperature difference. This results in a small mismatch in the
% magnitude of the BR and SA estimates given by the two methods.
