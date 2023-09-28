function      [pry,Gyro] = gyro_rot(Gw,fs,fc,k)
%    [pry,Gyro] = gyro_rot(Gw,fs,fc,k)
%    Estimate body rotations around the three axes (pry) using a 3-axis 
%    gyroscope sensor. 
%
%    INPUT:
%       Gw = nx3 matrix of whale frame triaxial gyroscope signals at 
%            sampling rate fs. The columns of this matrix are the sensor 
%            measurements for respectively roll rate, pitch rate, and yaw 
%            rate (i.e., rotations about the x, y, and z axes). In each
%            axis, a clockwise rotation viewing in the positive direction
%            is assumed to give a positive rotation rate. The sensor 
%            readings should be calibrated to radians/s.
%       fs = sensor sampling rate in Hz.
%       fc = cut-off frequency of the high and low pass filters used to
%            separate swimming body rotations from slow postural changes. 
%            fc should be a fraction (e.g., 0.4) of the nominal 
%            stroking rate in Hz.
%       k =  sample range over which to analyse. Skip this argument to
%            analyse all data.
%
%    OUTPUT: 
%       pry = estimated body rotations around all three axes in radians.
%            Columns are [pitch, roll, yaw] 
%            Where pitch = body rotations around the y axis (positive
%                 pitch is a clockwise rotation viewing in the +ve y
%                 direction).
%            roll = body rotations around the x axis (positive
%                 roll is a clockwise rotation viewing in the +ve x
%                 direction).
%            yaw = body rotations around the z axis (positive
%                 yaw is an anti-clockwise rotation viewing in the +ve z
%                 direction).
%       Gyro = is a structure containing the following elements 
%            [Ginteg,Glf,Ghf]
%            Ginteg = integrated gyroscope signal giving the instanantaneous  
%               angle relative to a starting orientation. The angles will
%               drift as offsets in the gyroscope accumulate.
%            Glf = low pass filtered 3 axis gyroscope signal representing
%               slow postural changes and gyroscope drift.  
%            Ghf = high-pass filtered 3-axis gyroscope signal representing
%               the body rotations in swimming and maneuvering.
%
%   Note: this functions uses a left-hand rule for the body axes defined as:
%     x = longitudinal axis, +ve rostrally.
%     y = lateral axis, +ve to the right,
%     z = ventral-dorsal axis, +ve dorsally.
%
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

% fl is the filter cut-off normalized to half the sampling frequency
% (Nyquist frequency).
fl=fc/(fs/2);
% define the length of symmetric FIR (Finite Impulse Response) filter.
nf = round(fs/fc*4);
% apply a symmetric FIR low-pass filter to Gw with 0 group delay to 
% obtain the low-pass filtered gyroscope signal Glf
Glf = fir_nodelay(Gw,nf,fl);
%integrate the low-pass filtered gyroscope signal to estimate the absolute angle of rotation
GIlf=cumsum(Glf)/fs;

%to allow cases when no k is given
if nargin<4 || isempty(k)
   k = 1:size(Gw,1) ;
end

% calculate Ghf, the high-pass filtered Ginteg signal.
GIhf=cumsum(Gw(k,:)-Glf(k,:))/fs; 

% matrix of rotations around the y axis, x axis and z axis in
% radians/second. The sign changes are required to match the rotation
% direction definitions.
pry=[GIhf(:,2),GIhf(:,1),GIhf(:,3)];

field1='GIlf'; value1=GIlf; 
field2='GIhf'; value2=GIhf; 
Gyro=struct(field1,value1,field2,value2);
