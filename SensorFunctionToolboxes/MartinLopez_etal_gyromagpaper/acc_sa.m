function      [Sa,Acc] = acc_sa(Aw,pry,fs,fc,k)
%    [Sa,Acc] = acc_sa(Aw,pry,fs,fc,k)  
%    Estimate specific acceleration (Sa) given the body rotations
%    estimated from the magnetometer or gyroscope sensors. 
%    
%    INPUT:
%       Aw = nx3 matrix of whale frame triaxial accelerometer signals at 
%            sampling rate fs. n is the number of samples. The columns of 
%            Aw are the sensor measurements for respectively the animal's 
%            x, y, and z axes. Each axis should have the same sensitivity 
%            but the measurements do not need to be calibrated to an 
%            engineering unit. Aw is scaled automatically to have an average
%            magnitude equal to the earth's surface gravity, i.e., 9.81 m/s2.
%       pry = estimated body rotations calculated using magnet_rot.m or
%            gyro_rot.m. Angles are in radians.
%       fs = sensor sampling rate in Hz
%       fc = cut-off frequency of the high and low pass filters used to
%            separate swimming body rotations from slow postural changes. 
%            fc should be a fraction (e.g., 0.4) of the nominal 
%            stroking rate in Hz.
%       k = sample range over which to analyse. Skip this argument to
%            analyse all data. If this argument is specified, it must have
%            the same value as in the call to magnet_rot.m or gyro_rot.m.
%
%    OUTPUT: 
%       Sa = estimated specific acceleration [sx, sy, sz] in m/s^2.
%       Acc= is a structure containing the following elements 
%           [Alf,Ahf,Ga]
%           Alf = normalized low pass filtered 3-axis acceleration signal.
%               It represents the slowly-varying postural changes, in m/s^2. 
%               Normalization is to a field vector intensity of 9.81 m/s^2.
%           Ahf = high-pass filtered 3-axis acceleration signal, in m/s^2.  
%           Ga = estimated orientation (gravity) component of Ahf.
%
%   Note: this functions uses a left-hand rule for the body axes defined as:
%     x = longitudinal axis, +ve rostrally.
%     y = lateral axis, +ve to the right,
%     z = ventral-dorsal axis, +ve dorsally.
%
%   When using devices that combine different sensors as here (accelerometer 
%   and magnetometer or gyroscope) their coordinate systems must be aligned. 
%   If they are not physically aligned, rotate and/or invert the measurements
%   from each axis as necessary to produce aligned signals.
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
% apply a symmetric FIR low-pass filter to Aw and Mw with 0 group delay to 
% obtain the low-pass filtered acceleration signal Alf.
Alf = fir_nodelay(Aw,nf,fl);

% calculate Ahf, the high-pass filtered Aw signal.
Ahf=Aw-Alf;

%to allow cases when no k is given
if nargin<5 || isempty(k)
   k = 1:size(Aw,1) ;
end

% normalize and shorten Alf. As Ahf is assumed to contain negligible specific
% acceleration, it should have a constant magnitude equal to the gravity 
% field intensity which is taken as 9.81 m/s^2.
NA=norm2(Alf(k,:)).^(-1);
Alf=9.81*Alf(k,:).*repmat(NA,1,3);

% scale Ahf the same way. 
Ahf=9.81*Ahf(k,:).*repmat(NA,1,3);

sinpry=sin(pry); % sin of body rotations

%estimate the orientation component of Ahf using a small angle assumption.
Ga = [sinpry(:,1).*Alf(:,3)+sinpry(:,3).*Alf(:,2),...
    sinpry(:,2).*Alf(:,3)-sinpry(:,3).*Alf(:,1),...
    -sinpry(:,1).*Alf(:,1)-sinpry(:,2).*Alf(:,2) ] ;

Sa = Ahf-Ga ;   % specific acceleration estimate in m/s^2
 
field1='Alf'; value1=Alf; 
field2='Ahf'; value2=Ahf; 
field3='Ga'; value3=Ga; 
Acc=struct(field1,value1,field2,value2,field3,value3);
