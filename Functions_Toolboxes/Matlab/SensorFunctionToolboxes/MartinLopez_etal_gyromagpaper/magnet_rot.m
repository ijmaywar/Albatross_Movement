function      [pry,Mag] = magnet_rot(Mw,fs,fc,alpha,n,k)
%    [pry,Mag] = magnet_rot_sa(Mw,fs,fc,alpha,n,k)  
%    Estimate body rotations (pry) using 3-axis magnetometer sensors. 
%    
%    INPUT:
%       Mw = nx3 matrix of whale frame triaxial magnetometer signals at 
%            sampling rate fs. n is the number of samples. The columns of 
%            Mw are the sensor measurements for respectively the animal's 
%            x, y, and z axes. Each axis should have the same sensitivity 
%            but the measurements do not need to be calibrated to an 
%            engineering unit.
%       fs = sensor sampling rate in Hz
%       fc = cut-off frequency of the high and low pass filters used to
%            separate swimming body rotations from slow postural changes. 
%            fc should be a fraction (e.g., 0.4) of the nominal 
%            stroking rate in Hz.
%       alpha =  when the animal's rotation axis is within a few degrees
%            of the Earth's magnetic field vector, stroking rotations will
%            have a poor signal to noise ratio and should be removed from 
%            analysis. alpha sets the angular threshold in degrees below
%            which data will be removed. Default value is 25 degrees. 
%       n = axis around which body rotations are analysed:
%            1 for rotations around the y axis, pitch method (default value).
%            2 for rotations around the x axis, roll method.
%            3 for rotations around the z axis, yaw method.
%       k = sample range over which to analyse. Skip this argument to
%            analyse all data.
%
%    OUTPUT: 
%       pry = estimated body rotations in radians.
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
%            Only one rotation will be estimated depending on n. The other
%            two rotations will be set to zero.
%       Mag = is a structure containing the following elements: 
%            [Mlf,Mhf,Mhfest]
%            Mlf = normalized low pass filtered 3 axis magnetometer
%               signal. It represents the slowly-varying postural changes.
%               Normalization is to a field vector intensity of 1. 
%            Mhf = high-pass filtered 3-axis magnetometer signal.
%            Mhfest = estimate of Mhf allowing only one axis of body
%               rotations. The difference between Mhf and Mhfest gives the
%               observable body rotation that is not modelled by the
%               method.
%
%   Note: this functions uses a left-hand rule for the body axes defined as:
%     x = longitudinal axis, +ve rostrally.
%     y = lateral axis, +ve to the right,
%     z = ventral-dorsal axis, +ve dorsally.
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
% apply a symmetric FIR low-pass filter to Mw with 0 group delay to 
% obtain the low-pass filtered magnetometer signal Mlf.
Mlf = fir_nodelay(Mw,nf,fl);

% calculate Mhf, the high-pass filtered Mw signal as the difference between
% the measured signal and the low-pass filtered signal.
Mhf=Mw-Mlf;

%to allow cases when no alpha is given
if nargin<4 || isempty(alpha)
   alpha = 25;
end

%to allow cases when no n is given
if nargin<5 || isempty(n)
   n = 1;
end

%to allow cases when no k is given
if nargin<6 || isempty(k)
   k = 1:size(Mw,1);
end

% normalize Mlf and shorten it to the samples of interest. It
% should have a constant magnitude equal to the magnetic field intensity.
NM=sqrt(sum(Mlf(k,:).^2,2)).^(-1);

Mlf=Mlf(k,:).*repmat(NM,1,3);

% scale Mhf the same way and shorten it.
Mhf=Mhf(k,:).*repmat(NM,1,3);

if n==1, % pitch method
    fprintf('Estimating rotations around the y axis (pitch method)\n');
    %assume that body rotations can be approximated by a small-angle
    %pitching rotation i.e., rotation around the y axis.
    %Use least squared-error estimator to estimate body rotations
    c = (Mlf(:,1).^2) + (Mlf(:,3).^2) ;
    ic = 1./c ;
    W = [Mlf(:,3).*ic zeros(length(k),1) -Mlf(:,1).*ic] ;
    mp = (Mhf.*W)*[1;1;1] ;
    pry = [real(asin(mp)) zeros(length(k),2)]; % rotations in radians
    % replace with NaN data points when the animal’s lateral axis (y) is within
    % alpha degrees of the Earth's magnetic field vector.
    kk=find((((Mlf(:,1).^2)+ (Mlf(:,3).^2)))<(Mlf(:,2).^2)*((1/cos(alpha*pi/180)^2)-1));
    pry(kk,:)=NaN;
   
    
elseif n==2 %roll method
    fprintf('Estimating rotations around the x axis (roll method)\n');
    %assume that body rotations can be approximated by a small-angle
    %rolling rotation i.e., rotation around the x axis.
    %Use least squared-error estimator to estimate body rotations
    c = (Mlf(:,2).^2)+ (Mlf(:,3).^2) ;
    ic = 1./c ;
    W = [zeros(length(k),1) Mlf(:,3).*ic -Mlf(:,2).*ic] ;
    mp = (Mhf.*W)*[1;1;1] ;
    pry = [zeros(length(k),1) real(asin(mp)) zeros(length(k),1)]; %rotations in radians
    % remove data points when the animal’s longitudinal axis (x)is within
    % alpha degrees of the Earth's magnetic field vector.
    kk=find((((Mlf(:,2).^2)+ (Mlf(:,3).^2)))<(Mlf(:,1).^2)*((1/cos(alpha*pi/180)^2)-1));
    pry(kk,:)=NaN;
    
elseif n==3 %yaw method
    fprintf('Estimating rotations around the z axis (yaw method)\n');
    %assume that body rotations can be approximated by a small-angle
    %yaw rotation i.e., rotation around the z axis.
    %Use least squared-error estimator to estimate body rotations
    c = (Mlf(:,1).^2)+ (Mlf(:,2).^2) ;
    ic = 1./c ;
    W = [Mlf(:,2).*ic  -Mlf(:,1).*ic zeros(length(k),1)] ;
    mp = (Mhf.*W)*[1;1;1] ; 
    pry = [zeros(length(k),2) real(asin(mp))]; %rotations in radians
    % remove data points when the animal’s dorso-ventral axis (z) is within
    % alpha degrees of the Earth's magnetic field vector.
    kk=find((((Mlf(:,1).^2)+ (Mlf(:,2).^2)))<(Mlf(:,3).^2)*((1/cos(alpha*pi/180)^2)-1));
    pry(kk,:)=NaN;

end

 sinpry=sin(pry);%sin of body rotations
 
 %estimate high-pass filtered magnetometer signal assuming that body
 %rotations occur mainly around one body axis. E.g., body rotations during 
 %stroking around the y axis will generate signals in both the x and the z 
 %axis of the magnetometer.
 if n==1, % pitch method;  %this corresponds to Eqn 6 & 7 of (Martin et al., 2015).
 Mhfest=([Mlf(:,3).*sinpry(:,n)  zeros(length(k),1)      -Mlf(:,1).*sinpry(:,n)]); 
 elseif n==2  % roll method;
 Mhfest=([zeros(length(k),1)     Mlf(:,3).*sinpry(:,n)   -Mlf(:,2).*sinpry(:,n)]); 
 elseif n==3  % yaw method;
 Mhfest=([Mlf(:,2).*sinpry(:,n)  -Mlf(:,1).*sinpry(:,n)    zeros(length(k),1) ]); 
 end
 
 field1='Mlf'; value1=Mlf; 
 field2='Mhf'; value2=Mhf; 
 field3='Mhfest'; value3=Mhfest; 
 Mag=struct(field1,value1,field2,value2,field3,value3);
