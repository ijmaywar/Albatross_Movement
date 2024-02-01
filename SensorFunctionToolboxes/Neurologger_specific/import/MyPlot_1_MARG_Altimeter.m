close all; clear all; clc

%FName = 'L1';
%FName = 'L2';
%FName = 'L3';
%FName = 'L4';
%FName = 'L5';
%FName = 'L6';
%FName = 'L7';
FName = 'L8';
%FName = 'L1_Saft100uF_1800uF';  %OK, GPS OK
%FName = 'L2_Saft100uF_1800uF';  %OK, GPS OK
%FName = 'L3_Saft100uF_1800uF';  %OK, GPS OK
%FName = 'L4_Saft100uF_1800uF';  %OK, GPS OK

%FName = 'L5_Saft100uF_1800uF_c'; %OK, GPS OK
%FName = 'L6_Saft100uF_1800uF_c'; %OK, GPS OK
%FName = 'L7_Saft100uF_1800uF_c'; %OK, GPS OK
%FName = 'L8_Saft100uF_1800uF_c'; %OK, GPS OK

FName = 'BBAL_86_RB86_HRL4_Jan132020';  %good signal
FName = 'BBAL_99_NA41_HRL2_Jan172020';  %39 "sessions" instead of 1, 19 headers detected, good signal

DataSubdirectory = 'Data_Albatross';%'Data';
PDFSubdirectory = 'PDFs_Albatross'; %'PDFs';

CurrentFolder = pwd;
DataFolder = [CurrentFolder filesep DataSubdirectory];
PDFsFolder = [CurrentFolder filesep PDFSubdirectory];

Parameters.NChannels = 1;  %number of electrophysiological channels
Parameters.sps = 600;     % Hz , was 1600
Parameters.Ku = 166.6;    %input range +/-6 mV
Parameters.Coef = 2000/Parameters.Ku/1024;    %2V, 10 bit probably +/-0.5mV range

FileName = [DataFolder filesep FName];
FileOutputName = [PDFsFolder filesep FName];

[Calibration, EEG_t, Accel_t, Pressure_t, Temp_t, Gyro_t, Magn_t, Sync_t, ...
    HeaderPosition] = ...
    Datalogger4ChConverter_MARG_Altimeter([FileName '.dat'], Parameters);

%Write header positions in the text file
fileID = fopen([FileOutputName '_HeaderPos.txt'],'w');
for I_t = 1:length(HeaderPosition)
  fprintf(fileID,'%d\n',HeaderPosition(I_t));  
end
fclose(fileID);
%End write header positions in the text file

%Get reference values
%[Calibration, Pressure_t, Temp_t] = GetReference;

[ T, P ] = GetCalibTemperaturePressureBMP388( Temp_t, Pressure_t, Calibration );  %Celsius, Pascals

%Values at zero level to compute relative elevation, taken from Zurich
P0 = 96733.5;   %Pa
T0 = 28.2;      %degrees C

[ H ] = GetAltitude( P0, T0, P );  %Celsius, Pascals; altitude meters

%%
%temporarily take only 1 channel - removed
EEG_t = EEG_t(:,1);

EEG = single(cast(EEG_t,'uint16'))*Parameters.Coef;
EEG = EEG - repmat(median(EEG),size(EEG,1),1);

Accel = zeros(size(Accel_t),'int16');
for I_t = 1:3
  Accel(:,I_t) = typecast(Accel_t(:,I_t),'int16');  
end    
Accel = single(Accel)/256/128*16; %now +/-16g, was+/-8g

Gyro = zeros(size(Gyro_t),'int16');
for I_t = 1:3
  Gyro(:,I_t) = typecast(Gyro_t(:,I_t),'int16');  
end    
Gyro = single(Gyro)/256/128*2000*pi/180; %2000 degrees/s -> rad/s

Magn = zeros(size(Magn_t),'int16');
for I_t = 1:3
  Magn(:,I_t) = typecast(Magn_t(:,I_t),'int16');  
end    
Magn = single(Magn)/256/128*49.152; %49.152 Gauss

%let's plot magnetometer without 8 lowest bits to check whether they are
%essential or not.
Dim = size(Magn_t);
Mt = Magn_t;
for I_t = 1:Dim(1)
    for J_t = 1:Dim(2)
      Mt(I_t,J_t) = floor(Magn_t(I_t,J_t)/256)*256;
    end
end
%get min and max for each coordinate
MyMin = min(Mt);
MyMax = max(Mt);
%these values coinside - there is no any single jump in high 8 bits ->
%another file is needed for testing

scrsz = get(groot,'ScreenSize');


X = (1:size(EEG,1))/Parameters.sps;
f1 = figure(1); clf;  
set(f1,'Position',[1 1 scrsz(3) scrsz(4)]);

plot(X,EEG');
clear hl
if size(EEG,2)>1
  hl = legend('Ch#1','Ch#2','Ch#3','Ch#4'); 
  set(hl,'box','off');
end
xlabel('Time (s)');
ylabel('ECG (mV)');
xlim(X([1 end]));
box off
set(f1,'Units','Inches');
pos = get(f1,'Position');
set(f1,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f1,[FileOutputName '_ECG'],'-dpdf','-r0');

f10 = figure(10); clf;  
set(f10,'Position',[1 1 scrsz(3) scrsz(4)]);

plot(X,Sync_t);

xlabel('Time (s)');
ylabel('Synchronization');
xlim(X([1 end]));
box off
set(f10,'Units','Inches');
pos = get(f10,'Position');
set(f10,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f10,[FileOutputName '_Sync'],'-dpdf','-r0');

X1 = (1:size(Accel,1))/(Parameters.sps/8);
f2 = figure(2); clf; 
set(f2,'Position',[1 1 scrsz(3) scrsz(4)]); 
plot(X1,Accel');
hl2 = legend('X','Y','Z'); 
set(hl2,'box','off');
xlabel('Time (s)');
ylabel('Acceleration (g)');
xlim(X1([1 end]));
box off
set(f2,'Units','Inches');
pos = get(f2,'Position');
set(f2,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f2,[FileOutputName '_Acceleration'],'-dpdf','-r0');

X1 = (1:size(Gyro,1))/(Parameters.sps/8);
f20 = figure(20); clf; 
set(f20,'Position',[1 1 scrsz(3) scrsz(4)]); 
plot(X1,Gyro');
hl2 = legend('X','Y','Z'); 
set(hl2,'box','off');
xlabel('Time (s)');
ylabel('Rotation (Rad/s)');
xlim(X1([1 end]));
box off
set(f20,'Units','Inches');
pos = get(f20,'Position');
set(f20,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f20,[FileOutputName '_Gyroscope'],'-dpdf','-r0');

X1 = (1:size(Magn,1))/(Parameters.sps/8);
f21 = figure(21); clf; 
set(f21,'Position',[1 1 scrsz(3) scrsz(4)]); 
plot(X1,Magn');
hl2 = legend('X','Y','Z'); 
set(hl2,'box','off');
xlabel('Time (s)');
ylabel('Magnetic field (Gauss)');
xlim(X1([1 end]));
box off
set(f21,'Units','Inches');
pos = get(f21,'Position');
set(f21,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f21,[FileOutputName '_Magnetometer'],'-dpdf','-r0');

T(1:3) = [];    %some noise here
X1 = (1:size(T,1))/(Parameters.sps/8);
f3 = figure(3); clf; 
set(f3,'Position',[1 1 scrsz(3) scrsz(4)]);
plot(X1,T'); 
%set(hl,'box','off');
xlabel('Time (s)');
ylabel('Temperature (°C)');
xlim(X1([1 end]));
%ylim([25 40]);
box off
set(f3,'Units','Inches');
pos = get(f3,'Position');
set(f3,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f3,[FileOutputName '_Temperature'],'-dpdf','-r0');

P(1:3) = [];    %some noise here
X1 = (1:size(P,1))/(Parameters.sps/8);
f4 = figure(4); clf; 
set(f4,'Position',[1 1 scrsz(3) scrsz(4)]);
plot(X1,P'); 
%set(hl,'box','off');
xlabel('Time (s)');
ylabel('Pressure (Pa)');
xlim(X1([1 end]));
%ylim([9.6 10.3]*1e4);
%line(X1([1 end]),96733.5*[1 1],'color','r');
title(['P_0 = ' num2str(P0) ' Pa, T_0 = ' num2str(T0) '°C']);
box off
set(f4,'Units','Inches');
pos = get(f4,'Position');
set(f4,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f4,[FileOutputName '_Pressure'],'-dpdf','-r0');

H(1:3) = [];    %some noise here
X1 = (1:size(H,1))/(Parameters.sps/8);
f5 = figure(5); clf; 
set(f5,'Position',[1 1 scrsz(3) scrsz(4)]);
plot(X1,H'); 
%set(hl,'box','off');
xlabel('Time (s)');
ylabel('Altitude (m)');
xlim(X1([1 end]));
%ylim([-600 600]);
%line(X1([1 end]),0*[1 1],'color','r');
%line(X1([1 end]),1*[1 1],'color','r','linestyle','--');
%title(['P_0 = ' num2str(P0) ' Pa, T_0 = ' num2str(T0) '°C']);
box off
set(f5,'Units','Inches');
pos = get(f5,'Position');
set(f5,'PaperPositionMode','Auto','PaperUnits','Inches','PaperSize',[pos(3), pos(4)])
print(f5,[FileOutputName '_Altitude'],'-dpdf','-r0');


