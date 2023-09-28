function [Calibration, EEG, Accel, Pressure, Temp, Gyro, Magn, Sync, HeaderPosition] = ...
    Datalogger4ChConverter_MARG_Altimeter(Name_tmp, Parameters)
%Ku = [101 11 101 11];    %for sound recorder
%Coef = single(2000./Ku/2048);  %in mV

switch Parameters.NChannels
    case 0
        FrameLength = 3; %accel/pressure/temp
        AbyteC = {'FF','FF','FF','01'};
        Pattern = [1 2 3 4];    %only these four bytes are used in the analysis
    case 1
        FrameLength = 5; % one channel+accel/...
        AbyteC = {'FF','FF','FF','00','00','01'};
        Pattern = [1 2 3 6];    %only these four bytes are used in the analysis
    case 2
        FrameLength = 6; % two channels+accel/...
        AbyteC = {'FF','FF','FF','00','00','00','01'};
        Pattern = [1 2 3 7];    %only these four bytes are used in the analysis        
    case 3
        FrameLength = 8; % three channels+accel/...  
        AbyteC = {'FF','FF','FF','00','00','00','00','00','01'};
        Pattern = [1 2 3 9];    %only these four bytes are used in the analysis        
    case 4  
        FrameLength = 9; % four channels+accel/...     
        AbyteC = {'FF','FF','FF','00','00','00','00','00','00','01'};
        Pattern = [1 2 3 10];    %only these four bytes are used in the analysis 
end    

%find location of a special pattern before calibration that include the
%first element of the array
NbytesToRead = 0.7e5; %was 1e6, in this part of the file search of the header will be done
PatternLength = 24; %was 12 - in bytes 
Abyte = cellfun(@hex2dec,AbyteC);
%FName = [FName '.dat'];

HeaderPosition = FindPatternPosition( Name_tmp, Abyte, Pattern, NbytesToRead );

%let's take and parce calibration data first, and then main array;
CC = cell(length(HeaderPosition),1);
for J_t = 1:length(HeaderPosition)
C = zeros(1,27,'uint8');
%Delta = 4;  %this is for correction, was 0; frame length = 5
%PreCalibLength = 2*FrameLength+Delta;
%PreCalibLength = 4*FrameLength; %4 instead, of 2 changed 15.06.2019 - for 3 chanels at 1200Hz
fid = fopen(Name_tmp, 'r');
for I_t = 1:9
  fseek(fid,HeaderPosition(J_t)+FrameLength+(I_t-1)*FrameLength,'bof');  
  C((I_t-1)*3+(1:3)) = fread(fid,3,'uint8=>uint8');
end
CC{J_t} = C;
end

%here we have to remove labels that has wrong content not matching first configuration
for I_t =length(HeaderPosition):-1:2
   r = isequal(CC{1},CC{I_t});
   if (~r)
     HeaderPosition(I_t) = []; %remove from array  
   end    
end 

C = CC{1}; %addition 07.12.2019

disp(['Headers detected: ' num2str(length(HeaderPosition))]);
disp('Positions:');
disp(num2str(HeaderPosition')); %position before the pattern when counted from 1
                                        %position of the first element when counted from 0`

%the last is just for test
if (FrameLength==8) %3 chanels need correction in 20190703L3 becasue of restart at 37 sec. after start 
  fseek(fid,2*FrameLength,'cof');%string added 15.06.2019 because "big frame" consists of 4 frames
end
%    %neededbecause 2 frames were added to make proper calibration shift
AC = cell(length(HeaderPosition),1);
EEGC = AC;
AccelC = AC;
PressureC = AC; 
TempC = AC; 
GyroC = AC;
MagnC = AC; 
SyncC = AC;

for J_t = 1:length(HeaderPosition)
  I_t = 9;  
  Start = HeaderPosition(J_t)+FrameLength+(I_t-1)*FrameLength+3;
  fseek(fid,Start,'bof');
  if J_t<length(HeaderPosition)
    FragmentLength = HeaderPosition(J_t+1)-Start; 
    AC{J_t} = fread(fid,FragmentLength,'uint8=>uint8'); 
  else
    AC{J_t} = fread(fid,inf,'uint8=>uint8');
  end
end  
fclose(fid);

%This is for BMP388:
Calibration.dig_T1 = uint16(C(2))+bitshift(uint16(C(3)),8); %unsigned
Calibration.dig_T2 = uint16(C(4))+bitshift(uint16(C(5)),8); %unsigned
Calibration.dig_T3 = typecast(uint8(C(6)),'int8');
Calibration.dig_P1 = typecast(uint16(C(7))+bitshift(uint16(C(8)),8),'int16');
Calibration.dig_P2 = typecast(uint16(C(9))+bitshift(uint16(C(10)),8),'int16');
Calibration.dig_P3 = typecast(uint8(C(11)),'int8');
Calibration.dig_P4 = typecast(uint8(C(12)),'int8');
Calibration.dig_P5 = uint16(C(13))+bitshift(uint16(C(14)),8);
Calibration.dig_P6 = uint16(C(15))+bitshift(uint16(C(16)),8);
Calibration.dig_P7 = typecast(uint8(C(17)),'int8');
Calibration.dig_P8 = typecast(uint8(C(18)),'int8');
Calibration.dig_P9 = typecast(uint16(C(19))+bitshift(uint16(C(20)),8),'int16');
Calibration.dig_P10 = typecast(uint8(C(21)),'int8');
Calibration.dig_P11 = typecast(uint8(C(22)),'int8');

% %The last is for BMP280:
% Calibration.dig_T1 = uint16(C(2))+bitshift(uint16(C(3)),8);
% Calibration.dig_T2 = typecast(uint16(C(4))+bitshift(uint16(C(5)),8),'int16');
% Calibration.dig_T3 = typecast(uint16(C(6))+bitshift(uint16(C(7)),8),'int16');
% Calibration.dig_P1 = uint16(C(8))+bitshift(uint16(C(9)),8);
% Calibration.dig_P2 = typecast(uint16(C(10))+bitshift(uint16(C(11)),8),'int16');
% Calibration.dig_P3 = typecast(uint16(C(12))+bitshift(uint16(C(13)),8),'int16');
% Calibration.dig_P4 = typecast(uint16(C(14))+bitshift(uint16(C(15)),8),'int16');
% Calibration.dig_P5 = typecast(uint16(C(16))+bitshift(uint16(C(17)),8),'int16');
% Calibration.dig_P6 = typecast(uint16(C(18))+bitshift(uint16(C(19)),8),'int16');
% Calibration.dig_P7 = typecast(uint16(C(20))+bitshift(uint16(C(21)),8),'int16');
% Calibration.dig_P8 = typecast(uint16(C(22))+bitshift(uint16(C(23)),8),'int16');
% Calibration.dig_P9 = typecast(uint16(C(24))+bitshift(uint16(C(25)),8),'int16');

for J_t = 1:length(HeaderPosition)
    A = AC{J_t};
    
Am = rem(size(A,1),FrameLength)-1;
 if (Am>-1)
   A((size(A,1)-Am):size(A,1),:) = [];
 end;    
A = reshape(A,FrameLength,size(A,1)/FrameLength)';

B = A(:,(FrameLength-2):end)';

%B(1) = [];  %just for test

B = reshape(B,[],1);
Bm = rem(size(B,1),PatternLength)-1;
 if (Bm>-1)
   B((size(B,1)-Bm):size(B,1),:) = [];
 end;    
B = reshape(B,PatternLength,size(B,1)/PatternLength)';

EEG = zeros(size(A,1),Parameters.NChannels, 'uint16');
switch Parameters.NChannels
    case 1
       EEG(:,1) = bitshift(uint16(bitand(A(:,2),112)),4)+uint16(A(:,1)); 
    case 2
       EEG(:,1) = bitshift(uint16(bitand(A(:,2),112)),4)+uint16(A(:,1));  
       EEG(:,2) = bitshift(uint16(bitand(A(:,2),7)),8)+uint16(A(:,3));
    case 3
       EEG(:,1) = bitshift(uint16(bitand(A(:,2),112)),4)+uint16(A(:,1));  
       EEG(:,2) = bitshift(uint16(bitand(A(:,2),7)),8)+uint16(A(:,3));
       EEG(:,3) = bitshift(uint16(bitand(A(:,2+3),112)),4)+uint16(A(:,1+3));  
    case 4   
       EEG(:,1) = bitshift(uint16(bitand(A(:,2),112)),4)+uint16(A(:,1));  
       EEG(:,2) = bitshift(uint16(bitand(A(:,2),7)),8)+uint16(A(:,3));
       EEG(:,3) = bitshift(uint16(bitand(A(:,2+3),112)),4)+uint16(A(:,1+3));
       EEG(:,4) = bitshift(uint16(bitand(A(:,2+3),7)),8)+uint16(A(:,3+3));
end
Sync = []; %just in case if there is no channels
if Parameters.NChannels>0
  Sync = bitshift(bitand(A(:,2),8),-3);  
end 
%Sequence of columns in B match the sequence of recorded data and is the
%following:
%1. Accel X Hi
%2. Accel X Lo
%3. Accel Y Hi
%4. Accel Y Lo
%5. Accel Z Hi
%6. Accel Z Lo
%7. Pressure SuperHi   %in BMP280 sequence was inverted: Lo -> Hi -> SuperHi
%8. Pressure Hi
%9. Pressure Lo
%10. Temperature SuperHi %in BMP280 sequence was inverted: Lo -> Hi -> SuperHi
%11. Temperature Hi
%12. Temperature Lo
%13. Gyro X Hi           %And from here new sensors start
%14. Gyro X Lo
%15. Gyro Y Hi
%16. Gyro Y Lo
%17. Gyro Z Hi
%18. Gyro Z Lo
%19. Magn X Hi
%20. Magn X Lo
%21. Magn Y Hi
%22. Magn Y Lo
%23. Magn Z Hi
%24. Magn Z Lo

Accel = bitshift(uint16(B(:,1:2:6)),8)+uint16(B(:,2:2:6));

P =  B(:,6+(1:3));
Pressure = int32(bitshift(uint32(P(:,3)),16)+bitshift(uint32(P(:,2)),8)+bitshift(uint32(P(:,1)),0));
P = B(:,9+(1:3));
Temp = int32(bitshift(uint32(P(:,3)),16)+bitshift(uint32(P(:,2)),8)+bitshift(uint32(P(:,1)),0));

Gyro = bitshift(uint16(B(:,13:2:18)),8)+uint16(B(:,14:2:18));

Magn = bitshift(uint16(B(:,19:2:24)),8)+uint16(B(:,20:2:24));

EEGC{J_t} = EEG;
AccelC{J_t} = Accel;
PressureC{J_t} = Pressure;
TempC{J_t} = Temp;
GyroC{J_t} = Gyro;
MagnC{J_t} = Magn;
SyncC{J_t} = Sync;
end

%here we have to merge cells into arrays
EEG = cell2mat(EEGC);
Accel = cell2mat(AccelC);
Pressure = cell2mat(PressureC);
Temp = cell2mat(TempC);
Gyro = cell2mat(GyroC);
Magn = cell2mat(MagnC);
Sync = cell2mat(SyncC);




