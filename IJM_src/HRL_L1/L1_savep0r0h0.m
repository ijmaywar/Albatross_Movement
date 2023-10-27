%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tilt HRL files
%
% I. Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Clear variables
clearvars

%% USER INPUTED VALUES

szn = '2019_2020';
location = "Bird_Island"; % Options: 'Bird_Island', 'Midway', 'Wandering'
computer = "MacMini";

%% Set Environment

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

% set directories
GD_dir = findGD(computer);
% L0_0_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/HRL/L0_0_Raw/");
L0_1_dir = strcat(GD_dir,"L0/",location,"/Tag_Data/",szn,"/Aux/HRL/L0_1_Decompressed/");

% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/AlbatrossFlightDynamics/'))

cd(L0_1_dir)
fileList = dir('*.txt'); 

df = table('Size',[length(fileList),2],'VariableTypes',{'string','matrix'});
df.Properties.VariableNames = {'bird','Q'};
df.Q = [0,0,0;0,0,0;0,0,0];

%% Loop thru birds
for i = 1:length(fileList)  
    
    m = readtable(fileList(i).name,'Delimiter',',','ReadVariableNames',true,'TreatAsEmpty',{'NA'});   
    tmp=strsplit(fileList(i).name,"_");
    df.bird(i=char(tmp{1});

    % 3. Rotate Acc+Gyr axes to Mag axes (which is mostly aligned with bird frame, with the exception of the tag tilt)
    
    % rotate Acc+Gyro data
    % Rotation now occurs in import code.
%     ax_rotated = m.Ay.*-1;
%     ay_rotated = m.Ax.*-1;
%     az_rotated = m.Az;
    
    A = table2array([m(:,1), m(:,2), m(:,3)]);
    
    plott(A,75)
    grid on
        
    g=ginput(2);
    g=g.*60*60*75*24; %hrs or days
    Asub = A(g(1,1):g(2,1),:)    ;
    plot(Asub)  
    grid on
    
    % prh=[p0,r0,h0] are the Euler angles in radians of the second frame 
    % %      relative to the first
    Q = euler2rotmat(pi/180*[10 -20 0]); % change values here until you are satisfied with rotation
    V = rotate_vecs(Asub,Q);
    plott(Asub,25,V,25)
    grid on
    
     % if looks good, save Q
    df(i).Q = Q; 
     
    clearvars -except i fileList df
    close all
    
    
    
end
    
     
     save('~/Desktop/Q_rotate_df.mat','df')
      
      
      
      