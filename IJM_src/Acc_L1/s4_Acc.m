%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Untilt sensor tags using a rotation matrix of Euler angles.
% 
% Acc_s3_trimmed -> Acc_s4_trimmed_untilted
%
% I. Maywar, M. Conners
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [m,Q,HasNaN] = s4_Acc(m,dropdir,bird)

    fs = 25; %sampling rate
    ns = 2; % number of seconds in smoothing window
    onwaterBaseline = 2.5e-04;

    %% Calculate q-mat
    
    % Make a folder for the current bird's figures
    figdir = strcat('s4_Figures/',bird,'/');
    mkdir(strcat(dropdir,figdir))
    
    A = [m.Ax,m.Ay,m.Az];
    Astc = lowpassFilt(A, 'moveAvrg' , ns*fs); % static acceleration 
    Adyn = A - Astc;  
    
    % Use FindTiltRange function to identify the window of Acc data that will
    % be used to calculate q-mat
    [z_start, z_stop, Mvar_Az, HasNaN] = FindTiltRange(m,onwaterBaseline);
    Asub = Astc(z_start:z_stop,:);
    
    figure
    subplot(2,1,1)
    plot(Astc)
    xline(z_start, 'LineWidth', 2.0)
    xline(z_stop, 'LineWidth', 2.0)
    title('Window used to calculate rotation matrix')
    subplot(2,1,2)
    plot(Mvar_Az)
    ylim([0 0.01])
    yline(onwaterBaseline*2, 'LineWidth', 2.0)
    title('Moving variance (2-min window)')
    % Save a figure that displays the window of Acc data used
    saveas(gcf,strcat(dropdir,figdir,bird,'_TiltWindow&Mvar.png'))
    
    [x_tilt, y_tilt, Q] = AutoRotateToZero(Asub);
    V = rotate_vecs(Asub,Q);
    plott(Asub,25,V,25) % Check rotation after corrected with above rotation matrix
    yline(0, 'LineWidth', 2.0)
    % Save a figure that shows the before and after applying the rotation
    saveas(gcf,strcat(dropdir,figdir,bird,'_Pre&PostTilt.png'))
    
    %% Calculate rotated data for the entire deployment
    
    At = [m.Ax,m.Ay,m.Az];
    Ar = rotate_vecs(At,Q);
        
    % ONE LAST CHECK OF FULL FILE *POST-CORRECTION*
    ns = 2; % number of seconds in smoothing window
    Astc = lowpassFilt(Ar, 'moveAvrg' , ns*fs); % static acceleration 
    Adyn = Ar - Astc;                           % dynamic acceleration 
    plott(Adyn,25,Astc,25)
    % Save a figure displaying the static and dynamic acceleration of the
    % newly untilted data
    saveas(gcf,strcat(dropdir,figdir,bird,'_Static&DynamicAccUntilted.png'))
    
    %% Replace Untilted Axes Data with Rotated Axes

    m.Ax = Ar(:,1);
    m.Ay = Ar(:,2);
    m.Az = Ar(:,3);
    
    close all % close all figs
  
end

    
    
   
