%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function locates the section of the Acc data that should be used to
% untilt the tag. It looks for the maximum period of time spent off the
% water. On-water is detected using a 2-min moving variance of preprocessed
% Acc data (taken from FlappinDetection.m).
%
% The start and end of the selected z-axis Acc data are outputted as well as the
% moving variance of Az and if Az contains any NaNs
%
% Ian Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function [z_start, z_stop, Mvar_Az, HasNaN] = FindTiltRange(m, onwaterBaseline)
    Az = m.Az;

    if anynan(Az)
        
        Az_NoNaN = rmmissing(Az);
        HasNaN = true;

        mean_Az = Az_NoNaN - mean(Az_NoNaN);
        pp_Az = bandpass(mean_Az,[2,4.5],25);

        Mvar_Az = movvar(pp_Az,2*(60*25)); % Find variance using 2 min moving window
        
        logic_mAvg = Mvar_Az>onwaterBaseline*2;
        breaks = reshape(find(diff([0;logic_mAvg;0])~=0),2,[]);

        % Delete the start and stop intervals (in "break") that contain
        % incontinuous data
        idx_tracker = find(~isnan(Az));
        continuity = diff([0;idx_tracker]);
        cont_row = [];
        for col = 1:length(breaks)
            cont_row(col) = isempty(find(continuity(breaks(1,col):breaks(2,col)-1)~=1, 1));
        end
        pre_breaks_cont = [breaks;cont_row];
        breaks_cont = pre_breaks_cont(1:2,pre_breaks_cont(3,:)==1);

        [z_range,jmax]=max(diff(breaks_cont));
        z_start = idx_tracker(breaks_cont(1,jmax)); % convert index back to before NaNs were removed.
        z_stop = z_start+z_range-1;
    
    else
        HasNaN = false;
        mean_Az = Az - mean(Az);
        pp_Az = bandpass(mean_Az,[2,4.5],25);

        Mvar_Az = movvar(pp_Az,2*(60*25)); % Find variance using 2 min moving window
        
        logic_mAvg = Mvar_Az>onwaterBaseline*2;
        breaks=reshape(find(diff([0;logic_mAvg;0])~=0),2,[]);
        [z_range,jmax]=max(diff(breaks));
        z_start = breaks(1,jmax);
        z_stop = z_start+z_range-1;
    end



end

% [OLD VERSION THAT USES MOV AVG OF STATIC ACC
% function [z_start, z_stop] = FindTiltRange(Astc, threshold)
%     % Extract z-axis static Acceleration data
%     z = Astc(:,3);
% 
%     % Smooth the Acc data and look for the longest period of time
%     % consecutively spent above the threshold value
%     z_mAvg = movmean(z,(25*60*60));
%     logic_mAvg = z_mAvg>=threshold;
%     i=reshape(find(diff([0;logic_mAvg;0])~=0),2,[]);
%     [z_range,jmax]=max(diff(i));
%     z_start=i(1,jmax);
%     z_stop = z_start+z_range-1;
% 
% end
