function depth = depth_manage_mgc(dpt, max_dive_depth, DF)
% The 'depth_manage' function takes 'TagData' structure and the 
% 'max_dive_depth' of the corresponding dataset in. Taking noise and depth 
% shift and depth scale into account, exporting 'depth' as output.


P =  -dpt;
sf = round(DF);
% Filter data and find picks.
P_filt = moveAvgFilt(P, 0.5*sf);
[pks, locs] = findpeaks(P_filt, 'MinPeakProminence', 0.1);
% Find and remove "false" peaks which happened during diving.
ppks = nan(3,1);
while(length(ppks)>2)
    [ppks, llocs] = findpeaks(-pks, 'MinPeakProminence', 0.1);
    pks(llocs) = [];
    locs(llocs) = [];
end
% Define beginning and endding 'pks'.
pks_begin = mean(P_filt(1:sf));
pks_end = mean(P_filt(end-sf+1:end));
pks = [pks_begin; pks; pks_end];
locs = [1; locs; length(P_filt)];
% Shift all data down.
pks_interp = interp1(locs, pks, 1:length(P_filt));
P_shift = P_filt - pks_interp';
depth = abs(max_dive_depth)/max(abs(P_shift)) * P_shift;

% Plot if needed.
% if 1
% t = t;
% figure
% hold on
% plot(t, P)
% plot(t, P_filt, 'lineWidth', 2)
% plot(locs/DF, pks, 'ms', 'lineWidth',2)
% grid on
% xlabel('time [sec]')
% ylabel('depth [m]')
% legend('raw', 'filtered', 'peaks')


end