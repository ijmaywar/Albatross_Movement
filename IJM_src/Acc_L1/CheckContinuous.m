% m is the time series data and rate is the sampling rate in Hz

function [cont] = CheckContinuous(m,rate)

    cont = 1; % intially set to "is continuous"
    ms = 1000/rate;

    out = milliseconds(diff(m.DateTime));
    glitch_idx = find(out~=ms, 1);
    if ~isempty(glitch_idx)
        cont=0;
    end


end
