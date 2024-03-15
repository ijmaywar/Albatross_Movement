% nearest peak function
function pk_nearest = nearestPeak(pk_locs)
    
    diffPk_locs = diff(pk_locs);
    pk_nearest(1) = diffPk_locs(1);

    for i=2:length(diffPk_locs)
        pk_nearest(i) = max(diffPk_locs(i),diffPk_locs(i-1));
    end

    pk_nearest(end+1) = diffPk_locs(end);

    pk_nearest = pk_nearest';

end

