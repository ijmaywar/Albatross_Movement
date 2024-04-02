
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DateTime adjustment based on datetime_breaks
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function corrected_DT = DT_adjustment(m,break_tbl)

    if height(break_tbl) ~= 0
        cumulative_correction = [0;cumsum(break_tbl.break_length_mins)];
        for acc_idx = 1:height(m)
            idx_fit = find(break_tbl.break_start >= m.DateTime(acc_idx), 1, 'first');
            if isempty(idx_fit)
                % In this case the idx is found after all breaks
                idx_fit = height(break_tbl)+1;
            end
            corrected_DT(acc_idx) = m.DateTime(acc_idx) + minutes(cumulative_correction(idx_fit));
        end
    else
        corrected_DT = m.DateTime';
    end


    % BREAKS SHOULD CONTAIN NA INSTEAD OF JUST BEING BLANK

    
    % Change m and save m
    m.DateTime_PreCorrection = m.DateTime;
    m.DateTime = corrected_DT';
    m.DateTime_PreCorrection.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';
    m.DateTime.Format = 'yyyy-MM-dd HH:mm:ss.SSSSSS';
    m.DateTime_PreCorrection = string(m.DateTime_PreCorrection);
    m.DateTime = string(m.DateTime);




end

