%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This function automatically rotates the x and y axis from Acc data until 
% the mean x and y are zero
%
% Ian Maywar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function [x_tilt,y_tilt,Q] = AutoRotateToZero(Asub)
    
    % X axis
    current_x_mean = mean(Asub(:,1));
    if current_x_mean > 0
        x_direction = 1;
    elseif current_x_mean < 0
        x_direction = -1;
    end

    x_tilt = 1*x_direction;
    Q = euler2rotmat(pi/180*[x_tilt, 0, 0]);
    V = rotate_vecs(Asub,Q);
    V_x_mean = mean(V(:,1));
    while abs(current_x_mean) > abs(V_x_mean)
        current_x_mean = V_x_mean;
        x_tilt = (abs(x_tilt)+1)*x_direction;
        Q = euler2rotmat(pi/180*[x_tilt, 0, 0]);
        V = rotate_vecs(Asub,Q);
        V_x_mean = mean(V(:,1));
    end
    x_tilt = (abs(x_tilt)-1)*x_direction;

    % Y axis
    current_y_mean = mean(Asub(:,2));
    if current_y_mean > 0
        y_direction = 1;
    elseif current_y_mean < 0
        y_direction = -1;
    end

    y_tilt = 1*y_direction;
    Q = euler2rotmat(pi/180*[x_tilt, y_tilt, 0]);
    V = rotate_vecs(Asub,Q);
    V_y_mean = mean(V(:,2));
    while abs(current_y_mean) > abs(V_y_mean)
        current_y_mean = V_y_mean;
        y_tilt = (abs(y_tilt)+1)*y_direction;
        Q = euler2rotmat(pi/180*[x_tilt, y_tilt, 0]);
        V = rotate_vecs(Asub,Q);
        V_y_mean = mean(V(:,2));
    end
    y_tilt = (abs(y_tilt)-1)*y_direction;

    % Output values
    Q = euler2rotmat(pi/180*[x_tilt, y_tilt, 0]);
end

