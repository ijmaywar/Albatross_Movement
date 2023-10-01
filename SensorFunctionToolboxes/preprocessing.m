function y = preprocessing(x)
    mean_x = x - mean(x);
    y = bandpass(mean_x,[2,4.5],25);
end 