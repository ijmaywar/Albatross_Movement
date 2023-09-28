function signal = L_n(x,n)
    signal = movmax(movmin(x,[n 0]),[0 n]);
end