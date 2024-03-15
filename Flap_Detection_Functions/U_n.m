function signal = U_n(x,n)
    signal = movmin(movmax(x,[0 n]),[n 0]);
end