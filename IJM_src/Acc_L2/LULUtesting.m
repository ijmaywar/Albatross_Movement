
%% Figure 2 data set
x = [3,3,3,1,1,3,3,3,3,5,5,3,3,3,1,1,1,1,3,3,3,3,5,5,5,5,3,3,3,3,1,1,1,1,1,1,3,3,3,3,5,5,5,4,4,6,6,3,3,3];
i = 1;
n = 3;


%% Figure 3 data set
X = -[zeros(1,10),2,zeros(1,40),repmat(5,1,10),8,repmat(5,1,25),6,repmat(5,1,5),repmat(13,1,10),...
    repmat(10,1,3),15,repmat(repelem([10,11],3),1,4),10,11,10,11,11,11,repmat(repelem([10,11],3),1,9),...
    10,10,10,repmat(13,1,10),14,repmat(5,1,20),8,repmat(5,1,20),zeros(1,10),1,zeros(1,35),2,zeros(1,7)];
figure
plot(X,'-b')



%%
figure
subplot(4,1,1)
plot(X,'-b','MarkerFaceColor', 'b')
subplot(4,1,2)
% plot(L_n(x,3),'-or','MarkerFaceColor', 'r')
plot(U_n(X,2),'-r','MarkerFaceColor', 'r')
ylim([-15 0])
% plot(L_n(X,0)-L_n(X,1),'-r','MarkerFaceColor', 'r')
subplot(4,1,3)
plot(U_n(X,11),'-r','MarkerFaceColor', 'r')
ylim([-15 0])
subplot(4,1,4)
plot(Ukm(X,2,11),'-r','MarkerFaceColor', 'r')
ylim([-15 0])
% %% Functions
% 
% % LULU Operators
% 
% function signal = L_n(x,n)
%     signal = movmax(movmin(x,[n 0]),[0 n]);
% end
% 
% function signal = U_n(x,n)
%     signal = movmin(movmax(x,[0 n]),[n 0]);
%     %signal = [3 3 3 3 3 signal(1:end-n)];
% end
% 
% % LULU band pass filter
% 
% function signal = Lmk(x,m,k)
%     signal = (L_n(x,k) - L_n(x,m));
% end



