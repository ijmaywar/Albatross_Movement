
% Matlab functions toolbox
addpath(genpath('/Users/ian/Documents/GitHub/'))

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

%% Figure 3 data set with NaNs
X = -[zeros(1,10),2,zeros(1,40),NaN(1,10),8,repmat(5,1,25),6,repmat(5,1,5),repmat(13,1,10),...
    repmat(10,1,3),15,repmat(repelem([10,11],3),1,4),10,11,10,NaN,11,11,repmat(repelem([10,11],3),1,9),...
    10,10,10,NaN(1,10),14,repmat(5,1,20),8,repmat(5,1,20),zeros(1,10),1,zeros(1,35),2,zeros(1,7)];
figure
plot(X,'-b')

%% raw data, lower and upper components, bandpass
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

%% raw data, bandpass
figure
subplot(2,1,1)
plot(X,'-b','MarkerFaceColor', 'b')
subplot(2,1,2)
plot(Ukm(X,2,11),'-r','MarkerFaceColor', 'r')
ylim([-15 0])

%% Testing movmin and movmax
figure
subplot(3,1,1)
plot(x,'-b','MarkerFaceColor', 'b')
subplot(3,1,2)
plot(x,'-b','MarkerFaceColor', 'b')
hold on
plot(movmax(x,[0 5]))
subplot(3,1,3)
plot(x,'-b','MarkerFaceColor', 'b')
hold on
plot(movmin(x,[5 0]))


