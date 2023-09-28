% %Load Neurologger 3 data
% load L1_2020-02-12_220022
% sps = Data.GyroAccelCompass_sps;
% StartStopTime = [25 525];
% V = round(sps*StartStopTime(1)):round(sps*StartStopTime(2));
% x = Data.GyroAccelCompass(V,7);
% y = Data.GyroAccelCompass(V,8);
% z = Data.GyroAccelCompass(V,9);
% 
% %smoothing by moving average with the span 8
% x = smooth(x, 8);
% y = smooth(y, 8);
% z = smooth(z, 8);
% %taking every 8th point
% x = x(8:8:end);
% y = y(8:8:end);
% z = z(8:8:end);

% %Load Neurologger 2A data
% load NL2_2020-02-12_220022
% sps = Data.Magn_sps;
% StartStopTime = [125 625];
% V = round(sps*StartStopTime(1)):round(sps*StartStopTime(2));
% x = Data.Magn(V,1);
% y = Data.Magn(V,2);
% z = Data.Magn(V,3);

%Load Neurologger 2A data from albatross
load Magnetometer
sps = 75;   %Hz
StartStopTime = [0.4 1.68]*1e5; %in seconds
V = round(sps*StartStopTime(1)):round(sps*StartStopTime(2));
x = double(Magn(V,1));    %best +1
y = double(Magn(V,2));   %best +10
z = double(Magn(V,3));

%we have to remove outliers that disturb fitting first
%let's find center as a median value and remove al points that deviate from this 
%center more than 1 Gauss (in fact, absolute values should be around 0.4
%Gauss)
Med = [median(x), median(y), median(z)];
%Med = [mean(x), mean(y), mean(z)];

%find abs value of deviation
Dev = sqrt((x-Med(1)).^2+(y-Med(2)).^2+(z-Med(3)).^2);
%Devs = sort(Dev);
Ind = Dev>1;
x(Ind) = [];
y(Ind) = [];
z(Ind) = [];

% %subtract median values. This increases accuracy. Do not forget to include
% %this shift in correction later; Here this is omitted for simplicity
% x = x-Med(1); 
% y = y-Med(2); 
% z = z-Med(3); 

%let's smooth by moving average, Span = 8; This will leave frequencies below 10 Hz.
%This is probably OK.
Span = 8;
x = smooth(x,Span);
y = smooth(y,Span);
z = smooth(z,Span);

%let's decrease Number of points St1 times - in fact, not used 
St1 = 1;
x = x(1:St1:end); 
y = y(1:St1:end); 
z = z(1:St1:end); 

% do the fitting
[ center, radii, evecs, v, chi2,~ ,stdsum ] = ellipsoid_fit( double([ x y z ]), '' ); 
    %Conversion to "double" is important because this increases accuracy
    %this is especially important when no median subtraction is used    
fprintf( 'Ellipsoid center: %.5g %.5g %.5g\n', center );
fprintf( 'Ellipsoid radii: %.5g %.5g %.5g\n', radii );
fprintf( 'Ellipsoid evecs:\n' );
fprintf( '%.5g %.5g %.5g\n%.5g %.5g %.5g\n%.5g %.5g %.5g\n', ...
    evecs(1), evecs(2), evecs(3), evecs(4), evecs(5), evecs(6), evecs(7), evecs(8), evecs(9) );
fprintf( 'Algebraic form:\n' );
fprintf( '%.5g ', v );
%fprintf( '\nAverage deviation of the fit: %.5f\n', sqrt( chi2 / size( x, 1 ) ) );
fprintf( '\nStandardDeviation from the sphere: %.5f\n', sqrt( stdsum / size( x, 1 ) ) );
%fprintf( '\n' );

% draw data with Step, because too much points
Step = round(1e3/St1);   %1e3; %every 1000th point will be plotted
figure (1); clf;
plot3( x(1:Step:end), y(1:Step:end), z(1:Step:end), '.r' );
hold on;

%draw fit
mind = min( [ x y z ] );
maxd = max( [ x y z ] );
nsteps = 50;
step = ( maxd - mind ) / nsteps;
[ xt, yt, zt ] = meshgrid( linspace( mind(1) - step(1), maxd(1) + step(1), nsteps ), linspace( mind(2) - step(2), maxd(2) + step(2), nsteps ), linspace( mind(3) - step(3), maxd(3) + step(3), nsteps ) );

Ellipsoid = v(1) *xt.*xt +   v(2) * yt.*yt + v(3) * zt.*zt + ...
          2*v(4) *xt.*yt + 2*v(5)*xt.*zt + 2*v(6) * yt.*zt + ...
          2*v(7) *xt    + 2*v(8)*yt    + 2*v(9) * zt;
p = patch( isosurface( xt, yt, zt, Ellipsoid, -v(10) ) );
hold off;
set( p, 'FaceColor', 'g', 'EdgeColor', 'none' );
view( -70, 40 );
axis vis3d equal;
camlight;
lighting phong;
title('Original data');

%%Now correct data

% Step 1: subtract center
d = [ x - center(1), y - center(2), z - center(3) ]; % shift data to origin

% Step 2: rotate to match axes of the ellipsoid to coordinate axes
d = d * evecs; % rotate to cardinal axes of the conic;

% Step 3: scale data to get radius 1 (one can scale later to local magnetic
% field strength if needed)
d = [ d(:,1) / radii(1), d(:,2) / radii(2), d(:,3) / radii(3) ]; % normalize to the conic radii

% Step 4: rotate back to have zero rotation at the end - only scaling should be present!
d = d * inv(evecs);

xc = d(:,1); yc = d(:,2); zc = d(:,3);
v = [1, 1, 1, 0, 0, 0, 0, 0, 0, -1];

% %This is slightly different method to compute residuals used in Matlab Help to magcal()    
% N = length(xc); expMFS = 1;
% r = sum(d.^2,2) - expMFS.^2;
% E = sqrt(r.'*r./N)./(2*expMFS.^2);
% fprintf('Residual error in corrected data : %.5f\n',E);  

%%Draw corrected data
figure(2); clf;
plot3( xc(1:Step:end), yc(1:Step:end), zc(1:Step:end), '.r' );
hold on;

%draw fit
mind = min( [ xc yc zc ] );
maxd = max( [ xc yc zc ] );
nsteps = 50;
step = ( maxd - mind ) / nsteps;
[ xt, yt, zt ] = meshgrid( linspace( mind(1) - step(1), maxd(1) + step(1), nsteps ), linspace( mind(2) - step(2), maxd(2) + step(2), nsteps ), linspace( mind(3) - step(3), maxd(3) + step(3), nsteps ) );

Ellipsoid = v(1) *xt.*xt +   v(2) * yt.*yt + v(3) * zt.*zt + ...
          2*v(4) *xt.*yt + 2*v(5)*xt.*zt + 2*v(6) * yt.*zt + ...
          2*v(7) *xt    + 2*v(8)*yt    + 2*v(9) * zt;
p1 = patch( isosurface( xt, yt, zt, Ellipsoid, -v(10) ) );
hold off;
set( p1, 'FaceColor', 'g', 'EdgeColor', 'none' );
view( -70, 40 );
axis vis3d equal;
camlight;
lighting phong;
title('Corrected data');

%Now let's do correction with the novel matlab function magcal() that
%appeared in Matlab 2019a

%outliers removed before!

% %Hovever, let's put zero of the axes in the center of computed by previous
% %method elipsoid. Unfortunatery, results of magcal depend on location of
% %zero of the axes!!! I did not investigate whether this is arounding error,
% %or omission in the method of computation
%  x = x-center(1);
%  y = y-center(2);
%  z = z-center(3);

[A,b,expMFS]  = magcal(double([x, y, z]),'sym');
xyzCorrected = ([x, y, z]-b)*A;

% N = length(xc);
% r = sum(xyzCorrected.^2,2) - expMFS.^2;
% E = sqrt(r.'*r./N)./(2*expMFS.^2);
% fprintf('Residual error in corrected data : %.5f\n',E);

%Let's scale to sphere of radius 1 for comparison
xyzCorrected = xyzCorrected/expMFS;

xc = xyzCorrected(:,1);
yc = xyzCorrected(:,2);
zc = xyzCorrected(:,3);

stdsum = sum( ( ones(size(xyzCorrected,1),1) - sqrt(sum( xyzCorrected.^2, 2))).^2 );%change 2020-02-12, these are real residuals
fprintf( 'StandardDeviation from the sphere by magcal(): %.5f\n', sqrt( stdsum / size( xyzCorrected, 1 ) ) );
%fprintf( '\n' );

%%Draw corrected by magcal() data
figure(3); clf;
plot3( xc(1:Step:end), yc(1:Step:end), zc(1:Step:end), '.r' );
hold on;

%draw fit
mind = min( [ xc yc zc ] );
maxd = max( [ xc yc zc ] );
nsteps = 50;
step = ( maxd - mind ) / nsteps;
[ xt, yt, zt ] = meshgrid( linspace( mind(1) - step(1), maxd(1) + step(1), nsteps ), linspace( mind(2) - step(2), maxd(2) + step(2), nsteps ), linspace( mind(3) - step(3), maxd(3) + step(3), nsteps ) );

Ellipsoid = v(1) *xt.*xt +   v(2) * yt.*yt + v(3) * zt.*zt + ...
          2*v(4) *xt.*yt + 2*v(5)*xt.*zt + 2*v(6) * yt.*zt + ...
          2*v(7) *xt    + 2*v(8)*yt    + 2*v(9) * zt;
p2 = patch( isosurface( xt, yt, zt, Ellipsoid, -v(10) ) );
hold off;
set( p2, 'FaceColor', 'g', 'EdgeColor', 'none' );
view( -70, 40 );
axis vis3d equal;
camlight;
lighting phong;
title('Corrected data by magcal()');

%Let't shift zero to ellipsoid center and apply again 

x = x-b(1);
y = y-b(2);
z = z-b(3);

[A,b,expMFS]  = magcal(double([x, y, z]),'sym');
xyzCorrected = ([x, y, z]-b)*A;

% N = length(xc);
% r = sum(xyzCorrected.^2,2) - expMFS.^2;
% E = sqrt(r.'*r./N)./(2*expMFS.^2);
% fprintf('Residual error in corrected data : %.5f\n',E);

%Let's scale to sphere of radius 1 for comparison
xyzCorrected = xyzCorrected/expMFS;

xc = xyzCorrected(:,1);
yc = xyzCorrected(:,2);
zc = xyzCorrected(:,3);

stdsum = sum( ( ones(size(xyzCorrected,1),1) - sqrt(sum( xyzCorrected.^2, 2))).^2 );%change 2020-02-12, these are real residuals
fprintf( 'StandardDeviation from the sphere by magcal() after center shift to zero: %.5f\n', sqrt( stdsum / size( xyzCorrected, 1 ) ) );
%fprintf( '\n' );

%%Draw corrected by magcal() data
figure(4); clf;
plot3( xc(1:Step:end), yc(1:Step:end), zc(1:Step:end), '.r' );
hold on;

%draw fit
mind = min( [ xc yc zc ] );
maxd = max( [ xc yc zc ] );
nsteps = 50;
step = ( maxd - mind ) / nsteps;
[ xt, yt, zt ] = meshgrid( linspace( mind(1) - step(1), maxd(1) + step(1), nsteps ), linspace( mind(2) - step(2), maxd(2) + step(2), nsteps ), linspace( mind(3) - step(3), maxd(3) + step(3), nsteps ) );

Ellipsoid = v(1) *xt.*xt +   v(2) * yt.*yt + v(3) * zt.*zt + ...
          2*v(4) *xt.*yt + 2*v(5)*xt.*zt + 2*v(6) * yt.*zt + ...
          2*v(7) *xt    + 2*v(8)*yt    + 2*v(9) * zt;
p2 = patch( isosurface( xt, yt, zt, Ellipsoid, -v(10) ) );
hold off;
set( p2, 'FaceColor', 'g', 'EdgeColor', 'none' );
view( -70, 40 );
axis vis3d equal;
camlight;
lighting phong;
title('Corrected data by magcal() after center shift to zero');










