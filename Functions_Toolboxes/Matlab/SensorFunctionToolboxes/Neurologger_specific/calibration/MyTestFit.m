%Load Neurologger 3 data
load L1_2020-02-12_220022
sps = Data.GyroAccelCompass_sps;
StartStopTime = [25 525];
V = round(sps*StartStopTime(1)):round(sps*StartStopTime(2));
x = Data.GyroAccelCompass(V,7);
y = Data.GyroAccelCompass(V,8);
z = Data.GyroAccelCompass(V,9);

%smoothing by moving average with the span 8
x = smooth(x, 8);
y = smooth(y, 8);
z = smooth(z, 8);
%taking every 8th point
x = x(8:8:end);
y = y(8:8:end);
z = z(8:8:end);

% %Load Neurologger 2A data
% load NL2_2020-02-12_220022
% sps = Data.Magn_sps;
% StartStopTime = [125 625];
% V = round(sps*StartStopTime(1)):round(sps*StartStopTime(2));
% x = Data.Magn(V,1);
% y = Data.Magn(V,2);
% z = Data.Magn(V,3);

% do the fitting
[ center, radii, evecs, v, chi2,~ ,stdsum ] = ellipsoid_fit( [ x y z ], '' );
fprintf( 'Ellipsoid center: %.5g %.5g %.5g\n', center );
fprintf( 'Ellipsoid radii: %.5g %.5g %.5g\n', radii );
fprintf( 'Ellipsoid evecs:\n' );
fprintf( '%.5g %.5g %.5g\n%.5g %.5g %.5g\n%.5g %.5g %.5g\n', ...
    evecs(1), evecs(2), evecs(3), evecs(4), evecs(5), evecs(6), evecs(7), evecs(8), evecs(9) );
fprintf( 'Algebraic form:\n' );
fprintf( '%.5g ', v );
%fprintf( '\nAverage deviation of the fit: %.5f\n', sqrt( chi2 / size( x, 1 ) ) );
fprintf( '\nStandardDeviation from the sphere: %.5f\n', sqrt( stdsum / size( x, 1 ) ) );
fprintf( '\n' );

% draw data
figure (1); clf;
plot3( x, y, z, '.r' );
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

x = d(:,1); y = d(:,2); z = d(:,3);
v = [1, 1, 1, 0, 0, 0, 0, 0, 0, -1];

%%Draw corrected data
figure(2); clf;
plot3( x, y, z, '.r' );
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
title('Corrected data');





