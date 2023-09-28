Inventory:
1. gyromagdata.csv; 200 s extract of data analysed in the paper.
   The data come from a DTAG attached to a Blainville's beaked whale 
   diving off the coast of El Hierro in the Canary Islands and were 
   collected under permit from the Spanish Environment Ministry.
   The csv file contains the following columns:
   ax, ay, az, mx, my, mz, gz, gy ,gz
   All data are corrected for the tag orientation on the whale.
   The data sampling rate is 25 Hz. Acceleration data have units of m/s2,
   magnetic field data have units of uT, gyroscope data have units of rad/s.
   For details of data collection see:
   Martin Lopez et al. 'Tracking the kinematics of caudal-oscillatory swimming:
   a comparison of two on-animal sensing methods' Journal Experiment Biology 2016.
2. magnet_rot.m; Matlab function to perform magnetometer processing.
3. gyro_rot.m; Matlab function to perform gyroscope processing.
4. acc_sa.m; Matlab function to perform accelerometer processing.
5. fir_nodelay.m; Matlab function to filter data without adding group delay.
6. gyromagexample.m; Matlab script to process the example data.

Note: The above functions should run in any recent version of Matlab but require
that the signal processing toolbox is installed.

Data is distributed under the terms of the Creative Commons CC BY 4.0 license 
as published by the Creative Commons Corporation.
See <https://creativecommons.org/licenses/by/4.0/>.

All software is distributed under the terms of the GNU General Public 
License as published by the Free Software Foundation, either version 3, 
or any later version. See <http://www.gnu.org/licenses/>.

These data and software is distributed in the hope that they will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the relevant licenses
for more details.

