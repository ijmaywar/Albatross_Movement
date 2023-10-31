There are totally 7 codes. The bayesianModel is the main fuction.
******************************************************************
IMPORTANT:When running the main function, this function requires 
the chronux toolbox to be on the search path.
The chronux toolbox folder has been attached.
*****************************************************************
bayesianModel.m	//main fuction
****************************************************************
The function will save a file 'detectedPeak.mat', with 
'date': The time that the result generated,
'peaks': Detected peaks
'probability' : According probability 
****************************************************************
If you don't want to use the functions  separately, there is no need 
to read the below explanation.
*************************************************************
1.Probability.m 	
//function used to calculate the probability of each models, returen the highest probability peak and the corresponding probability and also the corresponding value of s tilde transpose * y. 


2.Timeind.m 	
//function used to calculate the index of the models, return the start time sample and end time sample that will be calculated in the Probability.m


3.Lowpassfilter.m	
//a low pass filter. Reserve the frequency from 0 - 150 Hz.


4.Diff_Down.m
//function for get two sets of downsampled data

5.detFirstTwoPeakdiff.m		
6.firstTwoPeak.m
//function used to find the first two peaks 


7.MatchDetection.m
8.MatchDetection2.m
// function for detection the peaks

9.compareTwoSetPeaks.mat
//function for combining two setd of peaks


10.meanbeat.mat
// the template extracted from high snr data