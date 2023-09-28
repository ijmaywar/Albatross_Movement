             
function my_magnetometerTest_plot(bird,inc,fs)

subplot(2,1,1)
   plot(inc)
   title([bird, ' Inclination Angle'])
            
subplot(2,1,2)
   plot(fs)
   title([bird, ' Field Strength'])
              
end
   