             
function my_spherical_plot(chunk,Mf,c1,c2,c3)

subplot(1,2,1)
   plot(chunk(:,1),chunk(:,2),'color',c1,'Marker','.','LineStyle','none')
   hold on;
   plot(chunk(:,3),chunk(:,2),'color',c2,'Marker','.','LineStyle','none')
   hold on;
   plot(chunk(:,1),chunk(:,3),'color',c3,'Marker','.','LineStyle','none')
            
subplot(1,2,2)
    plot(Mf(:,1),Mf(:,2),'color',c1,'Marker','.','LineStyle','none')
    hold on;
    plot(Mf(:,3),Mf(:,2),'color',c2,'Marker','.','LineStyle','none')
    hold on;
    plot(Mf(:,1),Mf(:,3),'color',c3,'Marker','.','LineStyle','none')
              
end
   