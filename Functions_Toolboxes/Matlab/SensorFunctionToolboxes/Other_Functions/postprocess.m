function postprocess(filename,width,height,fontsize)
% Load file
[pathstr, name, ext] = fileparts(filename);
open(filename)
% Change size
set(gcf,'paperunits','centimeters')
set(gcf, 'PaperPositionMode', 'manual');
set(gcf,'papersize',[width,height])
set(gcf,'paperposition',[0,0,width,height])
set(gcf, 'renderer', 'painters');
% Change font
set(findall(gcf,'-property','FontSize'),'FontSize',fontsize)
set(findall(gcf,'-property','FontName'),'FontName','Times New Roman')
% Export file
print('-depsc2',name)
end