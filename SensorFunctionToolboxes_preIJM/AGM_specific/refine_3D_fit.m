 function [gain,rotM]=refine_3D_fit(gain,rotM)
 % largest element should be on diagonal
 m=0; rm=0; cm=0;
 for r=1:3, for c=1:3,
 if abs(rotM(r,c))>m, m=abs(rotM(r,c)); rm=r; cm=c; end; % record max
 end; end;
 if rm~=cm, % swap cols if not on diagonal
 t=rotM(:,cm); rotM(:,cm)=rotM(:,rm); rotM(:,rm)=t;
 t=gain(cm); gain(cm)=gain(rm); gain(rm)=t;
 end; % largest now in the diagonal, in row rm

 % do the same on remaining 2x2 matrix
 switch rm, case 1, i=[2 3]; case 2, i=[1 3]; case 3, i=[1 2]; end;
 m=0; rm=0; cm=0;
 for r=1:2, for c=1:2,
 if abs(rotM(i(r),i(c)))>m, m=abs(rotM(i(r),i(c))); rm=i(r); cm=i(c); end;
 end; end;
 if rm~=cm, % swap cols if not on diagonal
 t=rotM(:,cm); rotM(:,cm)=rotM(:,rm); rotM(:,rm)=t;
 t=gain(cm); gain(cm)=gain(rm); gain(rm)=t;
 end;

 % neg cols to make it positive along diagonal
 if rotM(1,1)<0, rotM(:,1)=-rotM(:,1); end;
if rotM(2,2)<0, rotM(:,2)=-rotM(:,2); end;
 if rotM(3,3)<0, rotM(:,3)=-rotM(:,3); end;
end