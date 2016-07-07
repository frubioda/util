pro fig_dtrad ,k=k,ff=ff,ps=ps
;+
;   fig_dtrad ,k=k,ff=ff,/ps
;
;            plots delta(Trad(500)) and compares with 
;            delta(T)|cmasst0 and dtdcmass*dcmass
;            ff is fudge-factor in approximate formula for dkappa/kappa
;            to compensate for the fact that not all electrons come
;            from hydrogen. ff=1.1 at tau=1 and goes towards 1.0 deeper in
;
;-
@common_radyn
@common_movie
common cdepth, tgzt,pgzt,ddzt,vzzt,tgmt,pgmt,dmt,vzmt,tgtt,pgtt,dtt,vztt,$
 cmasszt,tauzt,zmt,taumt,cmasstt,ztt,zmm,cmassmm,taumm
common cdtrad,kappamt

if(n_elements(ps) ne 0) then set_plot,'ps'
if(!d.name eq 'PS') then begin
  file='fig_dtrad.ps'
  device,file=file,xsize=18,ysize=18,xoff=2,yoff=2
endif


cmass_calc,cmasst0h,cmassht
trad_calc,lambda,tradt
if(n_elements(kappamt) eq 0) then begin
  zipol,alog10(cmassht),xnormt,cmassmm,xnormmt
  kappamt=xnormmt/dmt
endif
if(n_elements(k) eq 0) then dum=min(abs(taumm),k)
if(n_elements(ff) eq 0) then ff=1.0

dtdm=fltarr(ndep)                ; derivative of T with respect to cmass
dtdm(1:ndep-2)=(tg1t(0:ndep-3,0)-tg1t(2:ndep-1,0))/$
 (cmassht(0:ndep-3,0)-cmassht(2:ndep-1,0))
dtdm(0)=dtdm(1)
dtdm(ndep-1)=dtdm(ndep-2)

dt1=(tgmt(k,*)-tgmt(k,0))/tgmt(k,0)
dt2=dtdm(k)*(cmasstt(k,*)-cmasstt(k,0))/tgmt(k,0)
dtr=dt1+dt2
dtrad=(tradt(8,*)-tradt(8,0))/tradt(8,0)
ymax=max(abs([dt1,dt2,dtr,dtrad]))

!p.multi=[0,0,2]
plot,timet,dt1,line=2,$
 xtitle='time [s]',ytitle='!4D!3T/T',$
 title=strtrim(title,2)+string(k,format="('  k=',i3)"),$
 yrange=[-ymax,ymax*1.3]
oplot,timet,dt2,line=1
oplot,timet,dtrad
oplot,timet,dtr,line=3

x0=0.05
y0=0.95
dy=0.04
label,x0,y0     ,2,'!4D!3T/T at fixed m!dc!n'
label,x0,y0-1*dy,1,'dT/dm!dc!n*!4D!3m!dc!n(!4s!3=1)'
label,x0,y0-2*dy,3,'Sum of above'
label,x0,y0-3*dy,0,'!4D!3T!dRAD!n/T!dRAD!n'

dkappa0=(kappamt(k,*)-kappamt(k,0))/kappamt(k,0)
dkappa1=sqrt(dmt(k,*)/dmt(k,0))*(tgmt(k,*)/tgmt(k,0))^(70160/tgmt(k,0)-0.75)-1.
dkappa=0.5*(dmt(k,*)-dmt(k,0))/dmt(k,0)+ $
 (-0.75+70160/tgmt(k,0))*(tgmt(k,*)-tgmt(k,0))/tgmt(k,0)/ff
dcmass=-dkappa*(1.-exp(-10.^cmassmm(k)))
plot,timet,dtrad,line=0,$
 xtitle='time [s]',ytitle='!4D!3T/T',$
 title=strtrim(title,2)+string(k,format="('  k=',i3)"),$
 yrange=[-ymax,ymax*1.3]
oplot,timet,dt1+dtdm(k)*dcmass/tgmt(k,0),line=3

label,x0,y0-2*dy,3,'dT/dm!dc!n*!4D!3m!dc!n(!4s!3=1) from appoximate formula'
label,x0,y0-3*dy,0,'!4D!3T!dRAD!n/T!dRAD!n'

reset

if(!d.name eq 'PS') then begin
  print,'figure file is: ',file
  device,/close
  set_plot,'x'
endif

end
