pro fig_dtdm ,ps=ps
;+
;    fig_dtdm,/ps
;
;            plots critical temperature gradient for opacity effect
;
;-
@common_radyn
@common_movie
common cdepth, tgzt,pgzt,ddzt,vzzt,tgmt,pgmt,dmt,vzmt,tgtt,pgtt,dtt,vztt,$
 cmasszt,tauzt,zmt,taumt,cmasstt,ztt,zmm,cmassmm,taumm
common cdtrad,kappamt

if(n_elements(ps) ne 0) then set_plot,'ps'
if(!d.name eq 'PS') then begin
  file='fig_dtdm.ps'
  device,file=file,xsize=18,ysize=18,xoff=2,yoff=2
endif

cmass_calc,cmasst0h,cmassht
dtdm=fltarr(ndep)                ; derivative of T with respect to cmass
dtdm(1:ndep-2)=(tg1t(0:ndep-3,0)-tg1t(2:ndep-1,0))/$
 (cmassht(0:ndep-3,0)-cmassht(2:ndep-1,0))
dtdm(0)=dtdm(1)
dtdm(ndep-1)=dtdm(ndep-2)

plot,zmt(*,0)*1.e-5,dtdm,xrange=[-100,200],$
xtitle='Height (km), t=0',ytitle='dT/dm',$
title=title

gam=5./3.
mcorr=1.-exp(-10.^cmassmm)
dtdm_crit=tgmt(*,0)/mcorr/(-0.75+0.5/(gam-1.)+70160./tgmt(*,0))
oplot,zmt(*,0)*1.e-5,dtdm_crit,lin=3
for i=11,15 do begin
  gam=i/10.
  dtdm_crit=tgmt(*,0)/mcorr/(-0.75+0.5/(gam-1)+70160./tgmt(*,0))
  oplot,zmt(*,0)*1.e-5,dtdm_crit,lin=1
endfor

x0=0.05
y0=0.40
dy=0.04
label,x0,y0     ,0,'dT/dm in atmosphere'
label,x0,y0-1*dy,1,'dT/dm!dcritical!n !4c!3=1.1 (lowest), 1.2, 1.3, 1.4, 1.5'
label,x0,y0-2*dy,3,'dT/dm!dcritical!n !4c!3=5/3'


if(!d.name eq 'PS') then begin
  print,'figure file is: ',file
  device,/close
  set_plot,'x'
endif

end
;