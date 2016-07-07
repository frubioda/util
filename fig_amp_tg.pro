pro fig_amp_tg,ps=ps
;+
;   fig_amp_tg,/ps
;
;            plot temperature amplitude on given cmass,z1,tau
;
;-
@common_radyn
@common_movie
common cdepth, tgzt,pgzt,ddzt,vzzt,tgmt,pgmt,dmt,vzmt,tgtt,pgtt,dtt,vztt,$
 cmasszt,tauzt,zmt,taumt,cmasstt,ztt,zmm,cmassmm,taumm



if(n_elements(ps) ne 0) then set_plot,'ps'
if(!d.name eq 'PS') then begin
  file='fig_amp_tg.ps'
  device,file=file,xsize=18,ysize=18,xoff=2,yoff=2
endif

cmasst0h=fltarr(ndep)               
cmasst0h(1:*)=0.5*(cmast0(1:*)+cmast0(0:ndep-2))
cmasst0h(0)=0.25*min(cmass1t(1,*))
cmasslg=alog10(cmasst0h)

cmassht=cmass1t
cmassht(2:*,*)=0.5*(cmass1t(2:*,*)+cmass1t(1:ndep-2,*))
cmassht(1,*)=0.5*cmass1t(1,*)
cmassht(0,*)=cmasst0h(0)
cmassht(ndep-1,*)=cmasst0h(ndep-1)


;zipol,alog10(cmassht),tg1t,alog10(cmassht(*,16)),tgmt
;zipol,zht,tg1t,zht(*,16),tgzt
;zipol,alog10(taut),tg1t,alog10(taut(*,16)),tgtt

trad_calc,lambda,tradt,intt
amp_calc,tradt,amp_trad,tgzt,amp_tgz,intt,amp_int
amp_calc,tradt,amp_trad,tgmt,amp_tgm,intt,amp_int
amp_calc,tradt,amp_trad,tgtt,amp_tgt,intt,amp_int

zkm=zht(*,16)*1.e-5
plot,zkm,amp_tgz,xran=[-100,200],yrange=[0,0.008],$
 xtitle='Height (km), t=160s',ytitle='!4D!3T/T',title=atmoid
oplot,zkm,amp_tgm,line=1
oplot,zkm,amp_tgt,line=2
label,0.75,0.9,0,'Fixed z'
label,0.75,0.85,2,'Fixed !4s!3!d500!n'
label,0.75,0.80,1,'Fixed m!dc!n'

tlg=alog10(taut(*,16))
yp=fltarr(5)
y0=0.0028
dy=0.0002
for i=0,4 do begin
  tlg0=-1.5+i*0.5
  intep,tlg,zkm,tlg0,dum
  yp(i)=dum
  oplot,[1,1]*yp(i),[y0,y0-dy]
  xyouts,yp(i),y0-2*dy,string(tlg0,format='(f4.1)'),align=0.5
endfor
oplot,[yp(0),yp(4)],[1,1]*y0
xyouts,-40,y0-1.5*dy,'lg !4s!3!d500!n'

tlg=alog10(cmassht(*,16))
yp=fltarr(5)
y0=0.0022
dy=0.0002
for i=0,4 do begin
  tlg0=i*0.2
  intep,tlg,zkm,tlg0,dum
  yp(i)=dum
  oplot,[1,1]*yp(i),[y0,y0-dy]
  xyouts,yp(i),y0-2*dy,string(tlg0,format='(f4.1)'),align=0.5
endfor
oplot,[yp(0),yp(4)],[1,1]*y0
xyouts,-40,y0-1.5*dy,'lg m!dc!n'


if(!d.name eq 'PS') then begin
  print,'figure file is: ',file
  device,/close
  set_plot,'x'
endif

end
