pro fig_models_oslo,ps=ps,frame=frame
;+
;   fig_models_oslo,/ps,/frame
;
;            show models 110, 157 and 158
;
;-
@common_radyn

if(n_elements(ps) ne 0) then set_plot,'ps'
if(!d.name eq 'PS') then begin
  file='fig_models_oslo.ps'
  device,file=file,xsize=8.4,ysize=8,yoff=2
endif

!p.charsize=0.8

read0,'idl0.110'
read1,'idl001.110' & idl1close
plot,z1*1.e-8,tg1,xran=[-0.5,2.5],yran=[3000,9000],xstyle=1,$
 xtitle='Height [Mm]',ytitle='Temperature',$
 xmargin=[8.4,0],ymargin=[3.3,1]
read0,'idl0.157'
read1,'idl001.157' & idl1close
oplot,z1*1.e-8,tg1,line=2
read0,'idl0.158'
read1,'idl001.158' & idl1close
xyouts,1.8,7000,'VAL3C',size=0.8
xyouts,1.8,5800,"VAL3A'",size=0.8
xyouts,2.45,3400,'Radiative equilibrium',size=0.8,align=1
oplot,z1*1.e-8,tg1,line=1

!p.charsize=1.0
if(n_elements(frame) ne 0) then frame

if(!d.name eq 'PS') then begin
  print,'figure file is:',file
  device,/close
  set_plot,'x'
endif

end
