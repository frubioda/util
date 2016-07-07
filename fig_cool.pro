pro fig_cool,cooltmt,coolmt ,ps=ps,t1=t1,t2=t2
;+
;   fig_cool,cooltmt,coolmt .ps=ps,t1=t1,t2=t2
;
;            plot cooling and its components
;
;-
@common_radyn
@common_movie

if(n_params(0) lt 2) then begin
  print,'fig_cool,cooltmt,coolmt ,t1=t1,t2=t2'
  return
endif

if(n_elements(ps) ne 0) then set_plot,'ps'
if(!d.name eq 'PS') then begin
  file='fig_cool.ps'
  device,file=file,xsize=18,ysize=18,xoff=2,yoff=2
endif

ntime=n_elements(timet)
if(n_elements(t1) eq 0) then t1=60
if(n_elements(t2) eq 0) then t2=ntime-1
if(n_elements(cooltmt) eq 0) then cool_add,coolt1t,cooltmt
if(n_elements(coolmt) eq 0) then begin
  iw=where((ielrad eq 1) or (cont eq 0),count)
  ntrans=max(iw)+1
  coolmt=fltarr(ndep,ntrans,ntime)
  for kr=0,nrad-1 do begin
    if(ielrad(kr) eq 1) or (cont(kr) eq 0) then begin
      cool_add,coolt(*,kr,*),dum
      coolmt(*,kr,*)=dum
    endif
  endfor
endif

;xx=z1t(*,0)*1.e-8
xx=alog10(cmass1t(*,0))
ymax=alog10(max(abs((qrmtt(*,t2)-qrmtt(*,t1))*d1t(*,0)/(timet(t2)-timet(t1)))))
ymax=fix(ymax)
ymin=ymax-5
yprange=[ymin,ymax]
ymrange=yprange
plot_log,xx,(cooltmt(*,t2)-cooltmt(*,t1))*d1t(*,0)/(timet(t2)-timet(t1)),$
 yprange=yprange,ymrange=ymrange,xrange=[-8,2],$
 xtitle='lg column mass',yptitle='lg Cooling',ymtitle='lg Heating'

hlines=fltarr(ndep)
for kr=0,nrad-1 do begin
  if(ielrad(kr) eq 1) and (cont(kr) eq 0) then begin
    hlines=hlines+(coolmt(*,kr,t2)-coolmt(*,kr,t1))
  endif
endfor
hlines=hlines*d1t(*,0)/(timet(t2)-timet(t1))

hcont=fltarr(ndep)
for kr=0,nrad-1 do begin
  if(ielrad(kr) eq 1) and (cont(kr) eq -1) then begin
    hcont=hcont+(coolmt(*,kr,t2)-coolmt(*,kr,t1))
  endif
endfor
hcont=hcont*d1t(*,0)/(timet(t2)-timet(t1))

if(nel eq 2) then begin
  calines=fltarr(ndep)
  for kr=0,nrad-1 do begin
    if(ielrad(kr) eq 2) and (cont(kr) eq 0) then begin
      calines=calines+(coolmt(*,kr,t2)-coolmt(*,kr,t1))
    endif
  endfor
  calines=calines*d1t(*,0)/(timet(t2)-timet(t1))
endif

;!p.linestyle=1
;plot_log,xx,hlines,$
; yprange=yprange,ymrange=ymrange,xrange=[-8,2],/oplot
;!p.linestyle=!p.linestyle+1
;plot_log,xx,hcont,$
; yprange=yprange,ymrange=ymrange,xrange=[-8,2],/oplot

!p.linestyle=!p.linestyle+1
plot_log,xx,hcont+hlines,$
 yprange=yprange,ymrange=ymrange,xrange=[-8,2],/oplot

!p.linestyle=!p.linestyle+1
plot_log,xx,calines,$
 yprange=yprange,ymrange=ymrange,xrange=[-8,2],/oplot

!p.noclip=1
!p.linestyle=0
label,0.15,1.9,0,'Total'
label,0.15,1.8,1,'Hydrogen + background'
label,0.15,1.7,2,'Calcium'

!p.noclip=0

;xl=cc*1.e8/frq

if(!d.name eq 'PS') then begin
  print,'figure file is: ',file
  device,/close
  set_plot,'x'
endif



return
end





