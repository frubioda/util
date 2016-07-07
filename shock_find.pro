pro shock_find,zshock,vmin,vmax,tmin,tmax
;+
;   shock_find,zshock,vmin,vmax,tmin,tmax
;
;            finds lowest shock and extracts position, velocities and temp
;
;-
@common_movie

if(n_params() lt 5) then begin
  print,'shock_find,zshock,vmin,vmax,tmin,tmax'
  return
endif

ntime=n_elements(z1t(0,*))
ndep=n_elements(z1t(*,0))
zshock=fltarr(ntime)
vmin=fltarr(ntime)
vmax=fltarr(ntime)
tmin=fltarr(ntime)
tmax=fltarr(ntime)
for i=0,ntime-1 do begin
  dvdz=(vz1t(1:ndep-1,i)-vz1t(0:ndep-2,i))/(z1t(1:ndep-1,i)-z1t(0:ndep-2,i))
  iw=where(dvdz lt -0.1,count) ; find large v gradient
  if(count gt 0) then begin
    k2=iw(count-1)
    iw2=where(dvdz(0:k2) ge -0.1,count2)
    if(count2 gt 0) then k1=iw2(count2-1)+1 else k1=0
    dum=min(dvdz(k1:k2))  ; largest gradient in v
    k=k1+!c                  ; index for largest gradient
    zshock(i)=z1t(k,i)
    iw=where((z1t(*,i) gt zshock(i)-50.e5) and (z1t(*,i) lt zshock(i)+50.e5))
    vmin(i)=min(vz1t(iw,i))
    vmax(i)=max(vz1t(iw,i))
    tmin(i)=min(tg1t(iw,i))
    tmax(i)=max(tg1t(iw,i))
  endif
endfor

end

    
