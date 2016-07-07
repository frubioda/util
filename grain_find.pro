pro grain_find,indx,igraint,threshold=threshold
;+
;   grain_find,indx,igraint
;
;            parametrizes grain brightness by intensity contrast
;            time index vector for max grain brightness is indx
;-
@common_movie

if(n_params() lt 2) then begin
  print,'grain_find,indx,igraint'
  return
endif

if(n_elements(outmu1t) le 1) then return

ntime=n_elements(outmu1t(0,0,*))
igraint=fltarr(ntime)
kr=16  ; CaII H line
for i=0,ntime-1 do begin
  y=outmu1t(50:60,kr,i)/outmu1t(1,kr,i)
  igraint(i)=max(y)-y(10)
  igraint(i)=igraint(i) > 0.0
endfor
igraint=igraint/max(igraint)

if(n_elements(threshold) eq 0) then threshold=0.01
iw1=where(igraint gt threshold,count)
if(count ne 0) then begin
  indx=intarr(ntime)
  i0=0
  maxi=igraint(0)
  indx(i0)=0
  for i=1,ntime-1 do begin
    if(igraint(i) gt threshold) then begin
      if(igraint(i) gt maxi) then begin
	maxi=igraint(i)
	indx(i0)=i
      endif
    endif else if(igraint(i-1) gt threshold) then begin
      i0=i0+1
      maxi=0.0
    endif
  endfor
  if(maxi gt 0.0) then indx=indx(0:i0) else indx=indx(0:i0-1)
endif

end
