pro intrpl,nmbr,zmin,zmax,zvar,vart,mtmax
;
@common_radyn.pro
@common_movie.pro
@common_spectra.pro
@common_intrpl.pro
;
if(n_elements(mtmax) eq 0) then mtmax=maxt
;
zevn=fltarr(nmbr)
varzt=fltarr(nmbr,mtmax+1)
;
dzevn=(zmax-zmin)/nmbr
zevn(0)=zmin
;
for j=1,nmbr-1 do begin
  zevn(j)=zevn(j-1)+dzevn
endfor
;
for k=0,mtmax do begin
  for j=0,nmbr-1 do begin
    zipol1,zvar,vart,zevn(j),vzpt
    varzt(j,k)=vzpt(k)
  endfor
endfor
;
return
end
@common_intrpl.pro
end
