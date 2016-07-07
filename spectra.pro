pro spectra,clamb,cflxt
;
;   97-05-09 bill abbett
;     initializes variables used in spectral analysis
;
@common_radyn.pro
@common_movie.pro
;@common_spectra.pro
;
mcnt=200
maxt=n_elements(timet)-1
convl=dblarr(mrad)
flux=dblarr(mq,mrad)
cflux=dblarr(mq,mrad)
xlamb=dblarr(mq,mrad)
clvac=dblarr(mq,mrad)
clsrf=dblarr(mq,mrad)
fluxt=dblarr(mq,mrad,maxt+1)
cfluxt=dblarr(mq,mrad,maxt+1)
cflux1t=dblarr(mq*mrad,maxt+1)
nradm=nrad-1
nmum=nmu-1
nqm=mq-1
ncntm=mcnt-1
;
;   compute flux (cgs units)
;
for mu=0,nmum do begin
  xmy=zmu(mu)
  for kr=0,nradm do begin
    for ny=0,nq(kr)-1 do begin
      flux(ny,kr)=flux(ny,kr)+wmu(mu)*xmy*outint(ny,mu,kr)
      flux(ny,kr)=2.*pi*flux(ny,kr)
    endfor
  endfor
endfor
for it=0,maxt do begin
  for mu=0,nmum do begin
    xmy=zmu(mu)
    for kr=0,nradm do begin
      for ny=0,nq(kr)-1 do begin
        fluxt(ny,kr,it)=fluxt(ny,kr,it)+wmu(mu)*xmy*outintt(ny,mu,kr,it)
        fluxt(ny,kr,it)=2.*pi*fluxt(ny,kr,it)
      endfor
    endfor
  endfor
endfor
flux=flux*1.e5
fluxt=fluxt*1.e5
;
;   specify continuum wavelengths in angstroms 
;   and fluxes in cgs
;
for kr=0,nradm do begin
  krc=ktrans(kr)-1
  if(cont(kr)) then begin
    clvac(0:nq(kr),krc)=cc/frq(0:nq(kr),krc)*1.e8
    cflux(0:nq(kr),krc)=flux(0:nq(kr),kr)
  endif
endfor
for it=0,maxt do begin
  for kr=0,nradm do begin
    krc=ktrans(kr)-1
    if(cont(kr)) then begin
      cfluxt(0:nq(kr),krc,it)=fluxt(0:nq(kr),kr,it)
    endif
  endfor
endfor
;
;   convert vacuum wavelengths to air
;
for kr=0,nradm do begin
  if(alamb(kr) lt 2000.) then begin
    convl(kr)=alamb(kr)
  endif else begin
    convl(kr)=alamb(kr)/( 1.0+2.735182E-4+131.4182/alamb(kr)/alamb(kr) $
      +2.76249E8/alamb(kr)/alamb(kr)/alamb(kr)/alamb(kr) )
  endelse
  krc=ktrans(kr)-1
  if(cont(kr)) then begin
    for i=0,nq(kr) do begin
    if(clvac(i,krc) lt 2000.) then begin
      clsrf(i,krc)=clvac(i,krc)
    endif else begin  
      clsrf(i,krc)=clvac(i,krc)/( 1.0+2.735182E-4+131.4182/ $
        clvac(i,krc)/clvac(i,krc)+2.76249E8/ $
        clvac(i,krc)/clvac(i,krc)/clvac(i,krc)/ $
        clvac(i,krc) )
    endelse
    endfor
  endif
endfor
;
;   sort the continuum wavelengths
;
j=0
iw=sort(clvac)
clvac=clvac(iw)
clsrf=clsrf(iw)
cflux=cflux(iw)
for it=0,maxt do begin
  tmp=cfluxt(*,*,it)
  tmp=tmp(iw)
  for i=0,(mq*mrad)-1 do begin
    cflux1t(i,it)=tmp(i)
  endfor
endfor
for i=0,(mq*mrad)-1 do begin
  if(clsrf(i) ne 0.) then begin
    j=j+1
  endif
endfor
cltmp=fltarr(j)
cftmp=fltarr(j)
cftmpt=fltarr(j,maxt+1)
j=0
for i=0,(mq*mrad)-1 do begin
  if(clsrf(i) ne 0.) then begin
    cltmp(j)=clsrf(i)
    cftmp(j)=cflux(i)
    for it=0,maxt do begin
      cftmpt(j,it)=cflux1t(i,it)
    endfor
    j=j+1
  endif
endfor
k=0
for i=0,j-1 do begin
  if(cftmp(i) ne 0.) then begin
    k=k+1
  endif
endfor
clamb=fltarr(k)
cflx=fltarr(k)
cflxt=fltarr(k,maxt+1)
icnt=0
for i=0,j-1 do begin
  if(cftmp(i) ne 0.) then begin
    clamb(icnt)=cltmp(i)
    cflx(icnt)=cftmp(i)
    for it=0,maxt do begin
      cflxt(icnt,it)=cftmpt(i,it)
    endfor
    icnt=icnt+1
  endif
endfor
;
;   compute frequency from line center
;
const=1.e5*qnorm/cc
for n=0,nqm do begin
  for kr=0,nradm do begin
    xlamb(n,kr)=-q(n,kr)*const*convl(kr)
  endfor
endfor
;
plot,clamb,cflxt ;added by fatima, study the code!

@common_radyn.pro
@common_movie.pro
;@common_spectra.pro
;
return
end
