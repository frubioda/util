pro flux,lam,fluxt ,ihy
;
;   98-05-13 bill abbett
;     converts vacuum frequency to air wavelength in
;     Angstroms and calculates integrated continuum 
;     irradiance as a function of time
;    
;     lam: continuum wavelength returned in microns
;     fluxt: irradiance in ergs/cm^2/s 
;
@common_radyn.pro
@common_movie.pro
;
if(n_params(0) lt 2) then begin
  print,'trad_calc,lambda,fluxt ,ihy'
  return
endif
if(n_elements(ihy) eq 0) then ihy=0
;
ntime=n_elements(timet)
if (ihy eq 0) then begin
  iw=where((cont ne 0),ncont) 
endif else begin
  iw=where((cont ne 0) and (ielrad eq 1),ncont)
endelse
;
nqq=total(nq(iw))
lam=fltarr(nqq)
fluxt=fltarr(nqq,ntime)
;
nu0=0
for kt=0,ncont-1 do begin
  kr=iw(kt)
  lam(nu0:nu0+nq(kr)-1)=cc*1.e8/frq(1:nq(kr),kt)
  for i=nu0,nu0+nq(kr)-1 do begin
    if(lam(i) gt 2000.) then $ 
      lam(i)=lam(i)/( 1.0+2.735182E-4+131.4182/ $
      lam(i)/lam(i)+2.76249E8/lam(i)/lam(i)/lam(i)/lam(i) )
  endfor
  for it=0,ntime-1 do begin
    for mu=0,nmu-1 do begin
      fluxt(nu0:nu0+nq(kr)-1,it)=fluxt(nu0:nu0+nq(kr)-1,it)+$
      2.*pi*wmu(mu)*zmu(mu)*outintt(1:nq(kr),mu,kr,it)
    endfor
  endfor
  nu0=nu0+nq(kr)
endfor
;
iw=sort(lam)
lam=lam(iw)
fluxt=fluxt(iw,*)
;
lam=lam/1.e4
for it=0,ntime-1 do begin
  fluxt(*,it)=fluxt(*,it)*cc/lam(*)
endfor
;
return
end
