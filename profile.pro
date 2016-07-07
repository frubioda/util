pro profile,iline,ntime,nscale
;
;   05-09-97 bill abbett
;      plots line profiles
;      iline = line number, ntime = integer timestep
;
@common_radyn
@common_movie
@common_spectra
;
if(n_elements(iline) eq 0) then iline=2
if(n_elements(ntime) eq 0) then ntime=maxt
if(n_elements(nscale) eq 0) then nscale=0
;
 plot,xlamb(1+nscale:nq(iline)-(2+nscale),iline), $
  fluxt(1+nscale:nq(iline)-(2+nscale),iline,ntime), $
 xtitle='Wavelength from line center ('+string("305B)+')', $
 ytitle='Surface Flux (ergs cm!e-2!n s!e-1!n '+string( "305B)+' !e-1!n)', $
 title=+string(cnvl(iline),fo='(f10.4)')
;
@common_radyn
@common_movie
@common_spectra
;
end
