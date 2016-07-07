pro cont,ntime,xlow,xup
;
;   05-20-97 bill abbett
;      plots continuum profile
;      xlow=lower xrange boundary
;      xup =upper xrange boundary
;
@common_radyn
@common_movie
@common_spectra
;
;if(n_elements(xlow) eq 0) then xlow=clamb(0)
;if(n_elements(xup) eq 0) then xup=clamb(icnt-1)
if(n_elements(xlow) eq 0) then xlow=100.
if(n_elements(xup) eq 0) then xup=9000.
if(n_elements(ntime) eq 0) then ntime=0
;
plot,clamb,cflxt(*,ntime),xr=[xlow,xup], $
 xtitle='Wavelength ('+string("305B)+')', $
 ytitle='Flux (ergs cm!e-2!n s!e-1!n '+string( "305B)+' !e-1!n)', $
 title='Continuum Flux'
;
@common_radyn
@common_movie
@common_spectra
;
end
