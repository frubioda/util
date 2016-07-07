pro it_norm,timet,outmu1t,fluxt,inorm,itime0,itime1,ymin,ymax
;
;   plots normalized intensity as a function of time
;   for H_alpha line wings (+-3 Angstroms), and 5000
;   Angstrom continuum
;
;   itime0 and itime1 define the time interval
;   (eg. itime=500,1000,1204,2100,2220 correspond to
;   1,2,4,60,and 70 seconds respectively for fd700.idlsave) 
;
;   bill abbett 05-17-98
;   requires idlsave variables timet and outmu1t
;
if(n_params() lt 8) then begin
  print,'it_norm,timet,outmu1t,fluxt,inorm,itime0,itime1,ymin,ymax
  return
endif
;
itime=n_elements(timet)
iqplus=37
iqminus=13
ihalpha=2
ilamb=192
tmax=timet(itime1)
tmin=timet(itime0)
iflux=169  
lambda=5128.57
flxnrm=fltarr(itime)
hplus=fltarr(itime)
hminus=fltarr(itime)
y0=fltarr(itime)
;
flxpre=fluxt(iflux,0)
flxmax=max(fluxt(iflux,0:inorm))
flxmin=min(fluxt(iflux,0:inorm)) 
flxnrm(0)=0.
if (abs(flxmin-flxpre) lt abs(flxmax-flxpre)) then begin
  flxnrm(1:*)=(fluxt(iflux,1:*)-flxpre)/(flxmax-flxpre)
endif else begin
  flxnrm(1:*)=(fluxt(iflux,1:*)-flxpre)/(flxpre-flxmin)
endelse
;
outpre=outmu1t(iqplus,ihalpha,0)
outmax=max(outmu1t(iqplus,ihalpha,0:inorm))
hplus(0)=0.
hplus(1:*)=(outmu1t(iqplus,ihalpha,1:*)-outpre)/(outmax-outpre)
;
outpre=outmu1t(iqminus,ihalpha,0)
outmax=max(outmu1t(iqminus,ihalpha,0:inorm))
hminus(0)=0.
hminus(1:*)=(outmu1t(iqminus,ihalpha,1:*)-outpre)/(outmax-outpre)
;
y0(*)=0.
;
plot,timet,flxnrm,lin=0,thick=2,ystyle=1,ytitle='!5Normalized Intensity',$
  xtitle='!5Time [sec]',xrange=[tmin,tmax],yr=[ymin,ymax]
oplot,timet,hplus,lin=2,thick=2
oplot,timet,hminus,lin=3,thick=2
oplot,timet,y0,lin=1
;
end
