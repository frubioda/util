; save t variables in t0 arrays

; cnewt variables

z1t0=z1t
vz1t0=vz1t
d1t0=d1t
tg1t0=tg1t
n1t0=n1t
ne1t0=ne1t
pg1t0=pg1t
fmjt0=fmjt
en1t0=en1t
if(n_elements(nco1t) ne 0) then nco1t0=nco1t
if(n_elements(zht) ne 0) then zht0=zht
if(n_elements(dzt) ne 0) then dzt0=dzt
coolt1t0=coolt1t
heat1t0=heat1t
if(n_elements(eion1t) ne 0) then eion1t0=eion1t
if(n_elements(outintt) ne 0) then outintt0=outintt
if(n_elements(coolt) ne 0) then cooltt0=coolt
if(n_elements(taut) ne 0) then taut0=taut

; ctimet variables

itimet0=itimet
timet0=timet
dtnt0=dtnt
dtnmt0=dtnmt
iitert0=iitert

; enrgr1 variables

if(n_elements(enit) ne 0) then begin
  enit0=enit
  ekit0=ekit
  emit0=emit
  egit0=egit
  entrpyt0=entrpyt
  bfent0=bfent
  bfekt0=bfekt
  bfegt0=bfegt
  bfput0=bfput
  bfvt0=bfvt
  bfct0=bfct
  bfmomt0=bfmomt
  bfmt0=bfmt
  bfen0t0=bfen0t
  bfek0t0=bfek0t
  bfeg0t0=bfeg0t
  bfpu0t0=bfpu0t
  bfv0t0=bfv0t
  bfc0t0=bfc0t
  wptt0=wptt
  wbtt0=wbtt
  wvtt0=wvtt
  heatnrt0=heatnrt
  heatrt0=heatrt
  wpi0t0=wpi0t
  wbi0t0=wbi0t
  wvi0t0=wvi0t
;
; nrgtr1 variables
;
  denmtt0=denmtt
  dethmtt0=dethmtt
  deiemtt0=deiemtt
  deimmtt0=deimmtt
  degmtt0=degmtt
  dekmtt0=dekmtt
  qrmtt0=qrmtt
  wpmtt0=wpmtt
  wbmtt0=wbmtt
  wvmtt0=wvmtt
  fpumtt0=fpumtt
  fvmtt0=fvmtt
  fcmtt0=fcmtt
  dfrmtt0=dfrmtt
  dfpumtt0=dfpumtt
  dfnrmtt0=dfnrmtt
  dfvmtt0=dfvmtt
  dfcmtt0=dfcmtt
  qintmtt0=qintmtt
  qkinmtt0=qkinmtt
  qtotmtt0=qtotmtt
endif  

; zipol variables

if(n_elements(tgzt) ne 0) then tgzt0=tgzt
if(n_elements(pgzt) ne 0) then pgzt0=pgzt
if(n_elements(dzt) ne 0) then dzt0=dzt
if(n_elements(vzzt) ne 0) then vzzt0=vzzt
if(n_elements(tgmt) ne 0) then tgmt0=tgmt
if(n_elements(pgmt) ne 0) then pgmt0=pgmt
if(n_elements(dmt) ne 0) then dmt0=dmt
if(n_elements(vzmt) ne 0) then vzmt0=vzmt
if(n_elements(zmm) ne 0) then zmm0=zmm
if(n_elements(amp_tg) ne 0) then amp_tg0=amp_tg
if(n_elements(amp_trad) ne 0) then amp_trad0=amp_trad
if(n_elements(amp_int) ne 0) then amp_int0=amp_int 
if(n_elements(tradt) ne 0) then tradt0=tradt
if(n_elements(intt) ne 0) then intt0=intt
if(n_elements(cmasslg) ne 0) then cmasslg0=cmasslg
if(n_elements(cmassht) ne 0) then cmassht0=cmassht


end
  