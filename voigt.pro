pro voigt,a,v,h
;+
;   voigt,a,v,h
;
;            calculates voigt function h
;            ref. hui, armstrong and wray, jqsrt 19, 509 (1977)
;            based on coding by a.nordlund/16-dec-82
;-
;
;  this vectorizable voigt function is based on the paper by
;  hui, armstrong and wray, jqsrt 19, 509 (1977).  it has been
;  checked against the old landolt & boernstein standard voigt.
;  errors become significant (at the < 1 % level) around the
;  "knee" between the doppler core and the damping wings for a
;  smaller than 1.e-3.  note that, as written here, the routine
;  has no provision for the trivial case a=0. the normalization
;  is such that the integral is sqrt(pi); i.e., a plain exp(-x**2)
;  should be used at a=0.  the new landolt & boernstein gives a
;  useful expression for the small but finite a case.
;
if (n_params(0) lt 3) then begin
  print,'voigt,a,v,h'
  return
endif

a0=122.607931777104326d0
a1=214.382388694706425d0
a2=181.928533092181549d0
a3=93.155580458138441d0
a4=30.180142196210589d0
a5=5.912626209773153d0
a6=0.564189583562615d0
b0=122.607931773875350d0
b1=352.730625110963558d0
b2=457.334478783897737d0
b3=348.703917719495792d0
b4=170.354001821091472d0
b5=53.992906912940207d0
b6=10.479857114260399d0

h=fltarr(n_elements(v))
if(n_elements(a) eq 1) then aa=(h+1.)*a else aa=a
iw=where(aa lt 500.,count)
if(count gt 0) then begin
  z=complex(a(iw),-abs(v(iw)))
  h(iw)=float( $
   ((((((a6*z+a5)*z+a4)*z+a3)*z+a2)*z+a1)*z+a0) $
   /(((((((z+b6)*z+b5)*z+b4)*z+b3)*z+b2)*z+b1)*z+b0) $
   )
endif

return
end
