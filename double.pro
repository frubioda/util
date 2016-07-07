pro double,kr,y,xx,yy; makes profiles symmetric around x(0)
;+
;   double,kr,y,xx,yy
;
;            makes profile symmetric around x(0). used for fluxes and
;            intensities.
;            typical call sequence:
;            double,0,flux,xx,yy
;            plot,xx,yy
;
;            for two-sided profiles, original profile is returned
;
;-
@common_radyn 

if n_params(0) eq 0 then begin
  print,'double,kr,y,xx,yy'
  return
endif

ndum=size(y)
if ndum(0) eq 2 then begin  ; two dimensional y array: flux values in y
  y2=y(1:nq(kr),kr)
endif else begin
  y2=y(1:nq(kr),nmu-1,kr)
endelse
qn=qnorm*1.e5/cc

if(ind(kr) eq 2) then begin
  yy=y2
  qx=q(0:nq(kr)-1,kr)                ; extract q
endif else begin    
  yy=fltarr(nq(kr)*2-1)                ; arrays for symmetrizized variables
  xx=fltarr(nq(kr)*2-1)
  qx=fltarr(nq(kr)*2-1)
  q2=q(0:nq(kr)-1,kr)                  ; extract q
  qx(nq(kr)-1)=q2
  qx(0)=-reverse(q2)                   ; make symmetric q
  yy(nq(kr)-1)=y2
  yy(0)=reverse(y2)
endelse
 
xx=-alamb(kr)*qx*qn/(1.0+qx*qn)

end
