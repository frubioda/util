;-----------------------------------------------------------------------------;
  function bcucof,y,y1,y2,y12,d1,d2
; --------------------------------------------------------------------------- ;
; CALCULATE BICUBIC COEFFICIENTS                                              ;
; REF: Press et al., Numerical Recipes in Fortran (2. ed), p. 119             ;
; vectorizable version                                                        ;
; --------------------------------------------------------------------------- ;
;  Translated to idl, 13.11.93 Torben Leifsen
;-----------------------------------------------------------------------------;
;
; Initialize:
;
  yz = size(y)
  ndep=yz(1)
  rc = dblarr(ndep,4,4)
  x  = dblarr(ndep,16)
  wt = dblarr(16,16)
  wt(*,0) =[ 1.,0.,-3., 2.,0.,0., 0., 0.,-3., 0., 9.,-6., 2., 0.,-6., 4.]
  wt(*,1) =[ 0.,0., 0., 0.,0.,0., 0., 0., 3., 0.,-9., 6.,-2., 0., 6.,-4.]
  wt(*,2) =[ 0.,0., 0., 0.,0.,0., 0., 0., 0., 0., 9.,-6., 0., 0.,-6., 4.]
  wt(*,3) =[ 0.,0., 3.,-2.,0.,0., 0., 0., 0., 0.,-9., 6., 0., 0., 6.,-4.]
  wt(*,4) =[ 0.,0., 0., 0.,1.,0.,-3., 2.,-2., 0., 6.,-4., 1., 0.,-3., 2.]
  wt(*,5) =[ 0.,0., 0., 0.,0.,0., 0., 0.,-1., 0., 3.,-2., 1., 0.,-3., 2.]
  wt(*,6) =[ 0.,0., 0., 0.,0.,0., 0., 0., 0., 0.,-3., 2., 0., 0., 3.,-2.]
  wt(*,7) =[ 0.,0., 0., 0.,0.,0., 3.,-2., 0., 0.,-6., 4., 0., 0., 3.,-2.]
  wt(*,8) =[ 0.,1.,-2., 1.,0.,0., 0., 0., 0.,-3., 6.,-3., 0., 2.,-4., 2.]
  wt(*,9) =[ 0.,0., 0., 0.,0.,0., 0., 0., 0., 3.,-6., 3., 0.,-2., 4.,-2.]
  wt(*,10)=[ 0.,0., 0., 0.,0.,0., 0., 0., 0., 0.,-3., 3., 0., 0., 2.,-2.]
  wt(*,11)=[ 0.,0.,-1., 1.,0.,0., 0., 0., 0., 0., 3.,-3., 0., 0.,-2., 2.]
  wt(*,12)=[ 0.,0., 0., 0.,0.,1.,-2., 1., 0.,-2., 4.,-2., 0., 1.,-2., 1.]
  wt(*,13)=[ 0.,0., 0., 0.,0.,0., 0., 0., 0.,-1., 2.,-1., 0., 1.,-2., 1.]
  wt(*,14)=[ 0.,0., 0., 0.,0.,0., 0., 0., 0., 0., 1.,-1., 0., 0.,-1., 1.]
  wt(*,15)=[ 0.,0., 0., 0.,0.,0.,-1., 1., 0., 0., 2.,-2., 0., 0.,-1., 1.]
;
; pack temporary vector x
;
  for i=0,3 do begin
    x(*,i)=y(*,i)
    x(*,i+4)=y1(*,i)*d1(*)
    x(*,i+8)=y2(*,i)*d2(*)
    x(*,i+12)=y12(*,i)*d1(*)*d2(*)
  endfor
;
; matrix multipy by the stored table
;
  for i=0,3 do begin
    for j=0,3 do begin
      rc(*,i,j)=0.
      l=i*4+j
      for m=0,15 do begin
        rc(*,i,j)=rc(*,i,j)+wt(l,m)*x(*,m)
      endfor
    endfor
  endfor
;
  return,rc
end

