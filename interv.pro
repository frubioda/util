pro interv, xt, lxt, x, left, mflag
;
; as interv.f
;
if(n_params(0) lt 5) then begin
  print,'interv,xt,lxt,x,left,mflag'
  return
endif

if(x lt xt(0)) then begin
  left=0
  mflag=-1
  return
endif else if(x ge xt(lxt-1)) then begin
  left=lxt-1
  mflag=1
  return
endif

left=max(where(xt(0:lxt-1) le x))
mflag=0

return
end

