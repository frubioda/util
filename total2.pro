;***********************total2.pro
function total2, xx, ff
  ss=size(xx,/dimen)
  nn=(reverse(ss))[0]

  if n_elements(ss) eq 1 then begin
    tot=0.0
    for i=0, nn-2 do tot=tot+(ff[i+1]+ff[i])*(xx[i+1]-xx[i])/2
    return, tot
  endif

  if n_elements(ss) eq 2 then begin
    tot=0.0
    for i=0, nn-1 do tot=tot+ff[i]*(xx[1,i]-xx[0,i])
    return, tot
  endif
end
;****************************
