pro cool_add_all,coolmt
;+
;
;   cool_add_all,coolmt
;
;            add up individual cooling as function of time
;
;-
@common_radyn
@common_movie

if(n_params(0) lt 1) then begin
  print,'cool_add_all,coolmt'
  return
endif

coolmt=coolt
for kr=0,nrad-1 do begin
  if(ielrad(kr) eq 1) or (cont(kr) eq 0) then begin
    print,'processing, kr=',kr
    cool_add,coolt(*,kr,*),dum
    coolmt(*,kr,*)=dum
  endif
endfor

return
end
