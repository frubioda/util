pro cool_add,cool1t,coolmt
;+
;   cool_add,cool1t,coolmt
;
;-
@common_radyn
@common_movie

if(n_params(0) lt 2) then begin
  print,'cool_add,cool1t,coolmt'
  return
endif

y1t=reform(cool1t)/d1t                    ; make coolt 2D in per gram

cons_ipol,y1t,y2t

; integrate over time

ntime=n_elements(timet)
coolmt=fltarr(ndep,ntime)

for i=1,ntime-1 do begin
  coolmt(*,i)=coolmt(*,i-1)+(timet(i)-timet(i-1))*0.5*(y2t(*,i)+y2t(*,i-1))
endfor

return
end
