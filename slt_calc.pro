pro slt_calc,slt
;+
;   slt_calc,slt
;
;            calculate line source function for CaII H from number densities
;
;-
@common_radyn
@common_movie

if(n_params() lt 1) then begin
  print,'slt_calc,slt'
  return
endif

aa=1.39706e8
blu=2.19956e10
bul=2.19956e10
j=3
i=0
slt=aa*n1t(*,j,1,*)/(blu*n1t(*,i,1,*)-bul*n1t(*,j,1,*))
slt=reform(slt)

end
