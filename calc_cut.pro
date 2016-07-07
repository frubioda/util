pro calc_cut,istep,cutp,cutf
;+
;   calc_cut,istep,cutp,cutf
;
;            calculates cutoff period and frequency at timestep istep
;
;-
@common_radyn
@common_movie

if(n_params(0) lt 3) then begin
  print,'calc_cut,istep,cutp,cutf'
  return
endif

gamma=5./3.
s=sqrt(gamma*pg1t(*,istep)/d1t(*,istep))
lnpg=alog(pg1t(*,istep))
hp=-(shift(z1t(*,istep),1)-z1t(*,istep))/(shift(lnpg,1)-lnpg)
cutp=4.*!pi*hp/s
cutf=1000./cutp

end
