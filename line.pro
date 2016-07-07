pro line,istep,kr,deltax
;
;    plots line profiles 
;
@common_radyn.pro
@common_movie.pro
;
if(n_elements(istep) eq 0) then istep=0
if(n_elements(kr) eq 0) then kr=32
if(n_elements(deltax) eq 0) then deltax=101
;
plot,q(*,kr)*qnorm,outmu1t(1:nq(kr),kr,istep)/outmu1t(1,kr,istep),$
xr=[-deltax,deltax]
;
end
