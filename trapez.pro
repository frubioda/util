function trapez,x0,y0
;+
;   trapez(x,y)
;
;            performs trapezoidal integration.
;
;-
if(n_params(0) lt 2) then begin
  print,'trapez(x,y)'
  return,0
endif

nx=n_elements(x0)
ny=n_elements(y0)
if(nx ne ny) then begin
  print,'trapez: different number of elements in x and y array'
  return,0
endif
x=reform(x0)
y=reform(y0)
integrand=(y(0:ny-2)+y(1:ny-1))*(x(1:nx-1)-x(0:nx-2))*0.5
return,total(integrand)
 
end
