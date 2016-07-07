pro calc_bw,bw
;+
;   calc_bw,bw
;
;            calculate Brundt-Waisala frequency
;
;-
@common_radyn
@common_movie

gamma=5./3.
s=sqrt(gamma*pg1t/d1t)
dtdz=(shift(tg1t,1)-tg1t)/(shift(z1t,1)-z1t)
ohm2=(gamma-1.)*(grav/s)^2 + grav/tg1t*dtdz
bw=sqrt(ohm2)/2./!pi

end
