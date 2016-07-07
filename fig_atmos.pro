pro fig_atmos,ext ,s,hp,cutf,_extra=e,ps=ps
;+
;   fig_atmos,ext ,s,hp,cutf,/ps
;
;            plot initial atmosphere
;
;-
@common_radyn

if(n_params(0) lt 1) then begin
  print,'fig_atmos,ext ,s,hp,cutf,/ps'
  return
endif

if(n_elements(ps) ne 0) then set_plot,'ps'

if(!d.name eq 'PS') then begin
  file='fig_atmos1_'+ext+'.ps'
  device,file=file,xsize=18,ysize=22,xoff=2,yoff=2
endif

read0,'idl0.'+ext
read1,'idl001.'+ext

cmasst0h=fltarr(ndep)               
cmasst0h(1:*)=0.5*(cmass1(1:*)+cmass1(0:ndep-2))
cmasst0h(0)=0.25*cmass1(1)
cmasslg=alog10(cmasst0h)

!p.multi=[0,2,2]

csize=0.8
zz=z1*1.e-8
plot,zz,tg1,yran=[3000,10000],ystyle=1,$
 title='Run '+ext,xtitle='Height [Mm]',$
 ytitle='Temperature [K]',_extra=e
scale2,zz,cmasslg,0.1,1,csize
x0=!cxmin+0.73*(!cxmax-!cxmin)
y0=!cymin+0.12*(!cymax-!cymin)
xyouts,x0,y0,'lg m!dc!n',size=csize
s=sqrt(gamma*pg1/d1)
lnpg=alog(pg1)
hp=-(shift(z1,1)-z1)/(shift(lnpg,1)-lnpg)
cutp=4.*!pi*hp/s
cutf=1000./cutp


plot,zz,s*1.e-5,$
 title='Run '+ext,xtitle='Height [Mm]',$
 ytitle='Sound speed [km/s]',_extra=e
scale2,zz,cmasslg,0.1,1,csize
x0=!cxmin+0.73*(!cxmax-!cxmin)
y0=!cymin+0.12*(!cymax-!cymin)
xyouts,x0,y0,'lg m!dc!n',size=csize

plot,zz(3:*),hp(3:*)*1.e-5,$
 title='Run '+ext,xtitle='Height [Mm]',$
 ytitle='Pressure scale height [km]',_extra=e
scale2,zz,cmasslg,0.1,1,csize
x0=!cxmin+0.73*(!cxmax-!cxmin)
y0=!cymin+0.12*(!cymax-!cymin)
xyouts,x0,y0,'lg m!dc!n',size=csize

plot,zz(3:*),cutf(3:*),$
 title='Run '+ext,xtitle='Height [Mm]',$
 ytitle='Cut off frequency [mHz]',_extra=e
scale2,zz,cmasslg,0.1,1,csize
x0=!cxmin+0.73*(!cxmax-!cxmin)
y0=!cymin+0.12*(!cymax-!cymin)
xyouts,x0,y0,'lg m!dc!n',size=csize

if(nk1 lt 4) then goto,cleanup

if(!d.name eq 'PS') then begin
  print,'figure file is:',file
  device,/close
  file='fig_atmos2_'+ext+'.ps'
  device,file=file,xsize=18,ysize=22,xoff=2,yoff=2
endif else begin
  text=''
  read,'<cr> to continue',text
endelse

timescale2,t1,t2
plot_io,zz,t1,$
 title='Run '+ext,xtitle='Height [Mm]',$
 ytitle='timescale H-ionization [s]',_extra=e
scale2,zz,cmasslg,0.1,1,csize
x0=!cxmin+0.73*(!cxmax-!cxmin)
y0=!cymin+0.12*(!cymax-!cymin)
xyouts,x0,y0,'lg m!dc!n',size=csize

cleanup:

reset  

if(!d.name eq 'PS') then begin
  print,'figure file is:',file
  device,/close
  set_plot,'x'
endif

end
