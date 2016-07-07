pro ci_cont,istep, kr=kr,yrange=yrange,cntrb=cntrb,tmax=tmax,ps=ps,bw=bw
;
;  PS:     /ps gives postscript file
;
@common_radyn
@common_movie
common cxnyc,x_nyc,tauq_nyc,src
;
if(n_params() lt 1) then begin
  print,'ci_cont,istep,kr=kr,yrange=yrange,cntrb=cntrb,tmax=tmax,ps=ps,bw=bw'
  return
endif
;
if(n_elements(kr) eq 0) then kr=10
if(cont(kr) eq 0) then begin
  print,'not a continuum transition' 
  return
endif
if(n_elements(yrange) eq 0) then yrange=[-0.1,2.3]
if(n_elements(cntrb) eq 0) then cntrb=4
if(cntrb gt 4) then cntrb=4
if(cntrb le 0) then cntrb=4
;
if(n_elements(ps) ne 0) then set_plot,'ps'
if(!d.name eq 'PS') then begin
  if(n_elements(xsize) eq 0) then xsize=12
  if(n_elements(ysize) eq 0) then ysize=10
  if(n_elements(bw) eq 0) then begin
    file='fig_ci_cont'+'_'+string3(istep)+'_c.ps'
    device,file=file,xsize=xsize,ysize=ysize,xoff=2,yoff=2,bits=8,/color
  endif else begin
    file='fig_ci_cont'+'_'+string3(istep)+'.ps'
    device,file=file,xsize=xsize,ysize=ysize,xoff=2,yoff=2,bits=8
  endelse
  thick_tau=5.5
  thick_t=5.5
  thick_f=5.5
  csize=1.5
  cthick=5.5
  !p.charsize=1.0
endif else begin
  csize=1.0
  thick_tau=2.5
  thick_t=2.5
  thick_f=2.5
  cthick=2.0
  !p.charsize=1.0
  if(n_elements(xsize) eq 0) then xsize=768
  if(n_elements(ysize) eq 0) then ysize=576
endelse
ci_lct,white,red,green,blue,yellow,black,rr,gg,bb,bw=bw
!color=white
back=black
if(n_elements(ps) ne 0) then back=white

xt_cont,istep,kr    ; calculate x_nyc, tauq_nyc, and src

!x.style=1
!y.style=1

y=(z1t(0:ndep-2,istep)+z1t(1:ndep-1,istep))*0.5e-8 
y=[2*y(0)-y(1),y] ; extrapolate at top

x=cc*1.e7/frq(0:nq(kr)-1,ktrans(kr)-1)
nfrq=n_elements(x)
frq0=x(1)
z1=x_nyc/xmu(nmu-1)*alog(10.)
z2=tauq_nyc*exp(-tauq_nyc)
z3=fltarr(ndep,nfrq)
z4=z3
for ny=1,nfrq-1 do begin
  z3(1:*,ny)=taut(1:*,istep)/tauq_nyc(1:*,ny)
  z4(*,ny)=src(*,ny)
endfor

tau1=fltarr(nfrq)
for ny=1,nfrq-1 do begin
  intep,alog10(tauq_nyc(1:*,ny)),y(1:*),0.0,yp
  tau1(ny)=yp
endfor
tau1(0)=tau1(1)
flux,lam,fluxt

ztot=fltarr(nfrq,ndep,2,2)
ztot(*,*,0,0)=transpose(z1*z3)
ztot(*,*,0,1)=transpose(z2)
ztot(*,*,1,0)=transpose(z4)
ztot(*,*,1,1)=transpose(z1*z2*z3*z4)

if (cntrb eq 1) then z=ztot(*,*,0,0) & zid='d(lg !4s!dm!n!3)/d(lg !4s!3!d500!n)'
if (cntrb eq 2) then z=ztot(*,*,0,1) & zid='!4s!dm!n!3exp(-!4s!dm!3!n)'
if (cntrb eq 3) then z=ztot(*,*,1,0) & zid='S!4!dm!3!n'
if (cntrb eq 4) then z=ztot(*,*,1,1) & zid='C!dI!n'
if(n_elements(tmax) eq 0) then begin
  zmax=max(z)
endif else begin
  zmax=planck(alamb(kr),tmax)
endelse
xmin=min(x)
xmax=max(x)
shade=bytscl(z,max=zmax,top=white)
if(!d.name eq 'PS') then begin
  shade_surf,z,x,y,shade=shade,az=0,ax=90,xstyle=1,ystyle=1,$
     yrange=yrange,zstyle=4,color=white,background=back,$
     /clip,xmargin=[9.2,2]
endif else begin
  shade_surf,z,x,y,shade=shade,az=0,ax=90,xstyle=1,ystyle=1,$
     yrange=yrange,zstyle=4,color=white,background=back,$
     /clip
endelse
oplot,[!cxmax,!cxmax,!cxmin],[!cymin,!cymax,!cymax],/noclip
oplot,x,tau1,color=green,thick=thick_tau,lin=0
plot,lam*1.e3,fluxt(*,istep),thick=thick_f,xrange=[xmin,xmax],$
  /noerase,zstyle=4,xstyle=1,ystyle=4,lin=2
plot,alog10(tg1t(*,istep)),y,linestyle=0,thick=thick_t,$
  yrange=yrange,xrange=[7,3],/noerase,zstyle=4,xstyle=4,ystyle=1,$
  color=red
plot,x,y,/nodata,/noerase,xstyle=1,ystyle=1,yrange=yrange
oplot,[!cxmax,!cxmax,!cxmin],[!cymin,!cymax,!cymax],/noclip
if(!d.name eq 'PS') then begin
  axis,xaxis=0,xstyle=1,color=black,xtit='Wavelength (nm)'
  axis,xaxis=0,xstyle=1,color=white,xtitle='',xtickname=replicate(' ',20)
  axis,yaxis=0,ystyle=1,color=black,ytit='Height (Mm)'
  axis,yaxis=0,ystyle=1,color=white,ytitle='',ytickname=replicate(' ',20)
endif
cx0=!cxmax-(!cxmax-!cxmin)/6.
cx1=(!cxmax-!cxmin)/12.+!cxmin
xyouts,cx1,!cymax-0.1*(!cymax-!cymin),zid,size=csize,charthick=cthick
xyouts,cx1,!cymax-0.3*(!cymax-!cymin),'T!dg!n',size=csize,$
  color=red,charthick=cthick
xyouts,cx1,!cymax-0.2*(!cymax-!cymin),'F!d!4m!3!n ---',size=csize,$
  charthick=cthick
xyouts,cx1,!cymax-0.4*(!cymax-!cymin),'!4s!dm!3!n=1',size=csize,$
  color=green,charthick=cthick
;
if(!d.name eq 'PS') then begin
  print,'figure file is: ',file
  device,/close
  set_plot,'x'
endif 
;
end

pro xt_cont,istep,kr

@common_radyn
@common_movie

common cxnyc,x_nyc,tauq_nyc,src

if(n_params() lt 1) then begin
  print,'xt_cont,istep,iel,kr'
  return
endif

nq0=nq(kr)
x_nyc=fltarr(ndep,nq0)
tauq_nyc=fltarr(ndep,nq0)
src=fltarr(ndep,nq0)
x_nyc(*,*)=0.
tauq_nyc(*,*)=0.
src(*,*)=0.

for ny=1,nq0-1 do begin
  xt_nycont,istep,kr,ny,x,tauq,slc
  x_nyc(*,ny)=x
  tauq_nyc(*,ny)=tauq
  src(*,ny)=slc
endfor

end

pro ci_lct,white,red,green,blue,yellow,black,rr,gg,bb,bw=bw
;+
;   ci_lct,white,red,green,blue,yellow,black,rr,gg,bb,bw=bw
;
;            set color lookup table for Ci panel plots
;
;-

max_color=!d.n_colors-1


if(n_elements(bw) eq 0) then begin
  rr=findgen(256)/(max_color-5)*255
  gg=rr
  bb=rr
  white =max_color-5
  red   =max_color-4
  green =max_color-3
  blue  =max_color-2
  yellow=max_color-1
  black =0

  rr(max_color-4:255)=0
  gg(max_color-4:255)=0
  bb(max_color-4:255)=0
  rr(red)   =255
  gg(green) =150
  bb(blue)  =255
  rr(yellow)=255
  gg(yellow)=255
;  rr(yellow)=255
;  bb(yellow)=255

  tvlct,rr,gg,bb

endif else begin
  white =!d.n_colors-1
  red   =!d.n_colors-10
  green =150*white/255.
  blue  =200*white/255.
  yellow=200*white/255.
  black =0
  loadct,0
endelse

end

