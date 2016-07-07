pro fig_method,ps=ps
;+
;   fig_method ,/ps
;
;            makes an explanation of the employed method
;
;-
common cfig_method,vobs,vpist,v260

if(n_elements(ps) ne 0) then set_plot,'ps'
if(!d.name eq 'PS') then begin
  file='fig_method.ps'
  device,file=file,xsize=18,ysize=24,xoff=2,yoff=2,bits=8,/color
endif

; load color lookup table with 
; 1  black
; 2 red
; 3 green
; 4 blue

r=findgen(!d.n_colors)/(!d.n_colors-1.)*255
g=r
b=r
r(2)=255
g(2)=0
b(2)=0
r(3)=0
g(3)=255
b(3)=0
r(4)=0
g(4)=0
b(4)=255
tvlct,r,g,b

; read in velocities in iron line and at piston

if(n_elements(vpist) eq 0) then restore,'fig_method.idlsave',/verbose

font='!17'
!color=1
dx=0.30
dy=0.16
y1=-0.013
y2=0.168
!p.charsize=0.7
!x.margin=[8,0]
!y.margin=[4,0]
!p.position=[0,0,0,0]
!p.region=[0,y1,dx,y1+dy]
t=findgen(375)*10.
plot,t,vpist,yran=[-1,1],xtitle='time [s]',ytitle='V [km/s]',$
 title='V piston  (h=-100 km)',back=255

!p.region=[0,y2,dx,y2+dy]
plot,t,v260,yran=[-1,1],xtitle='time [s]',ytitle='V [km/s]',/noerase,$
 title='V simulation (h= 260 km)'


!p.region=[0.97-dx,y2,0.97,y2+dy]
plot,t,vobs,yran=[-1,1],xtitle='time [s]',ytitle='V [km/s]',/noerase,$
 title='V observations Fe I line'

x0=0.53
y0=0.4
x1=x0+0.35
y1=0.9
!p.region=[x0,y0,x1,y1]
if(n_elements(xrange) eq 0) then xrange=[-0.5,0.5]
xtitle= '!4Dk!3 ('+string(197B)+'ngstr'+string(246B)+'m)'
ytitle='Time [s]'
if(n_elements(gam) eq 0) then gam=1.0
if(n_elements(yoff) eq 0) then yoff=0.0
spec_init,xl,time
spec,image,110
    yobs=fltarr(223,750)+1.0
    yobs(0:155,0:749)=image(169:324,0:749)
    yy=(alog10(yobs)*100.+255)*gam+yoff       ; make logarithmic scaling
    yy=(yy > 0) < 255                    ; restrict range
    yy=byte(yy)                          ; convert to type byte
    nt=n_elements(yy(0,*))               ; find number of timesteps
    nx=n_elements(yy(*,0))               ; find number of wavelengths
    dt=5.
    x0=-1.29864+findgen(nx)/(nx-1)*2*1.29864
    iw=where((x0 gt xrange(0)) and (x0 lt xrange(1)))
    ymin=1000
    ymax=2000
    plot,x0(iw),yobs(iw,0),/nodata,$
       xrange=xrange,xstyle=1,$
       yrange=[ymin,ymax],ystyle=1,ymargin=[4,2],$
       title='Ca II H-line',$
       xtitle=xtitle,ytitle=ytitle,charsize=1.0,/noerase
    for i=ymin/dt,ymax/dt,3 do begin
      oplot,x0(iw),yobs(iw,i)*500+i*dt
    endfor

;    plot_image,yy(iw,0:nt-1),xrange,[0,(nt-1)*dt],$
;       xrange=xrange,xstyle=1,$
;       yrange=[0,(nt-1)*dt],ystyle=1,ymargin=[4,2],$
;       title='Ca II H-line',$
;       xtitle=xtitle,ytitle=ytitle,charsize=1.0,/noerase
!p.charsize=1.0
!p.region=[0,0,0,0]
!p.position=[0.5,0.05,1.0,0.95]
plot,[0,1],[-100,2000],/nodata,xstyle=4,ystyle=9,ytitle='Height [km]',/noerase

oplot,[0,0.475],[260,556],linestyle=2
oplot,[0,0.475],[260,267.8],linestyle=2

oplot,[0,-0.4],[260,556],linestyle=2,/noclip
oplot,[0,-0.4],[260,267.8],linestyle=2,/noclip

oplot,[0,-0.4],[-100,139],linestyle=2,/noclip
oplot,[0,-0.4],[-100,-149],linestyle=2,/noclip

oplot,[0.703,0.703,0.084,0.03,0.084,0.772,0.772],$
      [194.2,-75,-75,-100,-125,-125,194.2],color=2,/noclip

xyouts,-0.50,2000,font+'Simulation!3',align=0.5,color=2,size=2.0
xyouts, 0.50,2000,font+'Observation!3',align=0.5,color=2,size=2.0

x0=-0.5
y0=1900
xyouts,x0,y0,font+'Transparent upper boundary!3',size=1.0,align=0.5

x0=-0.5
y0=1500
xyouts,x0,y0,font+'t=0: div(F!drad!n+F!dconv!n)=0!3',size=1.5,color=2,align=0.5



x0=-0.5
y0=1300
dy=50

text=strarr(10)
cs=fltarr(10)+1.4
col=intarr(10)+4
col(0)=2
text(0)='17 equations, 101 depth points'
text(1)='  Charge'
text(2)='  Mass'
text(3)='  Momentum'
text(4)='  Energy'
text(5)='  Grid'
text(6)='  12 rate equations'
text(7)='    H  5+cont'
text(8)='    Ca 5+cont'
text(9)='    non-LTE, CRD'
col(7:9)=1

for i=0,n_elements(text)-1 do begin
  xyouts,x0,y0-i*dy,font+text(i)+'!3',size=cs(i),color=col(i),align=0.5
endfor

xyouts,0.04, 32,font+'Piston velocities from!3',size=1.2,color=4
xyouts,0.04,-10,font+'observed shifts in Fe line!3',size=1.2,color=4

if(!d.name eq 'PS') then begin
  print,'figure file is: ',file
  device,/close
  set_plot,'x'
endif

end


