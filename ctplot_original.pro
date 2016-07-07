;  $Id: ctplot.pro 646 2010-12-13 11:47:26Z bingert $
;
;
pro ctplot,bild,x,y,xtitle=xtitle,ytitle=ytitle,charsize=charsize, $
           title=title,bartitle=bartitle,range=range,log=log, $
           interpolate=interpolate,ps=ps,wpos=wpos,zlog=zlog,ctb=ctb,  $
           invert=invert,noerase=noerase,noframe=noframe,orig=orig, $
           axis_color=axis_color,keep=keep,noxylabel=noxylabel, $
           xmargin=xmargin,ymargin=ymargin,noxlabel=noxlabel, $
           noylabel=noylabel,aspect=aspect,ctw=ctw
;
; 16/Jun/2008 bingert: coded
;     bingert@kis.uni-freiburg.de
;
if n_elements(bild) eq 0  then begin
print,' Calling sequence'
print,' ----------------'
print,'ctplot,bild,x,y,xtitle=xtitle,ytitle=ytitle,charsize=charsize, $'
print,'            title=title,bartitle=bartitle,range=range,log=log, $'
print,'            interpolate=interpolate,ps=ps,wpos=wpos,zlog=zlog,ctb=ctb,  $'
print,'            invert=invert,noerase=noerase,noframe=noframe,orig=orig, $ '
print,'            axis_color=axis_color,keep=keep'
print,' ----------------'
;
print,'  bild:     is the 2-D image'
print,'  ctb:      add a color table to the right of the image'
print,'  ctw:      width of the colortable. Default:0.06'
;
print,'  x and y:  specify the x and y axis of the plot'
;
print,'  xtitle:   title for x-axis '
print,'  ytitle:   title for y-axis '
print,'  title:    main title for window'
print,'  bartitle: title for the color table (z-axis)'
print,''
print,'  log:      show image in log plot and a linear axis'
print,'            negative and zero values'
print,'            will be shown as minimum values !!!'
print,''
print,'  zlog:     same as log but logarithmic axis'
print,' '
print,'  range:    display image with given range'
print,'            if /log is set range has to be given in log values'
print,''
print,'  interpolate:  smooth image; works only for X-devices'
print,''
print,'  ps:       ps=filename creates a PS-File named filename.ps'
print,''
print,'  wpos:     variable will be filled with the main plot positions'
print,'            usefull for overplotting a contour plot'
print,''
print,'  keep:     keeps aspect ratio  of the original image'
print,''
print,'  aspect:   force aspect ratio to the given number nx=aspect*ny'
print,''
print,'  orig:     plots the original size of the image without any scaling'
print,''
print,'  axis_color: sets the color of the axis'
print,''
   return
endif else bild2 = reform(bild)   ; store in new variable to allow
                                  ; image manipulation
;
if ((size(bild2))[0] ne 2) then begin
   print,'WARING: wrong image dimension'
   return
endif
;
if not keyword_set(ctw) then ctw=0.06
tvlct,r,g,b,/get
loadct,0,/silent
;
if keyword_set(log) or keyword_set(zlog) then begin
    if (min(bild2) le 0.) then $
      print,'WARNING: negative or zero values in log plot, will be set to min values'
    ;
    minval = min(bild2[where(bild2 gt 0.)])
    ;
    good = where(bild2 le 0)
    if good[0]  ne -1 then bild2[good] = minval
                                ;
    bild_nolog = bild2
    bild2 = alog10(bild2)
endif
;
if keyword_set(range) then begin
   bild2=(bild2 > range[0]) < range[1]
;
; has to save range if range is bigger then picture values
   plotr=range*1.
endif else plotr=[min(bild2),max(bild2)]
; now plotr is the range for the colortable
;
;
; if no axis are give create new ones
if n_elements(x) eq 0 then x = findgen((size(bild2))[1])
if n_elements(y) eq 0 then y = findgen((size(bild2))[2])
;
if keyword_set(interpolate) then cubic=1 else cubic=0
;
if not keyword_set(charsize) then charsize=1.
;
;
if keyword_set(ps) then begin
    set_plot,'PS'
    !p.font=0
    device,filename=ps+".ps",xsize=12,ysize=10,/color,bits_per_pixel=8
endif
;
;  first initialize plot for given size of titles
;
if (not keyword_set(noerase)) then $
   plot,x,y,xtitle=xtitle,ytitle=ytitle,charsize=charsize,/nodata, $
        xstyle=5,ystyle=5,title=title,xmargin=xmargin,ymargin=ymargin
;
;
; read out corners
;
pmimax = !x.window    ; coordinates of corners in percent
pmimay = !y.window    ; coordinates of corners in percent
;
; take care of !p.multi option
;
xdiv=1.
delta_x=0.
dx = pmimax[0]
;
if total(!p.multi) gt 0  then begin
    xdiv =!p.multi[1]
    while dx gt 0 do begin
        dx = dx - 1./xdiv
    endwhile
    dx = dx + 1./xdiv
    delta_x = pmimax[0]-dx
endif
;
; create cornes of the new boxes
;
if keyword_set(ctb) then begin
   ;
   ; if color table is requested move the right boundary
   ;
   position= [pmimax[0],pmimay[0],1./xdiv-dx-ctw+delta_x,pmimay[1]]
   pos_ct = [position[2]+0.01,pmimay[0],position[2]+ctw,pmimay[1]]
endif else begin
   position= [pmimax[0],pmimay[0],pmimax[1],pmimay[1]]
endelse
;
; until here the image would be stetched to fit into the plotting area
;
; keyword orig does not scale the image in any direction
if keyword_set(orig) then begin
   pmimax[1]=pmimax[0]+1.*(size(bild2))[1]/!d.x_size
   pmimay[1]=pmimay[0]+1.*(size(bild2))[2]/!d.y_size
   position= [pmimax[0],pmimay[0],pmimax[1],pmimay[1]]
   pos_ct = [position[2]+0.01,pmimay[0],position[2]+ctw,pmimay[1]]
endif
;
; keyword keep scales the image but conserves aspect ratio
; image will the scaled to fit best into the plotting area
if keyword_set(keep) then begin
   nx = (size(bild2))[1]*1.
   ny = (size(bild2))[2]*1.
   ;
   dx=(position[2]-position[0])*!d.x_size ; max available dx
   dy=(position[3]-position[1])*!d.y_size ; dy size in pixels
   ;
   if nx/ny*dy lt dx then begin
      position[2] = position[0]+nx/ny*dy/!d.x_size
      pos_ct = [position[2]+0.01,position[1],position[2]+ctw,position[3]]
   endif else begin
      position[3] = position[1]+ny/nx*dx/!d.y_size
      pos_ct = [position[2]+0.01,position[1],position[2]+ctw,position[3]]
   endelse
endif
;
if keyword_set(aspect) then begin
   if aspect le 0 or aspect ge 1 then begin
       print,'Aspect has to be between 0 and 1'
       return
   endif
   nx = (size(bild2))[1]*1.
   ny = (size(bild2))[2]*1.
   if ny gt aspect*nx then begin
       nx = ny/aspect
   endif else ny = aspect*nx
   ;
   dx=(position[2]-position[0])*!d.x_size ; max available dx
   dy=(position[3]-position[1])*!d.y_size ; dy size in pixels
   ;
   if nx/ny*dy lt dx then begin
      position[2] = position[0]+nx/ny*dy/!d.x_size
      pos_ct = [position[2]+0.01,position[1],position[2]+ctw,position[3]]
   endif else begin
      position[3] = position[1]+ny/nx*dx/!d.y_size
      pos_ct = [position[2]+0.01,position[1],position[2]+ctw,position[3]]
   endelse
endif
;
; rescale 2D-image only for X-Window
;
npicsx=(position[2]-position[0])*!d.x_size
npicsy=(position[3]-position[1])*!d.y_size
;
bb = bild2
if not (keyword_set(ps)) and not  (keyword_set(orig))  and not(!d.name eq 'PS') then $
  bb = congrid(bild2,npicsx-1,npicsy-1,cubic=cubic)
;
; macht mehr oder weniger das Gleiche wie bytscl()
;
resu = (bb - plotr[0])/(plotr[1]-plotr[0])*255.
resu = (resu > 0) < 255  ; need to adjust because congrid (7 lines above)
                         ; tends to change the range
;
; create color bar
;
if keyword_set(ctb) then begin
    npx=(pos_ct[2]-pos_ct[0])*!d.x_size
    npy=(pos_ct[3]-pos_ct[1])*!d.y_size
    if (npx le 1 ) then begin
       print,'WARNING: ctw may be choosen to small'
       return
    endif
    ;
    if keyword_set(ps) or !d.name eq 'PS' then begin
        ctb = findgen(256)
        ctb = reform(ctb,1,256)
        ctb = rebin(ctb,2,256)
    endif else begin
        ctb = findgen(npy)/(npy-1.)*255.
        ctb = rotate(rebin(ctb,npy,npx),1)
    endelse
    ;
    if keyword_set(invert) then ctb = -ctb-1
    ;
    ;  plot color bar
    ;
    tvlct,r,g,b
    tv,ctb,pos_ct[0]*!d.x_size+1,pos_ct[1]*!d.y_size+1,ysize=npy,xsize=npx
    ;
    loadct,0,/silent
    ;
    if not keyword_set(noframe) then begin
        plot,[0,1],/noerase,position=pos_ct,xticks=1,xminor=1,/nodata,yticks=1,yminor=1, $
          xstyle=1,ystyle=1,xtickname=replicate(' ',60),ytickname=replicate(' ',60)

    ;
    ;  add axis
    ;
       if (keyword_set(zlog)) then begin
          plotr = 10^plotr
       endif
    ;
       axis,1,0,yax=1,ystyle=1,yrange=plotr,charsize=charsize,ytitle=bartitle, $
            ylog=zlog,yticklen=0.3
    endif else begin
        plot,[0,1],/noerase,position=pos_ct,xticks=1,xminor=1,/nodata,yticks=1,yminor=1, $
          xstyle=4,ystyle=4
    endelse
    ;
    tvlct,r,g,b
    ;
endif
;
; plot image
;
tvlct,r,g,b

if keyword_set(invert) then resu=abs(resu-255)


tv,resu,pmimax[0]*!d.x_size+1,pmimay[0]*!d.y_size+1,ysize=npicsy-1,xsize=npicsx-1
loadct,0,/silent

wpos=position
if (keyword_set(keep) or keyword_set(orig)) and keyword_set(title) then begin
   xyouts,0.5*(wpos[2]+wpos[0]),wpos[3]+0.5*!d.y_ch_size/!d.y_size,title,align=0.5,/normal,charsize=charsize
   title=''
endif

if keyword_set(noxylabel) then begin
   noxlabel=1
   noylabel=1
endif
if keyword_set(noxlabel) then xtickname = replicate(' ',60)
if keyword_set(noylabel) then ytickname = replicate(' ',60)
;
if not keyword_set(noframe) then begin
    plot,x,y,xtitle=xtitle,ytitle=ytitle,charsize=charsize, $
      position=position,/nodata,/noeras,xstyle=1,ystyle=1, $
      xrange=[x[0],x[n_elements(x)-1]],yrange=[y[0],y[n_elements(y)-1]],color=axis_color, $
      xtickname=xtickname,ytickname=ytickname
endif else begin
    plot,x,y,xtitle=xtitle,ytitle=ytitle,charsize=charsize, $
      position=position,/nodata,/noeras,xstyle=4,ystyle=4, $
      xrange=[x[0],x[n_elements(x)-1]],yrange=[y[0],y[n_elements(y)-1]],color=axis_color, $
      xtickname=xtickname,ytickname=ytickname
endelse
;
;  return position of the plotting area.
;  it can be used e.g. as input for contour plots
if (keyword_set(wpos)) then wpos=position
;
if keyword_set(ps) then begin
    device,/close
    set_plot,'X'
    !p.font=-1   ; return to default value
endif
 tvlct,r,g,b
;
;
end
