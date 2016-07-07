; $Id: image_cont.pro,v 1.9 2003/02/03 18:13:17 scottm Exp $
;
; Copyright (c) 1988-2003, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

pro image_cont_sj, aorig,xorig,yorig, $
                   WINDOW_SCALE = window_scale, ASPECT = aspect, $
                   INTERP = interp,contour=cont, $
                   xrange=xrg_orig,yrange=yrg_orig,zrange=zrg_orig, $
                   xtitle=xtitle,ytitle=ytitle,title=title,ztitle=ztitle, $
                   _extra=_extra,zlog=zlog,only_colorbar=only_colorbar, $
                   zticks=zticks,ztickv=ztickv,color_range=color_range, $
                   transparent=transparent,position=position,label=label, $
                   nvcolor=nvcolor,barpos=barpos,cutaspect=cutaspect, $
                   retpos=retpos, nobar=nobar, zlen=zlen, distbar=distbar, $
				   barthick=barthick, oc_im=oc_im, oc_color=oc_color, $
				   oc_thick=oc_thick, oc_nlevels=oc_nlevels, $
				   loadct_im=loadct_im, loadct_oc=loadct_oc, $
				   noxval=noxval, noyval=noyval, bwr=bwr, conimage2=conimage2, $
				   qoc_ims=qoc_ims, qoc_colors=qoc_colors, qoc_thicks=qoc_thicks, $
				   qoc_nlevelss=qoc_nlevelss, qloadct_ocs=qloadct_ocs
;+
; NAME:
;	IMAGE_CONT
;
; PURPOSE:
;	Overlay an image and a contour plot.
;
; CATEGORY:
;	General graphics.
;
; CALLING SEQUENCE:
;	IMAGE_CONT, A
;
; INPUTS:
;	A:	The two-dimensional array to display.
;
; KEYWORD PARAMETERS:
; WINDOW_SCALE:	Set this keyword to scale the window size to the image size.
;		Otherwise, the image size is scaled to the window size.
;		This keyword is ignored when outputting to devices with 
;		scalable pixels (e.g., PostScript).
;
; ASPECT:	Set this keyword to retain the image's aspect ratio.
;		Square pixels are assumed.  If WINDOW_SCALE is set, the 
;		aspect ratio is automatically retained.
;
; CUTASPECT: If set then the plot size is reduced to get rid of free
;   space when using the ASPECT keyword
;  
; INTERP:	If this keyword is set, bilinear interpolation is used if 
;		the image is resized.
;
; Keywords added by A. Lagg:
; XRANGE: 2-el vector giving range for x-axis
; YRANGE: 2-el vector giving range for y-axis
; ZRANGE: 2-el vector giving range for z-axis
; CONTOUR: flag for drawing contour levels (default=yes)
; BARPOS: specify position of color bar: 0=right (=def),1=top,2=left,3=bottom  
; XTITLE: xtitle
; XTITLE: ytitle
; TITLE:  title
; 
; Keywords added by Shahin Jafarzadeh:
; NOBAR:		If set, no color bar is drawn
; ZLEN:			Similar to xticklen but for the color bar
; DISTBAR:		Distance of the color bar from the plot;
;				If not set, it will be connected to the plot
; BARTHICK: 	The thickness of the color bar
; OC_IM:		Overlay a contour from another (this) image rather than the 
;				plooted image (contour=1 should have been set). 
;				If not set, and the contour=1 is set, the countour is 
;				computed from the same image.
; OC_COLOR: 	Contour lines' color in "loadct_oc" color table;
;				default: 111 (Red) for default color table "loadct,5"
; OC_THICK:		Thickness of contours; default: 5
; OC_NLEVELS:	Similar no nlevel in contour; default:1
; LOADCT_IM:	color table for image
; LOADCT_OC:	color table for contour
; NOXVAL:		If NOXVAL=1, x-axis values are not shown
; NOYVAL:		If NOYVAL=1, y-axis values are not shown
; BWR:			If this keyword is set, the Blue-White-White-Red color table
;				from the modified procedure "MYCT.pro" is loaded
;
; Hints:		  ctableplot : plots the color table
;			      Ex.) IDL> loadct, 38
;					   IDL> ctableplot
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	The currently selected display is affected.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	If the device has scalable pixels, then the image is written over
;	the plot window.
;
; MODIFICATION HISTORY:
;	DMS, May, 1988.
;	Shahin Jafarzadeh, October, 2011
;-

  doublecoloraxes=0

  if not (keyword_set(nobar)) then nobar=0
  if not (keyword_set(oc_im)) then oc_im=aorig
  if not (keyword_set(oc_color)) then oc_color=111
  if not (keyword_set(oc_nlevels)) then oc_nlevels=1
  if not (keyword_set(oc_thick)) then oc_thick=5
  if not (keyword_set(loadct_oc)) then loadct_oc=5
  if not (keyword_set(loadct_im)) then loadct_im=0
  if not (keyword_set(zlen)) then zlen=0.4
  if not (keyword_set(distbar)) then distbar=0
  if not (keyword_set(barthick)) then barthick=0
  if not (keyword_set(noxval)) then noxval=0
  if not (keyword_set(noyval)) then noyval=0

  if not (keyword_set(conimage2)) then conimage2=0
  if not (keyword_set(qoc_ims)) then qoc_ims=aorig
  if not (keyword_set(qoc_colors)) then qoc_colors=190
  if not (keyword_set(qoc_nlevelss)) then qoc_nlevelss=1
  if not (keyword_set(qoc_thicks)) then qoc_thicks=5
  if not (keyword_set(qloadct_ocs)) then qloadct_ocs=5

  if not (keyword_set(bwr)) then CTLOAD, loadct_im else MYCT, /BuWhWhRd

  if n_elements(aorig) eq 0 then begin
    message,/cont,'No array specified.'
    return
  endif
  
  if n_elements(label) eq 0 then label=1
  
                                ;do scaling if zrange is present
  a=aorig
  nan=where(finite(aorig) eq 0)
  
  if n_elements(xrg_orig) eq 2 then xrange=xrg_orig
  if n_elements(yrg_orig) eq 2 then yrange=yrg_orig
  if n_elements(zrg_orig) eq 2 then zrange=zrg_orig
  if n_elements(zrange) ne 2 then zrange=[min(aorig,/nan),max(aorig,/nan)]
  if n_elements(barpos) eq 0 then barpos=0
  if n_elements(color_range) ne 2 then color_range=[1,254]
  cr=color_range
  
  zrflag=0
  if n_elements(zrange) eq 2 then begin
    if max(zrange) ne min(zrange) then begin
      a=(((aorig-zrange(0))/float(zrange(1)-zrange(0)))<1)>0
;    a=bytscl(a)
      a=((a*(cr(1)-cr(0))+cr(0))>cr(0))<cr(1)
                                ;keep NAN values, set to BG-color
      nf=where(finite(aorig) eq 0)
      if nf(0) ne -1 then begin
        if n_elements(nvcolor) eq 0 then a(nf)=!p.background else a(nf)=nvcolor
      endif
    endif
    zrflag=1
  endif
  
  on_error,2                    ;Return to caller if an error occurs
  sz = size(a)              
  if sz[0] lt 2 then message, 'Parameter not 2D'

  
  if n_params() eq 2 then message,'You must also specify the y coordinate.'
  
  if n_params() eq 3 then begin
    
    xarr=xorig 
    yarr=yorig 
    
    if size(xarr,/type) le 3 then xarr=float(xarr)
    if size(yarr,/type) le 3 then yarr=float(yarr)
    
    xcont=xarr
    ycont=yarr
    ok=sz(1:2) eq (size(xarr))(1:2) and sz(1:2) eq (size(yarr))(1:2)
    if min(ok) eq 0 then message,'X and Y must have same dimension as image.'
    
    
    if n_elements(xrange) ne 2 then xrange=[min(xarr),max(xarr)]
    if n_elements(yrange) ne 2 then yrange=[min(yarr),max(yarr)]
    
                                ;get new image
    dx=min(abs(xarr(1:*,0)-xarr(0:sz(1)-2,0)))
    dy=min(abs(yarr(0,1:*)-yarr(0,0:sz(2)-2)))
    angle=atan(yarr(1)-yarr(0),xarr(1)-xarr(0))
    ddx=dx*cos(angle) + dy*sin(angle)
    ddy=-dx*sin(angle) + dy*cos(angle)
    gs=abs([ddx,ddy])
    nx=(abs((xrange(1)-xrange(0))/ddx)<(sz(1)*sqrt(2.)))<1024
    ny=(abs((yrange(1)-yrange(0))/ddy)<(sz(2)*sqrt(2.)))<1024
    arot=tri_surf_store(a,xarr,yarr,nx=nx,ny=ny,missing=!values.f_nan,/linear)
    
                                ;select image inside x/y range
    minx=min(xarr,/nan,max=maxx)
    miny=min(yarr,/nan,max=maxy)
    xn=findgen(nx)/(nx-1)*(maxx-minx)+minx
    yn=findgen(ny)/(ny-1)*(maxy-miny)+miny
    inx=where(xn ge min(xrange) and xn le max(xrange))
    iny=where(yn ge min(yrange) and yn le max(yrange))
    if inx(0) ne -1 and iny(0) ne -1 then begin
      arot=arot(min(inx):max(inx),min(iny):max(iny))
      xarr=(fltarr(n_elements(iny))+1) ## xn(inx)
      yarr=yn(iny) ## (fltarr(n_elements(inx))+1)
    endif else arot(*)=!values.f_nan
    a=arot
    sz=size(a)
  endif  else begin
    if n_elements(xrange) ne 2 then xrange=[0.,sz(1)]
    if n_elements(yrange) ne 2 then yrange=[0.,sz(2)]
    xarr=((fltarr(sz(2))+1) ## $
          (findgen(sz(1))/(sz(1)-1)*(xrange(1)-xrange(0))+xrange(0)))
    yarr=((findgen(sz(2))/(sz(2)-1)*(yrange(1)-yrange(0))+yrange(0)) ## $
          (fltarr(sz(1))+1))
    xcont=xarr
    ycont=yarr
  endelse
    if size(xrange,/type) le 3 then xrange=float(xrange)
    if size(yrange,/type) le 3 then yrange=float(yrange)
    
    !p.position=0
  contour,[[0,0],[1,1]],/nodata, xstyle=5, ystyle = 5,_extra=_extra, $
    position=position
  
  px = !x.window * !d.x_vsize   ;Get size of window in device units
  py = !y.window * !d.y_vsize
  
                                ;subtract 10% for zrange
  pxo=px & dx=pxo(1)-pxo(0)
  pyo=py & dy=pyo(1)-pyo(0)
  
                                ;px and py contain the image, pxl,pyl
                                ;the label position

  if zrflag and label then begin
    case barpos of
      0: begin
        px(1)=px(1)-0.15*dx
        pxl=[px(1),pxo(1)-0.1*dx]
        pyl=py        
      end
      1: begin
        pxl=px
        py(1)=py(1)-0.15*dy
        pyl=[py(1),pyo(1)-0.1*dy]
      end
      2: begin
        px(0)=px(0)+0.15*dx
        pxl=[pxo(0)+0.1*dx,px(0)]
        px=px-0.08*dx
        pxl=pxl-0.08*dx
        pyl=py
      end
      3: begin
        pxl=px
        py(0)=py(0)+0.15*dy
        pyl=[pyo(0)+0.1*dy,py(0)]
        py=py-.08*dy
        pyl=pyl-.08*dy
      end
    endcase
  endif
  
  if keyword_set(cutaspect) then begin
    aspect=1                    ;set aspect to one (cutaspect only
                                ;makes sence when /aspect is used
    dpx=float(px(1)-px(0)) & dxr=xrange(1)-xrange(0)
    dpy=float(py(1)-py(0)) & dyr=yrange(1)-yrange(0)
;    stop
    if dpx/dxr ge dpy/dyr then begin
      pyn=py & dpyl=0
      pxn=(px(1)+px(0))/2.+[-1.,1.]/2.*dpy/dpx/dyr*dxr*(px(1)-px(0))
;      dpxl=(pxn(0)-px(0))
    endif else begin
      pxn=px & dpxl=0
      pyn=(py(1)+py(0))/2.+[-1.,1.]/2.*dpx/dpy/dxr*dyr*(py(1)-py(0))
;      dpyl=(pyn(0)-py(0))
    endelse
    dpxl=(pxl(1)-pxl(0))
    dpyl=(pyl(1)-pyl(0))
    case barpos of
      0: begin
;        px=pxn & pxl=pxn-dpxl
        px=pxn & pxl=pxn(1)+[0.,dpxl]
        py=pyn & pyl=pyn
      end
      1: begin
        px=pxn & pxl=pxn
;        py=pyn & pyl=pyl-dpyl
        py=pyn & pyl=pyn(1)+[0.,dpyl]+distbar
      end
      2: begin
;        px=pxn & pxl=pxl+dpxl
        px=pxn & pxl=pxn(0)+[-dpxl,0.]
        py=pyn & pyl=pyn
      end
      3: begin
        px=pxn & pxl=pxn
;        py=pyn & pyl=pyl+dpyl
        py=pyn & pyl=pyn(0)+[-dpyl,0.]+distbar
      end
    endcase
  endif
  
  
                                ;change x and y range to maintain
                                ;aspect ratio
   if keyword_set(aspect) then begin
     ddx=(px(1)-px(0))/(max(xrange)-min(xrange))
     ddy=(py(1)-py(0))/(max(yrange)-min(yrange))
     if ddy gt ddx then yrange=total(yrange)/2.+[-.5,.5]/ddx*(py(1)-py(0))
     if ddx gt ddy then xrange=total(xrange)/2.+[-.5,.5]/ddy*(px(1)-px(0))
   endif
  

  swx = px[1]-px[0]             ;Size in x in device units
  swy = py[1]-py[0]             ;Size in Y
  six = float(sz[1])            ;Image sizes
  siy = float(sz[2])
  mmx=minmaxp(xarr,/nan)
  if xrange(1) lt xrange(0) then mmx=reverse(mmx)
  offx=(mmx(0)-xrange(0))/(xrange(1)-xrange(0))*swx
  mmy=minmaxp(yarr,/nan)
  if yrange(1) lt yrange(0) then mmy=reverse(mmy)
  offy=(mmy(0)-yrange(0))/(yrange(1)-yrange(0))*swy
  maxx=(mmx(1)-xrange(0))/(xrange(1)-xrange(0))*swx
  maxy=(mmy(1)-yrange(0))/(yrange(1)-yrange(0))*swy
  pxi=[px(0)+offx,px(0)+maxx]
  pyi=[py(0)+offy,py(0)+maxy]
  swxi = (pxi[1]-pxi[0])>1      ;Size in x in device units
  swyi = (pyi[1]-pyi[0])>1      ;Size in Y
  
  
                                ;Image aspect ratio
;   if n_elements(xrange) eq 2 and n_elements(yrange) eq 2 then $
;     aspi=float(xrange(1)-xrange(0))/(yrange(1)-yrange(0)) $
;   else  aspi = six / siy
;   aspw = swx / swy              ;Window aspect ratio
;   f = aspi / aspw               ;Ratio of aspect ratios

                                ;use the right 10% for displaying the
                                ;zrange

   if nobar eq 1 then begin
      zrflag = 0
      label = 0
   endif 

  if zrflag and label then begin
    zswy = swy
    zswx = swx
;     if keyword_set(aspect) then begin	;Retain aspect ratio?
;                                 ;Adjust window size
;       if f ge 1.0 then zswy = swy / f else zswx = swx * f
;     endif
                                ;convert label pos to device units
    
                                ;make array for zrange
    szx=2. & szy=512.
    zarr=(findgen(szy)/(szy-1.)*(cr(1)-cr(0))+cr(0)) ## (fltarr(szx)+1)

    if barpos eq 1 or barpos eq 3 then zarr=transpose(zarr)
    szx=(size(zarr))(1)
    szy=(size(zarr))(2)
    swx1=(pxo(1)-px(1))
    if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
      tv,zarr,pxl[0],pyl[0],xsize=pxl[1]-pxl[0], ysize=pyl[1]-pyl[0]+barthick,/device
    endif else begin
      tv,poly_2d(zarr,$         ;Have to resample image
                 [[0,0],[szx/(pxl[1]-pxl[0]),0]], $
                 [[0,szy/([pyl[1]-pyl[0]])],[0,0]],$
                 keyword_set(interp),(pxl[1]-pxl[0])+1,pyl[1]-pyl[0]+1), $
        pxl[0],pyl[0]
    endelse
    if barpos eq 0 or barpos eq 2 then begin
	  loadct, 0
      plot,/noerase,zrange,/nodata,xrange=[0,1],/yst,/xst,xticks=1, $
        xtickname=strarr(3)+' ',yrange=zrange, $
        pos = [pxl[0],pyl[0], pxl[1],pyl[1]],/dev, $
        ytickname=strarr(32)+' '
      if not (keyword_set(bwr)) then CTLOAD, loadct_im else MYCT, /BuWhWhRd
    endif else begin
	  loadct, 0
      plot,/noerase,zrange,/nodata,yrange=[0,1],/yst,/xst,yticks=1, $
        ytickname=strarr(3)+' ',xrange=zrange, $
        pos = [pxl[0],pyl[0], pxl[1],pyl[1]+barthick],/dev, color=0, $
        xtickname=strarr(32)+' ', yticklen=0
      if not (keyword_set(bwr)) then CTLOAD, loadct_im else MYCT, /BuWhWhRd
    endelse

    zax=1
    if n_elements(zticks) ne 0 then zax=zticks ge 1
    if zax then begin
      if n_elements(_extra) ne 0 then begin
        ex=_extra
        if max(tag_names(ex) eq 'YTICKNAME') eq 1 then ex.ytickname=''
        if max(tag_names(ex) eq 'ZTICKNAME') eq 1 then ytn=ex.ztickname
        if max(tag_names(ex) eq 'YTICKS') eq 1 then $
        if n_elements(zticks) eq 0 then ex.yticks=0 else ex.yticks=zticks
      endif
      xtl=!x.ticklen & ytl=!y.ticklen
      !x.ticklen=zlen & !y.ticklen=zlen
	  loadct, 5
      case barpos of
        0: axis,yax=1,1.,/yst,yrange=zrange,ytitle=ztitle,yticks=zticks, $
          ytickv=ztickv,ytickname=ytn,_extra=ex, color=0
        1: axis,xax=1,/xst,xrange=zrange,xtitle=ztitle,xticks=zticks, $
          xtickv=ztickv,xtickname=ytn,_extra=ex, color=0
        2: axis,yax=0,/yst,yrange=zrange,ytitle=ztitle,yticks=zticks, $
          ytickv=ztickv,ytickname=ytn,_extra=ex, color=0
        3: axis,xax=0,1.,/xst,xrange=zrange,xtitle=ztitle,xticks=zticks, $
          xtickv=ztickv,xtickname=ytn,_extra=ex, color=0
      endcase
	   if not (keyword_set(bwr)) then CTLOAD, loadct_im else MYCT, /BuWhWhRd
      !x.ticklen=xtl & !y.ticklen=ytl
    endif
  endif
  if keyword_set(only_colorbar) then return
  
                                ;reset ISO flag
  if n_elements(_extra) ne 0 then begin
    tn=tag_names(_extra)
    iiso=where(strpos(tn,'ISO') ne -1)
    if iiso(0) ne -1 then _extra.(iiso(0))=0
    ind=where(strpos(tn,'NODATA') ne -1)
    if ind(0) ne -1 then nodata=_extra.(ind(0))   
  endif
  

  
                                ;keep NAN values, set to BG-color
;   fi=where(finite(a))
;   if fi(0) ne -1 then a(fi)=(a(fi)>1)<cr(1)
;   nf=where(finite(a) eq 0)
;   if nf(0) ne -1 then a(nf)=!p.background
  if keyword_set(nodata) eq 0 then begin
    if nan(0) eq -1 or keyword_set(transparent) eq 0 then begin
      if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
        pxi=[pxi[1]-swxi,pxi[1]]
        tv,((a)>cr(0))<cr(1),pxi[0],pyi[0],xsize = swxi, ysize = swyi, /device

      endif else begin                          ;Not scalable pixels	
        if keyword_set(window_scale) then begin ;Scale window to image?
          pxi=[pxi[1]-six,pxi[1]]
          tv,a,pxi[0],pyi[0]    ;Output image
        endif else begin        ;Scale window
          pxi=[pxi[1]-swxi,pxi[1]]
                                ;Have to resample image
          ap=poly_2d((a),[[0,0],[six/swxi,0]], [[0,siy/swyi],[0,0]],$
                     keyword_set(interp),swxi,swyi)
          tv,((ap)>cr(0))<cr(1),pxi[0],pyi[0]
        endelse                 ;window_scale
      endelse                   ;scalable pixels
    endif else begin            ;overplot valid pixels only
      a=float(a) & a(nan)=!values.f_nan
      for ix=0,sz(1)-1 do for iy=0,sz(2)-1 do if finite(a(ix,iy)) then begin
                                ;make it faster + ps file smaller:
                                ;check if iy+1 ... is
                                ;also finite
        iyadd=0
        ende=0
        repeat begin
          iyadd=iyadd+1
          if iy+iyadd eq sz(2) then ende=1 $
          else ende=finite(a(ix,iy+iyadd)) eq 0
        endrep until ende
        iyadd=iyadd-1        
        if iyadd eq 0 then ap=a(ix,iy)+intarr(2,2) $
        else ap=a(ix,iy:iy+iyadd) ## (intarr(2)+1)
;        iyadd=0
;        ap=a(ix,iy)+intarr(2,2)
        szx=round(swxi/six)>2
        szy=round(swyi*(iyadd+1)/siy)>2        
;        if total(finite(ap)) ge 1 then begin
          if (!d.flags and 1) ne 0 then begin	;Scalable pixels?
            tv,ap,pxi(0)+ix*swxi/six,pyi(0)+iy*swyi/siy, $
              xsize=szx,ysize=szy,/device
          endif else begin
;           ap=poly_2d(ap,[[0,0],[1,0]], [[0,iyadd+1],[0,0]], $
;                      keyword_set(interp),szx,szy)
            ap=congrid(ap,szx,szy,/interp)
            tv,ap,pxi(0)+ix*swxi/six,pyi(0)+iy*swyi/siy,/device
          endelse
;        endif
        iy=iy+iyadd
      endif
    endelse
  endif
  
  mx = !d.n_colors-1                           ;Brightest color
  colors = [mx,mx,mx,0,0,0]                    ;color vectors
  if !d.name eq 'PS' then colors = mx - colors ;invert line colors for pstscrp
  
  if n_elements(cont) eq 0 then cont=1
  
  retpos=[px[0],py[0], px[0]+swx,py[0]+swy]
                                ;set variables to allow overplot
  posnorm=convert_coord(/to_normal,/device,retpos([[0,1],[2,3]]))
  !x.region=posnorm(0,*)
  !y.region=posnorm(1,*)
  !p.position=posnorm([0,1,3,4])

  CTLOAD, loadct_oc
  contour,oc_im,xcont,ycont,/noerase,xst=5,yst=5,zst=5,$ ;Do the contour
    c_color = oc_color, NLEVELS=oc_nlevels, c_thick=oc_thick, $
    xrange=xrange,yrange=yrange,nodata=(cont eq 0) or keyword_set(nodata), $
    xtitle=xtitle,ytitle=ytitle,title=title,_extra=_extra

 if conimage2 then begin
 CTLOAD, qloadct_ocs
 contour,qoc_ims,xcont,ycont,/noerase,xst=5,yst=5,zst=5,$ ;Do the contour
   c_color = qoc_colors, NLEVELS=qoc_nlevelss, c_thick=qoc_thicks, $
   xrange=xrange,yrange=yrange,nodata=(cont eq 0) or keyword_set(nodata), $
   xtitle=xtitle,ytitle=ytitle,title=title,_extra=_extra
 endif

  case barpos of
    0: begin
      xax1=0 & posx1=yrange(0) & xax2=1 & posx2=yrange(1)
      yax1=0 & posy1=xrange(0) & yax2=1 & posy2=xrange(1)
    end
    1: begin
      xax1=0 & posx1=yrange(0) & xax2=1 & posx2=yrange(1)
      yax1=0 & posy1=xrange(0) & yax2=1 & posy2=xrange(1)
    end
    2: begin
      xax1=0 & posx1=yrange(0) & xax2=1 & posx2=yrange(1)
      yax2=0 & posy2=xrange(0) & yax1=1 & posy1=xrange(1)
    end
    3: begin
      xax2=0 & posx2=yrange(0) & xax1=1 & posx1=yrange(1)
      yax1=0 & posy1=xrange(0) & yax2=1 & posy2=xrange(1)
    end
  endcase
  xtn=strarr(32)+' '
  ytn=strarr(32)+' '
  if n_elements(_extra) ne 0 then begin
    exx=_extra
    exy=_extra
    if max(tag_names(exx) eq 'XTICKNAME') eq 1 then begin
      exx.xtickname=' '
      dummy=temporary(xtn)
    endif
    if max(tag_names(exy) eq 'YTICKNAME') eq 1 then begin
      exy.ytickname=' '
      dummy=temporary(ytn)
    endif
  endif
  
  if doublecoloraxes then begin
	  axis,xaxis=xax1,0,posx1,/xst,xtitle=xtitle,_extra=_extra, color=255
	  axis,xaxis=xax2,0,posx2,/xst,_extra=exx,xtickname=xtn, color=255
	  axis,yaxis=yax1,posy1,/yst,ytitle=ytitle,_extra=_extra, color=255
	  axis,yaxis=yax2,posy2,/yst,_extra=exy,ytickname=ytn, color=255
	
	  axis,xaxis=xax1,0,posx1,/xst,xtitle=xtitle,_extra=_extra, xticklen=-0.05
	  axis,xaxis=xax2,0,posx2,/xst,_extra=exx,xtickname=xtn, xticklen=-0.05
	  axis,yaxis=yax1,posy1,/yst,ytitle=ytitle,_extra=_extra, yticklen=-0.05
	  axis,yaxis=yax2,posy2,/yst,_extra=exy,ytickname=ytn, yticklen=-0.05
  endif else begin
	  if noxval then axis,xaxis=xax1,0,posx1,/xst,xtitle=xtitle,_extra=_extra, color=0, xtickformat='(A1)' else $
	  axis,xaxis=xax1,0,posx1,/xst,xtitle=xtitle,_extra=_extra, color=0
	  axis,xaxis=xax2,0,posx2,/xst,_extra=exx,xtickname=xtn, color=0
	  if noyval then axis,yaxis=yax1,posy1,/yst,ytitle=ytitle,_extra=_extra, color=0, ytickformat='(A1)' else $
	  axis,yaxis=yax1,posy1,/yst,ytitle=ytitle,_extra=_extra, color=0
	  axis,yaxis=yax2,posy2,/yst,_extra=exy,ytickname=ytn, color=0
  endelse

  return
end
