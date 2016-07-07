
; $Id: image_cont.pro,v 1.8 2002/02/06 21:45:47 scottm Exp $
;
; Copyright (c) 1988-2002, Research Systems, Inc.  All rights reserved.
;   Unauthorized reproduction prohibited.

pro contour_image, a,b, WINDOW_SCALE = window_scale, ASPECT = aspect, $
    INTERP = interp,ncl=ncl,pcl=pcl,xtitle=xtitle,ytitle=ytitle,title=title,scale=scale,charsize=charsize,charthick=charthick,$
    c_thick=c_thick
;+
; NAME:
;   IMAGE_CONT_NEW
;
; PURPOSE:
;   Overlay an image and a contour plot.
;
; CATEGORY:
;   General graphics.
;
; CALLING SEQUENCE:
;   IMAGE_CONT, A,B
;
; INPUTS:
;   A:    The two-dimensional array to display.
;
; KEYWORD PARAMETERS:
; WINDOW_SCALE: Set this keyword to scale the window size to the image size.
;     Otherwise, the image size is scaled to the window size.
;     This keyword is ignored when outputting to devices with
;     scalable pixels (e.g., PostScript).
;
;   ASPECT:   Set this keyword to retain the image's aspect ratio.
;     Square pixels are assumed.  If WINDOW_SCALE is set, the
;     aspect ratio is automatically retained.
;
;   INTERP:   If this keyword is set, bilinear interpolation is used if
;     the image is resized.
;
; OUTPUTS:
;   No explicit outputs.
;
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   The currently selected display is affected.
;
; RESTRICTIONS:
;   None.
;
; PROCEDURE:
;   If the device has scalable pixels, then the image is written over
;   the plot window.
;
; MODIFICATION HISTORY:
;   DMS, May, 1988.
;-

on_error,2                      ;Return to caller if an error occurs
sz = size(a)         ;Size of image
if sz[0] lt 2 then message, 'Parameter not 2D'

    ;set window used by contour


contour,[[0,0],[1,1]],/nodata, xstyle=4, ystyle = 4

px = !x.window * !d.x_vsize ;Get size of window in device units
py = !y.window * !d.y_vsize
swx = (px[1]-px[0])     ;Size in x in device units
swy = (py[1]-py[0])     ;Size in Y
six = float(sz[1])   ;Image sizes
siy = float(sz[2])
aspi = six / siy       ;Image aspect ratio
aspw = swx / swy       ;Window aspect ratio
f = aspi / aspw        ;Ratio of aspect ratios


loadct,0
if (!d.flags and 1) ne 0 then begin ;Scalable pixels?
  if keyword_set(aspect) then begin ;Retain aspect ratio?
          ;Adjust window size
    if f ge 1.0 then swy = swy / f else swx = swx * f
    endif

  tvscl,a,px[0],py[0],xsize = swx, ysize = swy, /device

endif else begin    ;Not scalable pixels
   if keyword_set(window_scale) then begin ;Scale window to image?
    tvscl,a,px[0],py[0]    ;Output image
    swx = six   ;Set window size from image
    swy = siy
    endif else begin       ;Scale window
    if keyword_set(aspect) then begin
       if f ge 1.0 then swy = swy / f else swx = swx * f
       endif     ;aspect
    tv,poly_2d(bytscl(a),$ ;Have to resample image
       [[0,0],[six/swx,0]], [[0,siy/swy],[0,0]],$
       keyword_set(interp),swx,swy), $
       px[0],py[0]
    endelse         ;window_scale
  endelse      ;scalable pixels

;mx = !d.n_colors-1   ;Brightest color
;colors = [mx,mx,mx,0,0,0]   ;color vectors
;if !d.name eq 'PS' then colors = mx - colors ;invert line colors for pstscrp

;loadct,14
device,decomposed=0
!p.color=0
!p.background=255
;loadct,13

contour_p=0
contour_n=0

contour_p=!d.table_size-2
contour_n=!d.table_size-3


tvlct,255,0,0,contour_p   ; color of v-positive contour
tvlct,0,0,255,contour_n   ; color of v-negative contour

if keyword_set(scale) then begin

    nb=congrid(b,sz(1)*scale(0),sz(2)*scale(1),/interp)

endif else begin

nb=b

endelse

contour,nb,/noerase,c_color=contour_n,$    ;Do the contour
       pos = [px[0],py[0], px[0]+swx,py[0]+swy],/dev,$
       levels=ncl,/xst,/yst,xtitle=xtitle,ytitle=ytitle,title=title,c_thick=c_thick,charthick=charthick,charsize=charsize ;$
       ;,xticks=1,yticks=1;,xtickinterval=600,ytickinterval=500

contour,nb,/noerase,c_color=contour_p,$    ;Do the contour
       pos = [px[0],py[0], px[0]+swx,py[0]+swy],/dev,$
       levels=pcl,/xst,/yst,c_thick=c_thick,charthick=charthick,charsize=charsize ;$
       ;,xticks=1,yticks=1;,xtickinterval=600,ytickinterval=500



loadct,0

return

end
