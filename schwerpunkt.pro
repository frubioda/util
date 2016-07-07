FUNCTION schwerpunkt, x, y, weight
;
;+
; NAME:
;      SCHWERPUNKT
; PURPOSE:
;       Calculates center-of-gravity (cog) of a structure
; CALLING SEQUENCE:
;       Result = schwerpunkt(x,y,[weight])
; INPUTS:
;       x = vector containing x-values
;       y = vector containig y-values
;       weight = vector containing 'mass' of each point (x,y)
; OUTPUTS:
;       Result = 2 elements array 
;        [0] = x coordinate of cog
;        [1] = y coordinate of cog
; COMMON BLOCKS:
;       none       
; SIDE EFFECTS:
;       
; RESTRICTIONS:
;             
; PROCEDURE:
;             
; MODIFICATION HISTORY:
;       19-Nov-00 J. Hirzberger, IGAM
;-
;

on_error, 2

nx = n_elements(x)
ny = n_elements(y)

IF nx NE ny THEN MESSAGE,'X and Y have different number of elements'

IF n_params() LT 3 THEN weight = replicate(1., nx) ELSE BEGIN

   weight = weight - min(weight)
   weight = weight/mean(weight)

ENDELSE

sx = total(x * weight)/total(weight)
sy = total(y * weight)/total(weight)

cog=[sx,sy]

RETURN, cog

END
       


 
  
