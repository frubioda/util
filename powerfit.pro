;+
; NAME:
;  POWERFIT
;
; PURPOSE: 
;  Fitting a power-law plus a constant using the full covariance matrix
;
;  i.e. fitting Y = P0*X^P1 + P2
;
; CATEGORY:
;
;  Curve fitting
;
; CALLING SEQUENCE:
;  
;   Result = powerfit(INX,INY,COVAR,POWERFIX=powerfix,CONSTFIX=constfix,RR=rr,SVAL=sval,
;                     PREC=prec,STPOW=stpow,/CINVERT,OUTINVC=outinvc,YFIT=yfit, $
;                     CHISQR=chisqr)
;
; INPUTS:
;
;  INX: Array of independent variable values
;
;  INY: Array of dependent variable values
;
;  COVAR: N x N covariance matrix, where N is the size of the INX and INY array
;
; OPTIONAL INPUTS:
;
;  POWERFIX: Fixes the exponent of the power law at the value specified when calculating 
;            the fit
;
;  CONSTFIX: Fixes the constant at the value specified when calculating the fit
;
;  RR: ridge regression variable. Set it equal to the fraction of the median of the diagonal
;      elements of the covariance matrix to add to the diagonal before inverting. If both RR
;      and SVAL are set, RR overrides and only ridge regression is performed.
;
;  SVAL: singular value threshold for SVD conditioning. After performing an SVD on the
;        covariance matrix, all singular values below the threshold are set equal to zero
;        (as well as their inverses) before calculating the inversion.
;
;  PREC: precision used by AMOEBA for determining the exponent of the power-law fit
;        (Default = 1d-8)
;
;  STPOW: starting value used by AMOEBA for determining the exponent of the power-law fit
;         (Default = -0.58d)
;
; KEYWORD PARAMETERS:
;
;  CINVERT: when keyword is set, the input COVAR is assumed to be the already inverted 
;           covariance matrix and carries it through the calculation. Helpful if you are 
;           performing many fits with the same covariance matrix and don't want to keep 
;           recalculating the inversion. 
;
;
; OUTPUTS:
;  Outputs a 3 element array containing: [P0,P1,P2] as described under PURPOSE
;
; OPTIONAL OUTPUTS:
;
;  OUTINVC: output variable for the inverted covariance matrix
;  YFIT: output variable for the best fit model function
;  CHISQR: output variable for the chi^2 of the fit
;
; COMMON BLOCKS:
;
;  common PVAR - communicates the neccessary data to the function used by AMOEBA to
;                find the minimum chi^2
; 
;
; PROCEDURE:
;
;  Calculates the fit parameters where P0 and P2 (as defined under PURPOSE) are 
;  calculated analytically using standard linear regression formulae for a fixed 
;  exponent, P1. To fit for all three parameters simultaneously, it uses the 
;  function AMOEBA to search for the exponent value that minimizes chi^2.
;
;
; EXAMPLE:
;
; Fit with 3% ridge regression and a fixed exponent:
;  
;   params = powerfit(theta,wtheta,covar,rr=0.03,powerfix=-0.6)
;
; MODIFICATION HISTORY:
; 
;-

function covar_linfit, des, yin, invcov

;Function that performs the matrix algebra for a linear fit
;des - design matrix (in rows, not columns), yin - dependent variable
;invcov - inverted cov. matix

  dtd = des ## invcov ## transpose(des)
  dty = des ## invcov ## transpose(yin)

  if n_elements(dty) eq 1 then pout = (dty / dtd)[0] else begin
  
;The LA_INVERT function uses LU decomposition to compute the inverse of a square array 
;and is based on LAPACK routines. It is included in the IDL distribution.

    pout = la_invert(dtd, /double, status=stat) ## dty
    if stat ne 0 then print, 'dtd status:', stat
 
  endelse

  return, pout

end


function inpow_chisqr, inpow

;Function used by AMOEBA to find the exponent value corresponding to minimum chi^2
;Input - exponent value, output - chi^2

common pvar,  xcom, ycom, nxcom, invc, constcom, ncon

;Fixes problems caused if AMOEBA inputs an exponent very close to zero while searching for 
;minimum chi^2
  if abs(inpow[0]) lt 1e-8 then inpow[0] = -1e-4

;Determines the amplitude and constant for the input exponent value
  if ncon eq 1 then begin
    pa = covar_linfit(xcom^inpow[0], ycom-constcom, invc)
    yfit = pa*xcom^inpow[0] + constcom
  endif else begin
    pac = covar_linfit([[dblarr(nxcom)+1.],[xcom^inpow[0]]], ycom, invc)
    yfit = pac[1]*xcom^inpow[0] + pac[0]
  endelse

  outchisqr = ((ycom - yfit) ## invc ## transpose(ycom - yfit))[0]

  return, outchisqr

end


function invc_cond, incov, RR=rr, SVAL=sval

;Function that inverts the cov. matrix and conditions the matrix if keywords are used

  ncov = (size(incov))[1]

;Ridge regression conditioning
  if keyword_set(RR) then begin
  
;Adds a fraction (set by the value of RR) of the diagonal median to the diagonal elements 
;of the covariance matrix before inverting.

    ctmp = incov + rr*identity(ncov)*median(diag_matrix(incov),/even)
    outinv = la_invert(ctmp, /double, status=stat)
    if stat ne 0 then print, 'invc status:', stat

  endif else $

;SVD conditioning
  if keyword_set(SVAL) then begin
  
;SVDC performs a singular value decomposition of a matrix where (in this case):
;INCOV = UC ## diag_matrix(WC) ## transpose(VC) (WC is the 1-D array of singular values)
;SVDC is based on the routine svdcmp described in section 2.6 of Numerical Recipes in C
;(Second Edition)

    svdc, incov, wc, uc, vc
    whw = where(abs(wc) lt sval, ct)
    wt = 1. / wc
    if ct gt 0 then wt[whw] = 0.
    outinv = vc ## diag_matrix(wt) ## transpose(uc)

  endif else begin

;Inversion with no conditioning
    outinv = la_invert(incov, /double, status=stat)
    if stat ne 0 then print, 'invc status:', stat

  endelse

  return,outinv

end

function powerfit, inx, iny, covar, POWERFIX=powerfix, CONSTFIX=constfix, RR=rr, $
                                    SVAL=sval, PREC=prec, STPOW=stpow, CINVERT=cinvert, $
                                    OUTINVC=outinvc, YFIT=yfit, CHISQR=chisqr

  common pvar

  xcom = double(inx)
  ycom = double(iny)
  covtmp = double(covar)

  nxcom = n_elements(xcom)

;Inverts covariance matrix unless keyword CINVERT is set
  if keyword_set(cinvert) then invc = covtmp else invc = invc_cond(covtmp, rr=rr, sval=sval)

  ncon = n_elements(constfix)
  if ncon eq 1 then constcom = double(constfix)


  out = dblarr(3)

  if keyword_set(powerfix) then out[1] = double(powerfix) else begin

;If keyword powerfix is not set, AMOEBA searches for exponent corresponding to min. chi^2 
;(Make sure all inputs for AMOEBA are double precision) 
    if not keyword_set(stpow) then stpow = -0.58d else powst = double(stpow)
    if not keyword_set(prec)  then prec = 1d-8    else prec = double(prec)
 
;The AMOEBA function performs a minimization of a function using the downhill simplex 
;method and is based on the routine amoeba described in section 10.4 of Numerical Recipes
;in C (Second Edition). It is included in the IDL distribution.

    powtmp = amoeba(prec, function_name='inpow_chisqr', p0=stpow, scale=.1d)
    out[1] = powtmp[0]

  endelse

;Determines the amplitude and constant for the given exponent value
  if ncon eq 1 then begin
    pa = covar_linfit(xcom^out[1], ycom-constcom, invc)
    out[0] = pa
    out[2] = constcom
  endif else begin
    pac = covar_linfit([[dblarr(nxcom)+1.],[xcom^out[1]]], ycom, invc)
    out[0] = pac[1]
    out[2] = pac[0]
  endelse

  outinvc = invc
  yfit = out[0]*inx^out[1]+out[2]
  chisqr = ((ycom - yfit) ## invc ## transpose(ycom - yfit))[0]

  return, out

end