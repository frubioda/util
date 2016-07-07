;-----------------------------------------------------------------------------
;
;  HISTPLOT.PRO           Procedure making histogram-plot of array    
;                                                        C.Wahlstroem 9/3-92
;-----------------------------------------------------------------------------
;
pro histplot,yval,x,y,xx,yy,binsize=binsize,min=min,max=max,help=help
;
If (n_params() lt 1) Then Begin
   print,'histplot,yval xx,yy,binsize=binsize,min=min,max=max,/help'
   return
Endif
If (n_elements(binsize) eq 0) Then binsize=1
If (n_elements(max) eq 0) Then max=max(yval)
If (n_elements(min) eq 0) Then min=min(yval)
If (n_elements(help) ne 0) Then Begin
   Print,'Procedure plotting a histogram for a given array.'
   Print,' Keywords:'
   Print,'   Binsize; Size of bin. Default=1'
   Print,'   Max; Maximum value of plot. Default=maximum value of array'
   Print,'   Min; Minimum value of plot. Default=minimum value of array'
   Print,''
Endif
;
ans=''
yy=histogram(yval,binsize=binsize,max=max,min=min)
nn=n_elements(yy)
ia=indgen(nn)
mean=total(yval)/float(nn)
;
xx=fltarr(nn)
xx(ia)=min+ia*binsize
x=fltarr(nn*2)
x(ia*2)=xx(ia)
x(ia*2+1)=xx(ia+1)
x(nn*2-1)=xx(nn-1)+binsize
;
y=fltarr(nn*2)
y(ia*2)=yy(ia)
y(ia*2+1)=yy(ia)
;
;-------------------------------------------------------------------------
;
;  Plot on screen 
;
;
;   Plot on screen
;
  !p.multi=[0,1,1]
  window,1,xsize=450,ysize=600,title='Histogram'
  wset,1
  plot,x,y,xrange=[min,max+binsize],xstyle=1
;  xyouts,mean,max(yy)/2,'* (mean-value)'
;
 Read,'Do you want a hardcopy of this magnificent histogram (y/n)? ',ans
 If (ans eq 'y') or (ans eq 'Y') Then Begin
;
;   Plot to laser
;
  set_plot,'ps'
  device,file='histogram.fig'
  plot,x,y,xrange=[min,max+binsize],xstyle=1
;  xyouts,mean,max(yy)/2,'* (mean-value)'
  device,/close
  set_plot,'X'
  Print,'Printing to laser...'
  spawn,'ppr -Pastro histogram.fig'
 Endif
;-------------------------------------------------------------------------
END


