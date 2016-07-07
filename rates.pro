pro rates,pt,pn
;+
;   rates
;
;            calculates rates. pt(k,i,j,iel) contains total rate,
;            pn(k,i,j,iel)=net rate, positive=up, 
;            pn(k,j,i,iel)=relative net rate.
;
;-
 
 common headi0, mdep, mk, mel, mkh, mline, mwide, mrad, mrfix, $
    mq, mmu, meq, mdep1, mk1, mid, mtgrd, maxlu, $
    mxl, mjp, mspli
 common atmr0, gamma, grav, sumh, grph
 common atmor0, vturb
 common atmor1, cmass, tau, xnorm
 common atmoc0, atmoid, dpid, dptype
 common atomi0, nrad, nline, nrfix, ion, ilev, iellev, ktrans, ielrad, $
    cont, jrad, irad, iwide, krad, jfx, ifx, ipho, $
    ielfx, itrad
 common atomr0, qnorm, abnd, awgt, ev, g, f, ga, gw, $
    gq, alamb, a, bij, bji, hn3c2, hny4p
 common atomc0, label, atomid, crout
 common atomr1, nrb, alfa, alfac, totn, bp, nstar, a0, trad, $
    c, gij, dnyd, adamp
 common bdryr0, dzj0, dzjx, pgj0, ddj0, pgjx, ddjx, tgj0, denj0, $
    tgjx, denjx, cph
 common collr1, dcdne, dcdt
 common conr0, ee, hh, cc, bk, em, amu, hce, hc2, $
    hck, pi, pi2, pi4, pi4i, pi8i, c23, c43
 common diffr0, cmua, cmub, cmue, cmubc, ckapa, ceta
 common enrgr1, ek, ei, ei00, emg, emg00, eg, eg00, heatnr, $
    heatr, bfei, bfk, bfpu, bfv, bfc, bfmom, wp1, $
    wpt, wb1, wbt, dv, ftz, ftz0, dvisc, ddifus, $
    entrpy, wp0, wb0, bfv0, dv0, bfc0, bfm, tm00
 common eqni0, iz, iv, id, it, in, ine, isum
 common eqnc0, ceq
 common gausi0, nmu
 common gausr0, xmu, wmu
 common genci0, ntmp, il, ih, ielc, nid
 common gencr0, cgrd, tgrd
 common gencc0, key
 common grfnr0, w1grd1, w2grd1
 common grfni0, nz1grd, nz2grd, nnz1, nnz2
 common gridr0, zgd, tgd, vdamp, wnorm, wv, wp, wd, wt, $
    wne, wn, wv2, wp2, wd2, wt2, wne2, wn2
 common itpri0, intact, itmax, iconv
 common itprr0, elim1, elim2
 common lgmxr1, dlgtmx, taumin, taumax, tau0
 common newr1, z1, vz1, d1, tg1, n1, ne1, nh1, pg1, $
    fmj, gml, en1, coolt1, heat1, eion1
 common oldr1, z0, vz0, d0, tg0, n0, ne0, pg0, en0, $
    coolt0, eion0
 common old2r1, zm, vzm, dm, tgm, nm, nem, pgm, enm, $
    cooltm, eionm
 common optbr1, eion, dneion, dteion, nenh, dnnenh, dtnenh
 common outpi0, iprint, itape, iwemax, iwwmat, iwrest
 common outpr0, print, tape
 common outpc0, title
 common parmi0, ndep, ndepm, nk1, nel, nk
 common parmr0, zsize, dzavrg
 common pistr0, period, omega, amp
 common ropti0, njp, loopmx, nrec, loptab
 common roptr0, xl
 common linei0, nq, ind
 common liner0, qmax, q0, diff, thin, q, wq, wqmu, frq
 common liner1, wphi, sl, rij, rji, frad, outint, cool
 common splii0, ncheat
 common splir0, xcheat, cheat
 common swtcr1, crsw, fw0
 common trmwi0, noscat, fixt, dyn, idlsty
 common trmwr0, upw1w, upw2w, tw, advw, sourw
 common tim1r0, theta
 common tim1i1, iiter, idebug
 common tim1r1, dtn, dtnm, dtnm2, advt, curt
 common tim2i0, itimex, itime0, istart
 common tim2r0, timex, cstep, cstep2
 common tim2i1, itime, jqx
 common tim2r1, time, dtx, dtxq, cstep0, cstep1, dmaxq, cournt
 common trani0, itran

if(n_params(0) lt 2) then begin
  print,'rates,pt,pn'
  return
endif

print,'pt(k,i,j,iel)=total rate from level i to j'
print,'pn(k,i,j,iel)=net rate, positive=up'
print,'pn(k,j,i,iel)=net collisional rate, positive up'
 
pt=fltarr(ndep,mk,mk,nel)
pn=fltarr(ndep,mk,mk,nel)
 
for iel=0,nel-1 do begin
  for i=0,nk(iel)-1 do begin
    for j=0,nk(iel)-1 do begin
      pt(*,i,j,iel)=pt(*,i,j,iel)+n1(*,i,iel)*c(*,i,j,iel)
    endfor
  endfor
endfor
for kr=0,nrad-1 do begin
  i=irad(kr)-1
  j=jrad(kr)-1
  iel=ielrad(kr)-1
  pt(*,i,j,iel)=pt(*,i,j,iel)+n1(*,i,iel)*rij(*,kr)
  pt(*,j,i,iel)=pt(*,j,i,iel)+n1(*,j,iel)*rji(*,kr)
endfor
for iel=0,nel-1 do begin
  for j=1,nk(iel)-1 do begin
    for i=0,j-1 do begin
      pn(*,i,j,iel)=pt(*,i,j,iel)-pt(*,j,i,iel)
      pn(*,j,i,iel)=n1(*,i,iel)*c(*,i,j,iel)-n1(*,j,iel)*c(*,j,i,iel)
    endfor
  endfor
endfor
 
end
