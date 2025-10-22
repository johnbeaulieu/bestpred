import numpy as np
from bestpred_h2 import *
from bestpred_log import *

def bestpred(
    mtrait,
    use3X,
    maxprnt,
    tstdays,
    dim,
    super,
    Xmilk,
    weigh,
    sample,
    MRD,
    yield_,
    herdavg,
    agefac,
    cowid,
    fresh,
    parity,
    length,
    DCRm,
    DCRc,
    DCRs,
    YLDvec,
    PERSvec,
    RELyld,
    RELpers,
    Yvec,
    bump,
    BLUPout,
    BLUPn,
    GRAFplot,
    DEBUGmsgs,
    ONscreen,
    laclen,
    dailyfreq,
    INTmethod,
    maxlen,
    DAILYbp,
    DAILYherd,
    WRITEcurve,
    CURVEfile,
    WRITEdata,
    DATAfile,
    plotfreq,
    INTmethodSCS,
    READparms,
    UNITSin,
    UNITSout,
    breedUNK,
    dim0,
    dim0flag,
    LOGon,
    LOGfile,
    LOGfreq,
    maxshow,
    herdstate,
    CURVEsmall,
    CURVEsingle,
    region,
    season,
):
    last = 2
    part = 1
    nbump = 0
    nbmp = 0
    lacn = 0
    month = 0
    ncall = 0
    precise = 1
    maxshow = maxshow
    maxbump = 0.0
    dyield = np.empty((0, 0, 0), dtype=float)
    lplus = 0.0
    meanyld = np.empty((0, 0, 0), dtype=float)
    covari = np.empty((0, 0, 0, 0, 0), dtype=float)
    corr305 = np.zeros((4, 4), dtype=float)
    covd = np.empty((0, 0, 0, 0, 0), dtype=float)
    meanp = np.zeros((2, 4), dtype=float)
    varp = np.zeros((2, 4), dtype=float)
    lacdif = 0.0
    sddif = 0.0
    stdvar = np.zeros((4, last), dtype=float)
    sd = np.empty((0, 0, 0), dtype=float)
    summ = 0.0
    dV1 = np.zeros((4, 4), dtype=float)
    dVd = np.zeros((4, 4), dtype=float)
    d0 = np.zeros((2, 4), dtype=float)
    vijkl = 0.0
    qpq = 0.0
    tdhi = 0.0
    tdlo = 0.0
    zero = 0.0
    dim0 = np.zeros(8, dtype=int)
    dim0wgts = np.array([0.285, 0.715], dtype=float)
    DAILYbp = np.zeros((2, 4, maxlen), dtype=float)
    DAILYherd = np.zeros((4, maxlen), dtype=float)
    herdstate = herdstate
    curve = np.empty((0, 0), dtype=float)
    curvem = np.empty((0, 0), dtype=float)
    graph1 = np.empty((0, 0), dtype=float)
    grafall = np.empty((0, 0, 0), dtype=float)
    curves = np.empty((0, 0, 0), dtype=float)
    freq3X = np.empty(0, dtype=float)
    std = np.empty((0, 0, 0), dtype=float)
    tempSTD = np.empty((0, 0, 0), dtype=float)
    stdslice = np.empty((0, 0), dtype=float)
    tempSTDslice = np.empty((0, 0), dtype=float)
    tempTD = np.empty((0, 0), dtype=float)
    cov305 = np.zeros((4, 200), dtype=float)
    covp305 = np.zeros((200, 4), dtype=float)
    covar305 = np.zeros((4, 200), dtype=float)
    vari305 = np.zeros((4, 4), dtype=float)
    multi305 = np.zeros((4, 1), dtype=float)
    cov365 = np.zeros((4, 200), dtype=float)
    covp365 = np.zeros((200, 4), dtype=float)
    covar365 = np.zeros((4, 200), dtype=float)
    vari365 = np.zeros((4, 4), dtype=float)
    multi365 = np.zeros((4, 1), dtype=float)
    cov999 = np.zeros((4, 200), dtype=float)
    covp999 = np.zeros((200, 4), dtype=float)
    covar999 = np.zeros((4, 200), dtype=float)
    vari999 = np.zeros((4, 4), dtype=float)
    multi999 = np.zeros((4, 1), dtype=float)
    single305 = np.zeros((4, 1), dtype=float)
    single365 = np.zeros((4, 1), dtype=float)
    single999 = np.zeros((4, 1), dtype=float)
    Fvalue = 0.0
    Fval = np.array(
        [
            [2.70555, 6.63495, 10.8277, 15.1369, 19.5118],
            [2.30260, 4.60521, 6.9079, 9.2105, 11.5132],
            [2.08381, 3.78166, 5.4222, 7.0360, 8.6341],
            [1.94487, 3.31921, 4.6168, 5.8783, 7.1185],
            [1.84728, 3.01729, 4.1031, 5.1491, 6.1714],
            [1.77412, 2.80202, 3.7430, 4.6428, 5.5180],
            [1.71673, 2.63937, 3.4746, 4.2683, 5.0371],
            [1.67021, 2.51131, 3.2656, 3.9786, 4.6666],
            [1.63153, 2.40737, 3.0975, 3.7468, 4.3713],
            [1.59873, 2.32096, 2.9589, 3.5565, 4.1298],
        ],
        dtype=float,
    )
    expamt = 0.6
    repty = np.array([0.55, 0.55, 0.55, 0.30], dtype=float)
    reptp = np.array([0.44, 0.44, 0.44, 0.20], dtype=float)
    reptyp = np.array([0.0, 0.0, 0.0, 0.0], dtype=float)
    X305 = np.array([1.0, 1.0, 1.0, 305.0], dtype=float)
    X100 = np.array([1.0, 1.0, 1.0, 100.0], dtype=float)
    q = np.zeros(6, dtype=int)
    qq = np.zeros((6, 200), dtype=int)
    size = 0
    i = 0
    j = 0
    k = 0
    l = 0
    m = 0
    n = 0
    i6 = 0
    brd = 0
    ntests = np.zeros(4, dtype=int)
    maxyld = 0.0
    minyld = 0.0
    middle = np.zeros(50, dtype=int)
    dimmilk = np.zeros(50, dtype=int)
    dimsort = np.zeros(50, dtype=int)
    mstart = 0
    mstop = 0
    kk = 0
    MRDi = 0
    testday = 0
    yearfr = 0
    oldbrd = 0
    usetd = 0
    dimvec = np.zeros(maxlen, dtype=int)
    cowid = cowid
    fresh = fresh
    brd1 = cowid[:1]
    if ONscreen == 1 and ncall == 0:
        print("--------------------------------------------------------------------------------")
        print("BESTPRED version ", "2.0rc7")
        print("    Released: ", "11/26/2014")
        print("    Support : ", "John B. Cole", " (john.cole@ars.usda.gov)")
        print("    Website : ", "http://www.aipl.arusda.gov/software/bestpred/")
        print("--------------------------------------------------------------------------------")
    if DEBUGmsgs > 1:
        print("Allocating memory")
    if ncall == 0:
        if LOGon == 1 and LOGfreq > 0:
            LogMessage = "Allocating matrices for means, SD, and covariances"
            log_message(LOGfile, LogMessage)
        dyield = np.zeros((maxlen, 4, last), dtype=float)
        meanyld = np.zeros((4, maxlen, last), dtype=float)
        sd = np.zeros((4, maxlen, last), dtype=float)
        covari = np.zeros((4, 4, maxlen, maxlen, last), dtype=float)
        covd = np.zeros((4, 4, maxlen, maxlen, last), dtype=float)
    if LOGon == 1 and LOGfreq > 0 and ncall % LOGfreq == 0:
        LogMessage = "Allocating arrays in call " + str(ncall)
        log_message(LOGfile, LogMessage)
    curve = np.zeros((maxlen, 200), dtype=float)
    curves = np.zeros((maxlen, 200, 4), dtype=float)
    curvem = np.zeros((maxlen, 200), dtype=float)
    graph1 = np.zeros((maxlen, 1), dtype=float)
    grafall = np.zeros((2, maxlen, 4), dtype=float)
    freq3X = np.zeros(maxlen, dtype=float)
    std = np.zeros((2, 4, maxlen), dtype=float)
    stdslice = np.zeros((1, maxlen), dtype=float)
    tempTD = np.zeros((4, maxlen), dtype=float)
    tempSTD = np.zeros((2, 4, maxlen), dtype=float)
    tempSTDslice = np.zeros((1, maxlen), dtype=float)
    fatpct = np.full(maxlen, "    ", dtype="<U4")
    propct = np.full(maxlen, "    ", dtype="<U4")
    scs = np.full(maxlen, "    ", dtype="<U4")
    tests = np.full(maxlen // plotfreq, "    ", dtype="<U4")
    dimvec = np.zeros(maxlen, dtype=int)
    std.fill(-999.0)
    tempTD.fill(-999.0)
    tempSTD.fill(-999.0)
    DAILYbp.fill(-999.0)
    DAILYherd.fill(0.0)
    sum305 = np.zeros(4, dtype=float)
    sum365 = np.zeros(4, dtype=float)
    sum999 = np.zeros(4, dtype=float)
    sumpart = np.zeros(4, dtype=float)

    if DEBUGmsgs > 1:
        print("UNITSin: ", UNITSin)
        print("UNITSout: ", UNITSout)
    if UNITSin == "P":
        if LOGon == 1 and LOGfreq > 0 and ncall % LOGfreq == 0:
            LogMessage = "Converting from lbs to kg for internal calculations"
            log_message(LOGfile, LogMessage)
        yield_[:, 0, :] /= 2.205
        herdavg[:, :3] /= 2.205
        if ncall == 0:
            dyld[:, :3, :] /= 2.205
            dsd[:, :3, :] /= 2.205
            brdyld[:, :3, :] /= 2.205
            brdsd[:, :3] /= 2.205
    if dim0flag == 1:
        dim0 = np.zeros(8, dtype=int)
        d0.fill(0.0)
    else:
        d0.fill(0.0)
        for j in range(4):
            d0[0, j] = dim0[j]
            d0[1, j] = dim0[4 + j]
    GRAFplotMIXED = 0
    GRAFplotSUM = np.sum(GRAFplot)
    if GRAFplotSUM > 0:
        for j in range(4):
            if j == 0 and GRAFplot[j] == 1:
                GRAFplotMIXED = 1
            if j == 0 and GRAFplot[j] == 2:
                GRAFplotMIXED = 2
            if j > 0 and GRAFplot[j] == 1 and GRAFplotMIXED == 2:
                GRAFplotMIXED = 3
            if j > 0 and GRAFplot[j] == 2 and GRAFplotMIXED == 1:
                GRAFplotMIXED = 3
    if ncall == 0 and DEBUGmsgs > 0:
        print("[bestpred]: Tipping points (1): ", dim0[:4])
        print("[bestpred]: Tipping points (2): ", dim0[4:8])
    if DEBUGmsgs > 0:
        print("======================================================")
        print("Inputs to bestpred for cow=", cowid, " fresh=", fresh)
        print("       tstdays=", tstdays, " parity=", parity, " length=", length, " maxprnt=", maxprnt)
        print(" dim sup frq way sam mrd milk   fat  prot   scs")
        for j in range(min(tstdays, 50)):
            if UNITSin == "P":
                print(dim[j], super[j], Xmilk[j], weigh[j], sample[j], MRD[j], yield_[0, 0, j] * 2.205, yield_[0, 1:4, j])
            else:
                print(dim[j], super[j], Xmilk[j], weigh[j], sample[j], MRD[j], yield_[0, :, j])
        if tstdays > 50:
            print(tstdays, " tests but", 50, " used")
        if UNITSin == "P":
            print("Herd average mlk fat pro scs =", herdavg[:, :3] * 2.205, herdavg[:, 3] * 100)
        else:
            print("Herd average mlk fat pro scs =", herdavg)
        print("======================================================")
    if DEBUGmsgs > 1:
        print("======================================================")
        print("John's arguments")
        print("    GRAFplot     :", GRAFplot)
        print("    DEBUGmsgs    :", DEBUGmsgs)
        print("    ONscreen     :", ONscreen)
        print("    laclen       :", laclen)
        print("    dailyfreq    :", dailyfreq)
        print("    INTmethod    :", INTmethod)
        print("    maxlen       :", maxlen)
        print("    WRITEcurve   :", WRITEcurve)
        print("    CURVEfile    :", CURVEfile)
        print("    WRITEdata    :", WRITEdata)
        print("    DATAfile     :", DATAfile)
        print("    plotfreq     :", plotfreq)
        print("    INTmethodSCS :", INTmethodSCS)
        print("    READparms    :", READparms)
        print("    UNITSin      :", UNITSin)
        print("    UNITSout     :", UNITSout)
        print("    breedUNK     :", breedUNK)
        print("    dim0         :", dim0)
        print("    dim0flag     :", dim0flag)
        print("    LOGon        :", LOGon)
        print("    LOGfile      :", LOGfile)
        print("    LOGfreq      :", LOGfreq)
        print("    agefac       :", agefac)
        print("======================================================")
    brd = breed6.find(brd1)
    if brd == -1:
        if DEBUGmsgs == 1:
            print("[WARNING]: Unknown breed ", brd1, "provided; defaulting to ", breedUNK, " (", breed[breedUNK], ").")
        if LOGon == 1 and LOGfreq > 0 and ncall % LOGfreq == 0:
            LogMessage = "Unknown breed " + str(brd) + " provided; defaulting to " + str(breedUNK) + " (" + breed[breedUNK] + ")"
            log_message(LOGfile, LogMessage)
        brd = breedUNK
    if (INTmethod == "L" or INTmethod == "W") and (INTmethodSCS == "L" or INTmethodSCS == "G"):
        herdstate = "00"
    if herdstate == "00":
        region = 2
        season = 1
    else:
        state_to_region(herdstate, region, DEBUGmsgs)
        date_to_season(fresh, season, DEBUGmsgs)
    if ncall == 0:
        oldbrd = brd
        oldregion = region
        oldseason = season
    if ncall > 0 and brd == oldbrd and dim0flag == 0 and season == oldseason and region == oldregion:
        goto_20 = True
    else:
        goto_20 = False
    if not goto_20:
        if maxprnt > 0 and ncall == 0:
            print("Lactation yields are MATURE (adjusted for age, etc.) and ACTUAL (unadjusted) values")
            if GRAFplotMIXED == 0:
                print("No lactation curves are being plotted.")
            if GRAFplotMIXED == 1:
                print("Plotted lactation curves are MATURE (adjusted for age, etc.)")
            if GRAFplotMIXED == 2:
                print("Plotted lactation curves are ACTUAL (unadjusted for age, etc.)")
            if GRAFplotMIXED == 3:
                print("Plotted lactation curves are a mix of MATURE (adjusted for age, etc.) and ACTUAL (unadjusted) values")
            if mtrait > 1:
                print("Multi-trait methods include data for", trt[:mtrait])
            else:
                print("!! Only single-trait methods were used !!")
            if use3X == 0:
                print("3X adjustments were not used")
            if use3X == 1:
                print("Old 3X factors were used")
            if use3X == 2:
                print("New 3X factors were used")
            if use3X == 3:
                print("New 3X factors phased in over time")
        if nbump > 0:
            if maxprnt > 0:
                print("DCR was reduced for bumpiest 1 /", 10**nbump, "lacts")
            nbmp = nbump
            maxbump = 1.0
        else:
            maxbump = 99.0
            if maxprnt > 0 and ncall == 0:
                print("DCRs were not adjusted for bumpiness")
        for lacn in range(last, 0, -1):
            for j in range(4):
                if j <= 2:
                    if DEBUGmsgs > 0:
                        print("[bestpred]: Calling interpolate for trait ", j + 1, " with method ", INTmethod, " and breed ", brd, " and region ", region, " and season ", season)
                    if LOGon == 1 and LOGfreq > 0 and ncall % LOGfreq == 0:
                        LogMessage = (
                            "[bestpred]: Calling interpolate(): trait "
                            + str(j + 1)
                            + ", method "
                            + INTmethod
                            + ", breed "
                            + str(brd)
                            + ", region "
                            + str(region)
                            + ", and season "
                            + str(season)
                        )
                        log_message(LOGfile, LogMessage)
                    interpolate(j, month, dyield, lacn, dyld, summ, meanyld, meanp, sd, dsd, INTmethod, DEBUGmsgs, maxlen, brd, region, season)
                else:
                    if LOGon == 1 and LOGfreq > 0 and ncall % LOGfreq == 0:
                        LogMessage = (
                            "[bestpred]: Calling interpolate(): trait "
                            + str(j + 1)
                            + ", method "
                            + INTmethodSCS
                            + ", breed "
                            + str(brd)
                            + ", region "
                            + str(region)
                            + ", and season "
                            + str(season)
                        )
                        log_message(LOGfile, LogMessage)
                    if DEBUGmsgs > 0:
                        print(
                            "[bestpred]: Calling interpolate for trait ", j + 1, " with method ", INTmethodSCS, " and breed ", brd, " and region ", region, " and season ", season
                        )
                    interpolate(j, month, dyield, lacn, dyld, summ, meanyld, meanp, sd, dsd, INTmethodSCS, DEBUGmsgs, maxlen, brd, region, season)
            for j in range(4):
                stdvar[j, lacn - 1] = covary(j, 0, 0, 305, maxlen, 1, covari, covd, sd, lacn, last, precise, 1)
            lplus = (precise - 1) / 2.0
            for j in range(4):
                for k in range(4):
                    corr305[j, k] = 0.0
                    dV1[j, k] = 0.0
                    dVd[j, k] = 0.0
                    for i in range(305):
                        for l in range(1, 306, precise):
                            vijkl = vary(i + 1, j + 1, 1, 2, 2, 1, l, k + 1, 1, 2, 2, 1, sd, lacn, last, maxlen)
                            vijkl *= min(precise, 306 - l)
                            corr305[j, k] += vijkl
                            dV1[j, k] += (i + 1) * vijkl
                            dVd[j, k] += (i + 1) * (l + lplus) * vijkl
                            if dim0flag == 1:
                                dim0[4 * (lacn - 1) + j] = dV1[j, j] / stdvar[j, lacn - 1]
                                d0[lacn - 1, j] = dim0[4 * (lacn - 1) + j]
            for j in range(4):
                meanp[lacn - 1, j] -= d0[lacn - 1, j] * meanyld[j, 305 - 1, lacn - 1]
                varp[lacn - 1, j] = dVd[j, j] - 2 * dV1[j, j] * d0[lacn - 1, j] + stdvar[j, lacn - 1] * d0[lacn - 1, j] ** 2
                for k in range(4):
                    for l in range(maxlen):
                        covd[j, k, l, 305 - 1, lacn - 1] = (covd[j, k, l, 305 - 1, lacn - 1] - covari[j, k, l, 305 - 1, lacn - 1] * d0[lacn - 1, j]) / np.sqrt(varp[lacn - 1, j])
            for i in range(305):
                tdhi = dyield[i, 0, lacn - 1] + tdregr
                tdlo = dyield[i, 0, lacn - 1] - tdregr
                qpq += (i + 1 - d0[lacn - 1, 0]) ** 2
            if maxprnt > 0 and ncall == 0:
                if lacn == last:
                    print("=======================" "==============================================")
                if lacn == last:
                    print("Breed ", brd1, " Lact  Mean, " "St.Dev.   Max.DCR   Lactation Corr.        mid-DIM")
                if lacn < last:
                    plus = " "
            for j in range(4):
                lacdif = meanyld[j, 305 - 1, lacn - 1] / meanyld[j, 305 - 1, last - 1]
                zero = 0.0
                sddif = np.sqrt(stdvar[j, lacn - 1] / stdvar[j, last - 1])
                if maxprnt > 0 and ncall == 0:
                    if UNITSout == "P":
                        print(
                            trt[j + 4],
                            lacn,
                            plus,
                            brdyld[brd, j, 1] * X100[j] * lacdif * 2.205,
                            brdsd[brd, j] * X100[j] * sddif * 2.205,
                            100.0 / rmonth[j, lacn - 1],
                            (corr305[j, k] / np.sqrt(stdvar[j, lacn - 1] * stdvar[k, lacn - 1]) for k in range(4)),
                            d0[lacn - 1, j],
                        )
                    else:
                        print(
                            trt[j + 4],
                            lacn,
                            plus,
                            brdyld[brd, j, 1] * X100[j] * lacdif,
                            brdsd[brd, j] * X100[j] * sddif,
                            100.0 / rmonth[j, lacn - 1],
                            (corr305[j, k] / np.sqrt(stdvar[j, lacn - 1] * stdvar[k, lacn - 1]) for k in range(4)),
                            d0[lacn - 1, j],
                        )
        if dim0flag == 1:
            print("New tipping points (1): ", dim0[:4])
            print("New tipping points (2): ", dim0[4:8])
        if oldbrd != brd:
            oldbrd = brd
        if oldregion != region:
            oldregion = region
        if oldseason != season:
            oldseason = season
        if maxprnt > 0 and ncall == 0:
            print("===========================================" "==========================")
        if GRAFplot[0] > 0 or GRAFplot[1] > 0 or GRAFplot[2] > 0 or GRAFplot[3] > 0:
            print("Compute DCR, predict yield of cow ---, graph contemps ...")
            print("Supervised =S    ampm1of2=A  2of3=B  1of3=C" " ver=V  LER=L dup=D")
            print("OwnerSample=O  OSampm1of2=P  2of3=Q  1of3=R" "  ownerLER=U, bad=X")
            print("Capital letters if milk was sampled, small if milk-only")
    ncall += 1
    lacn = min(parity, last)
    if lacn <= 0:
        lacn = last
    brd1 = cowid[:1]
    if length > maxlen:
        length = maxlen
    if tstdays <= 0:
        ntd = 1
        dim[0] = maxlen + 1
    else:
        ntd = tstdays
    if ntd > 50:
        ntd = 50
    dimvec.fill(0)
    MRDi = 0
    for i in range(ntd):
        if dim[i] <= maxlen:
            if dim[i] < 1:
                print("Bad DIM", dim[i], " cow ", cowid, " test", i + 1)
            else:
                if dim[i] - MRD[i] < 0:
                    MRD[i] = dim[i]
                MRDi = MRD[i]
                for k in range(MRDi):
                    if dimvec[dim[i] - k] > 0:
                        if MRDi == 1:
                            ndup += 1
                            if ndup <= 10:
                                print("Warning: cow ", cowid, " has duplicate tests " "lact", parity, " day", dim[i])
                        else:
                            overlaps += 1
                            if overlaps <= 10:
                                print("Warning: cow ", cowid, " LER segments overlap" " lact", parity, " day", dim[i] - k)
                            MRD[i] = k
                            break
                    else:
                        dimvec[dim[i] - k] = i + 1
    yearfr = int(fresh[:4])
    adjust3X(ntd, dim, length, yearfr, parity, Xmilk, meanyld, test3X, fact3X, part3X, use3X, 50, last, maxlen)
    for j in range(4):
        if herdavg[0, j] <= 0.0:
            herdavg[0, j] = brdyld[brd, j, lacn - 1]
        herd305[0, j] = herdavg[0, j] * X305[j]
        herd305[0, j] *= fact3X[j]
        if herd305[0, j] <= 0.0:
            if yearfr < 2000:
                herd305[0, j] = brdyld[brd, j, 0] * X305[j]
            if yearfr >= 2000:
                herd305[0, j] = brdyld[brd, j, 1] * X305[j]
        hratio[j] = herd305[0, j] / meanyld[j, 305 - 1, lacn - 1]
        for i in range(maxlen):
            std[0, j, i] = ymean(j, i + 1, maxlen, 1, dyield, lacn, last, hratio, herd305[0, j])
            if i + 1 <= 305:
                sum305[j] += std[0, j, i]
            if i + 1 <= 365:
                sum365[j] += std[0, j, i]
            if i + 1 <= laclen:
                sum999[j] += std[0, j, i]
            if i + 1 <= length:
                sumpart[j] += std[0, j, i]
        if herdavg[0, j] <= 0.0:
            herd305[0, j] *= herd305[0, j] / sum305[j]
        herd365[0, j] = herd305[0, j] * (sum365[j] / sum305[j])
        herd999[0, j] = herd305[0, j] * (sum999[j] / sum305[j])
        herdpart[0, j] = herd305[0, j] * (sumpart[j] / sum305[j])
        varfac[j] = brdsd[brd, j] * 305.0 / np.sqrt(stdvar[j, lacn - 1])
        maxyield[j] = ymean(j, 60, maxlen, 1, dyield, lacn, last, hratio, herd305) / (agefac[j] * part3X[j])
        minyield[j] = ymean(j, maxlen, maxlen, 1, dyield, lacn, last, hratio, herd305) / (agefac[j] * part3X[j])
        if j == 1 or j == 2:
            minyield[j] *= minyield[0] * 10
            maxyield[j] *= minyield[0] * 10
        if j == 3:
            minyield[j] *= 10
            maxyield[j] *= 10
        if j == 3:
            k = minyield[j]
            minyield[j] = maxyield[j]
            maxyield[j] = k
    maxyld = ymean(0, 60, maxlen, 1, dyield, lacn, last, hratio, herd305[0, :]) / (agefac[0] * part3X[0])
    minyld = ymean(0, maxlen, maxlen, 1, dyield, lacn, last, hratio, herd305[0, :]) / (agefac[0] * part3X[0])
    size = 0
    if mtrait > 1:
        if DEBUGmsgs > 0:
            print("Doing MT prediction, mtrait= ", mtrait)
        size = 0
        ntests = 0
        usetd = 0
        for i in range(ntd):
            if dim[i] < 1 or dim[i] > maxlen or dimvec[dim[i]] != i + 1:
                continue
            usetd += 1
            dimsort[usetd - 1] = dim[i]
            if sample[i] > Xmilk[i]:
                sample[i] = Xmilk[i]
            if MRD[i] > 1:
                weigh[i] = Xmilk[i]
            if j == 0:
                dimsort[i] = dim[i]
            for j in range(4):
                if weigh[i] < 1:
                    yield_[0, j, i] = zero
                if sample[i] < 1:
                    yield_[0, 1, i] = zero
                    yield_[0, 2, i] = zero
                    if yield_[0, 3, i] > zero:
                        sample[i] = 1
                if yield_[0, j, i] <= zero:
                    continue
                if j == 1:
                    yield_[0, j, i] = yield_[0, 0, i] * yield_[0, j, i] / 100.0
                if j == 2:
                    yield_[0, j, i] = yield_[0, 0, i] * yield_[0, j, i] / 100.0
                if yield_[0, 0, i] > maxyld:
                    maxyld = yield_[0, 0, i]
                if yield_[0, 0, i] < minyld:
                    minyld = yield_[0, 0, i]
                if j > mtrait:
                    continue
                if size == 200:
                    print(" Too much data (size > 200) for cow ", cowid)
                    continue
                size += 1
                if dim[i] <= maxlen:
                    ntests[j] += 1
                    if j == 0:
                        ntests[0] += MRD[i] - 1
                q[0] = dim[i]
                q[1] = j + 1
                q[2] = super[i]
                q[3] = Xmilk[i]
                if j == 0:
                    q[4] = weigh[i]
                if j > 0:
                    q[4] = sample[i]
                q[5] = MRD[i]
                qq[:, size - 1] = q[:]
                dev[size - 1, 0] = yield_[0, j, i] * test3X[j, i] - ymean(j, dim[i], maxlen, MRD[i], dyield, lacn, last, hratio, herd305[0, :]) / agefac[j]
                if yield_[0, j, i] != 0.0:
                    tempTD[j, dim[i] - 1] = yield_[0, j, i]
                for k in range(size):
                    kk = qq[1, k]
                    var[size - 1, k] = (
                        vary(q[0], q[1], q[2], q[3], q[4], q[5], qq[0, k], qq[1, k], qq[2, k], qq[3, k], qq[4, k], qq[5, k], sd, lacn, last, maxlen)
                        * varfac[j]
                        * varfac[kk - 1]
                        / (agefac[j] * agefac[kk - 1])
                    )
                    var[k, size - 1] = var[size - 1, k]
                for m in range(maxlen // dailyfreq):
                    for l in range(4):
                        curves[m, size - 1, l] = (
                            vary(q[0], q[1], q[2], q[3], q[4], q[5], (m + 1) * dailyfreq, l + 1, 1, 2, 2, 1, sd, lacn, last, maxlen)
                            * varfac[j]
                            * varfac[l]
                            / (agefac[j] * agefac[l])
                        )
                for k in range(4):
                    cov305[k, size - 1] = covary(k + 1, j + 1, dim[i], 305, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 1) * varfac[j] * varfac[k] / agefac[j]
                    covp305[size - 1, k] = cov305[k, size - 1]
                    cov365[k, size - 1] = covary(k + 1, j + 1, dim[i], 365, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 1) * varfac[j] * varfac[k] / agefac[j]
                    covp365[size - 1, k] = cov365[k, size - 1]
                    if laclen != 305 and laclen != 365:
                        cov999[k, size - 1] = covary(k + 1, j + 1, dim[i], laclen, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 1) * varfac[j] * varfac[k] / agefac[j]
                        covp999[size - 1, k] = cov999[k, size - 1]
                    dcov[k, size - 1] = covary(k + 1, j + 1, dim[i], 305, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 2) * varfac[j] * varfac[k] / agefac[j]
                    dcovp[size - 1, k] = dcov[k, size - 1]
                    covsum[k, size - 1] = 0.0
                    if part == 0:
                        continue
                    for m in range(length):
                        covsum[k, size - 1] += vary(q[0], q[1], q[2], q[3], q[4], q[5], m + 1, k + 1, 1, 2, 2, 1, sd, lacn, last, maxlen) * varfac[j] * varfac[k] / agefac[j]
        binsort(dimsort, usetd, 50)
        for k in range(usetd):
            dimmilk[k] = dimsort[k]
        if DEBUGmsgs > 1:
            print("[bestpred]: Test day data for milk used to calculate deviations.")
            print("")
            print("dev               yield    test3X   ymean    hratio   herd305    agefac")
            print("-----------------------------------------------------------------------")
            for i in range(ntd):
                j = 0
                print(dev[i, 0], yield_[0, j, i], test3X[j, i], ymean(j, dim[i], maxlen, MRD[i], dyield, lacn, last, hratio, herd305[0, :]), hratio[j], herd305[0, j], agefac[j])
        if size == 0:
            for l in range(4):
                vari[l, l] = 0.0
                dvari[l, l] = 0.0
                vari305[l, l] = 0.0
                vari365[l, l] = 0.0
                vari999[l, l] = 0.0
                partrec[l, 0] = 0.0
                multi[l, 0] = 0.0
                multi305[l, 0] = 0.0
                multi365[l, 0] = 0.0
                multi999[l, 0] = 0.0
                persist[l, 0] = 0.0
            graph1.fill(0.0)
        else:
            invrt2(var, 200, size, cowid)
            mult(covar305, cov305, var, 4, size, size, 4, 200, 200)
            mult(vari305, covar305, covp305, 4, size, 4, 4, 200, 4)
            mult(multi305, covar305, dev, 4, size, 1, 4, 200, 1)
            mult(covar365, cov365, var, 4, size, size, 4, 200, 200)
            mult(vari365, covar365, covp365, 4, size, 4, 4, 200, 4)
            mult(multi365, covar365, dev, 4, size, 1, 4, 200, 1)
            if laclen == 305:
                multi999 = multi305
            elif laclen == 365:
                multi999 = multi365
            else:
                mult(covar999, cov999, var, 4, size, size, 4, 200, 200)
                mult(vari999, covar999, covp999, 4, size, 4, 4, 200, 4)
                mult(multi999, covar999, dev, 4, size, 1, 4, 200, 1)
            mult(curvem, curves, var, maxlen // dailyfreq, size, size, maxlen, 200, 200)
            mult(graph1, curvem, dev, maxlen // dailyfreq, size, 1, maxlen, 200, 1)
            for j in range(mtrait):
                for l in range(maxlen // dailyfreq):
                    for i in range(size):
                        curve[l, i] = curves[l, i, j]
                mult(curvem, curve, var, maxlen // dailyfreq, size, size, maxlen, 200, 200)
                mult(graph1, curvem, dev, maxlen // dailyfreq, size, 1, maxlen, 200, 1)
                grafall[0, :, j] = graph1[:, 0]
                DAILYbp[0, j, :].fill(-999.0)
                for l in range(maxlen // dailyfreq):
                    DAILYbp[0, j, l] = graph1[l, 0]
            if part == 1:
                mult(partial, covsum, var, 4, size, size, 4, 200, 200)
                mult(partrec, partial, dev, 4, size, 1, 4, 200, 1)
            mult(covar305, dcov, var, 4, size, size, 4, 200, 200)
            mult(dvari, covar305, dcovp, 4, size, 4, 4, 200, 4)
            mult(persist, covar305, dev, 4, size, 1, 4, 200, 1)
        for l in range(4):
            YLDvec[0, l] = multi305[l, 0] + herd305[0, l]
            YLDvec[0, 4 + l] = multi365[l, 0] + herd365[0, l]
            YLDvec[0, 8 + l] = multi999[l, 0] + herd999[0, l]
            if part == 0 or mtrait < l + 1 or length == 0:
                YLDvec[0, 12 + l] = 0.0
                continue
            YLDvec[0, 12 + l] = partrec[l, 0] + meanyld[l, length - 1, lacn - 1] * hratio[l]
            RELyld[l] = vari305[l, l] / (stdvar[l, lacn - 1] * varfac[l] ** 2)
            regrel[l] = RELyld[l]
            DCRvec[l] = 100.0 * RELyld[l] / rmonth[l, lacn - 1]
            PERSvec[l] = persist[l, 0]
            RELpers[l] = dvari[l, l] / varfac[l] ** 2
            xmulti[l] = 1.0 / RELyld[l]
    if mtrait < 4:
        if mtrait == 1:
            st_start = 0
        elif mtrait == 3:
            st_start = 3
        else:
            print("[ERROR]: Unrecognized value of mtrait (1|3|4): ", mtrait)
        for j in range(st_start, 4):
            ntests[j] = 0
            size = 0
            for i in range(ntd):
                if dim[i] < 1 or dim[i] > maxlen or dimvec[dim[i]] != i + 1:
                    continue
                if sample[i] > Xmilk[i]:
                    sample[i] = Xmilk[i]
                if MRD[i] > 1:
                    weigh[i] = Xmilk[i]
                if weigh[i] < 1:
                    yield_[0, j, i] = zero
                if sample[i] < 1:
                    yield_[0, 1, i] = zero
                    yield_[0, 2, i] = zero
                    if yield_[0, 3, i] > zero:
                        sample[i] = 1
                if yield_[0, j, i] <= zero:
                    continue
                if j == 1:
                    yield_[0, j, i] = yield_[0, 0, i] * yield_[0, j, i] / 100.0
                if j == 2:
                    yield_[0, j, i] = yield_[0, 0, i] * yield_[0, j, i] / 100.0
                if yield_[0, 0, i] > maxyld:
                    maxyld = yield_[0, 0, i]
                if yield_[0, 0, i] < minyld:
                    minyld = yield_[0, 0, i]
                if size == 200:
                    print(" Too much data (size > 200) for cow ", cowid)
                    continue
                size += 1
                dimsort[size - 1] = dim[i]
                if dim[i] <= maxlen:
                    ntests[j] += 1
                    if j == 0:
                        ntests[0] += MRD[i] - 1
                q[0] = dim[i]
                q[1] = j + 1
                q[2] = super[i]
                q[3] = Xmilk[i]
                if j == 0:
                    q[4] = weigh[i]
                if j > 0:
                    q[4] = sample[i]
                q[5] = MRD[i]
                qq[:, size - 1] = q[:]
                dev[size - 1, 0] = yield_[0, j, i] * test3X[j, i] - ymean(j, dim[i], maxlen, MRD[i], dyield, lacn, last, hratio, herd305[0, :]) / agefac[j]
                for k in range(size):
                    kk = qq[1, k]
                    var[size - 1, k] = (
                        vary(q[0], q[1], q[2], q[3], q[4], q[5], qq[0, k], qq[1, k], qq[2, k], qq[3, k], qq[4, k], qq[5, k], sd, lacn, last, maxlen)
                        * varfac[j] ** 2
                        / agefac[j] ** 2
                    )
                    var[k, size - 1] = var[size - 1, k]
                if yield_[0, j, i] != 0.0:
                    tempTD[j, dim[i] - 1] = yield_[0, j, i]
                for m in range(maxlen // dailyfreq):
                    curve[m, size - 1] = vary(q[0], q[1], q[2], q[3], q[4], q[5], (m + 1) * dailyfreq, j + 1, 1, 2, 2, 1, sd, lacn, last, maxlen) * varfac[j] ** 2 / agefac[j] ** 2
                cov305[0, size - 1] = covary(j + 1, j + 1, dim[i], 305, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 1) * varfac[j] ** 2 / agefac[j]
                covp305[size - 1, 0] = cov305[0, size - 1]
                cov365[0, size - 1] = covary(j + 1, j + 1, dim[i], 365, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 1) * varfac[j] ** 2 / agefac[j]
                covp365[size - 1, 0] = cov365[0, size - 1]
                if laclen != 305 and laclen != 365:
                    cov999[0, size - 1] = covary(j + 1, j + 1, dim[i], laclen, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 1) * varfac[j] ** 2 / agefac[j]
                    covp999[size - 1, 0] = cov999[0, size - 1]
                dcov[0, size - 1] = covary(j + 1, j + 1, dim[i], 305, maxlen, MRD[i], covari, covd, sd, lacn, last, precise, 2) * varfac[j] ** 2 / agefac[j]
                dcovp[size - 1, 0] = dcov[0, size - 1]
                Zdev[i] = dev[size - 1, 0] * 305 / (X305[j] * np.sqrt(var[size - 1, size - 1]))
                covsum[0, size - 1] = 0.0
                if part == 0:
                    continue
                for m in range(length):
                    covsum[0, size - 1] += vary(q[0], q[1], q[2], q[3], q[4], q[5], m + 1, j + 1, 1, 2, 2, 1, sd, lacn, last, maxlen) * varfac[j] ** 2 / agefac[j]
            binsort(dimsort, size, 50)
            bump[j] = 0.0
            bumpsd[j] = 0.0
            varb[j] = 0.0
            if size > 1:
                for k in range(1, size):
                    Zdif = Zdev[dimvec[dimsort[k]]] - Zdev[dimvec[dimsort[k - 1]]]
                    bigbump[j] = max(bump[j], abs(Zdif))
                    bumpsd[j] += Zdif**2
                    varb[j] += 2.0 * (1.0 - var[k, k - 1] / np.sqrt(var[k, k] * var[k - 1, k - 1]))
                bump[j] = bumpsd[j] / varb[j]
                if bump[j] < 0:
                    bump[j] = 0.0
                if bump[j] > 99.0:
                    bump[j] = 99.0
            if j == 0:
                usetd = size
                for k in range(size):
                    dimmilk[k] = dimsort[k]
                regrel[0] = RELyld[0]
            xsingl[j] = 0.0
            lacwt[j] = 0.0
            partrec[j, 0] = 0.0
            if size == 0:
                vari[0, 0] = 0.0
                dvari[0, 0] = 0.0
                vari305[0, 0] = 0.0
                vari365[0, 0] = 0.0
                vari999[0, 0] = 0.0
                partrec[0, 0] = 0.0
                single305[0, 0] = 0.0
                single365[0, 0] = 0.0
                single999[0, 0] = 0.0
                pers[0, 0] = 0.0
                graph1.fill(0.0)
            else:
                invrt2(var, 200, size, cowid)
                mult(covar305, cov305, var, 1, size, size, 4, 200, 200)
                mult(vari305, covar305, covp305, 1, size, 1, 4, 200, 4)
                mult(single305, covar305, dev, 1, size, 1, 4, 200, 1)
                mult(covar365, cov365, var, 1, size, size, 4, 200, 200)
                mult(vari365, covar365, covp365, 1, size, 1, 4, 200, 4)
                mult(single365, covar365, dev, 1, size, 1, 4, 200, 1)
                if laclen == 305:
                    single999 = single305
                elif laclen == 365:
                    single999 = single365
                else:
                    mult(covar999, cov999, var, 1, size, size, 4, 200, 200)
                    mult(vari999, covar999, covp999, 1, size, 1, 4, 200, 4)
                    mult(single999, covar999, dev, 1, size, 1, 4, 200, 1)
                mult(curvem, curve, var, maxlen // dailyfreq, size, size, maxlen, 200, 200)
                mult(graph1, curvem, dev, maxlen // dailyfreq, size, 1, maxlen, 200, 1)
                grafall[0, :, j] = graph1[:, 0]
                DAILYbp[0, j, :].fill(-999.0)
                DAILYbp[0, j, graph1[:, 0] != 0.0] = graph1[graph1[:, 0] != 0.0, 0]
                if part == 1:
                    mult(partial, covsum, var, 1, size, size, 4, 200, 200)
                    mult(partrec, partial, dev, 1, size, 1, 4, 200, 1)
                mult(qCVC1, covar305, dcovp, 1, size, 1, 4, 200, 4)
                mult(covar305, dcov, var, 1, size, size, 4, 200, 200)
                mult(dvari, covar305, dcovp, 1, size, 1, 4, 200, 4)
                mult(pers, covar305, dev, 1, size, 1, 4, 200, 1)
            YLDvec[0, j] = single305[0, 0] + herd305[0, j]
            YLDvec[0, 4 + j] = single365[0, 0] + herd365[0, j]
            YLDvec[0, 8 + j] = single999[0, 0] + herd999[0, j]
            if part == 0 or length == 0:
                YLDvec[0, 12 + j] = 0.0
            else:
                YLDvec[0, 12 + j] = partrec[0, 0] + meanyld[j, length - 1, lacn - 1] * hratio[j]
            RELyld[j] = vari305[0, 0] / (stdvar[j, lacn - 1] * varfac[j] ** 2)
            regrel[j] = RELyld[j]
            if nbump > 0:
                Fvalue = Fval[nbmp, min(size, 10) - 1]
                if bump[j] > Fvalue:
                    RELyld[j] = RELyld[j] * Fvalue / bump[j]
                    DCRvec[j] = DCRvec[j] * Fvalue / bump[j]
                    if bump[j] / Fvalue > maxbump:
                        print("------------------------------------------------")
                        print(trt[j], " Regular REL =", regrel[j])
                        print(trt[j], " Reduced REL =", RELyld[j])
                        print("  Unusual data for cow ", cowid, "   F=", bump[j])
                        if maxprnt > 0:
                            maxprnt = max(maxprnt, ncall)
                        maxbump = bump[j] / Fvalue
            DCRvec[j] = 100.0 * RELyld[j] / rmonth[j, lacn - 1]
            xsingl[j] = (1.0 / regrel[j] - 1.0) * expamt + 1.0
            lacwt[j] = (1.0 - repty[j]) / (xsingl[j] - repty[j])
            PERSvec[j] = pers[0, 0]
            RELpers[j] = dvari[0, 0] / varfac[j] ** 2
            Rvar[0, 0] = (1.0 / regrel[j] - repty[j]) * stdvar[j, lacn - 1] * varfac[j] ** 2
            Rvar[1, 1] = (1.0 / RELpers[j] - reptp[j]) * varfac[j] ** 2
            Rcorr = qCVC1[0, 0] / np.sqrt(vari[0, 0] * dvari[0, 0])
            Rvar[1, 0] = Rcorr * np.sqrt(Rvar[0, 0] * Rvar[1, 1])
            if Rcorr > 0.99:
                Rvar[1, 0] = 0.99 * np.sqrt(Rvar[0, 0] * Rvar[1, 1])
            if Rcorr < -0.99:
                Rvar[1, 0] = -0.99 * np.sqrt(Rvar[0, 0] * Rvar[1, 1])
            if j == 0:
                Rvecm = 1.0 / Rvar[0, 0]
            if j == 1:
                Rvecf = 1.0 / Rvar[0, 0]
            if j == 2:
                Rvecp = 1.0 / Rvar[0, 0]
            if j == 3:
                Rvecs = 1.0 / Rvar[0, 0]
            Rvar[0, 1] = Rvar[1, 0]
            invrt2(Rvar, 2, 2, cowid)
            n = 0
            for l in range(2):
                for m in range(l, 2):
                    n += 1
                    Rvec[n - 1] = Rvar[l, m]
    else:
        print("[ERROR]: Unrecognized value of mtrait (1|3|4): ", mtrait)
    if DEBUGmsgs > 1:
        print("Correlation among 305-d yields for ", lacn, " parity")
        print(corr305)
    for j in range(4):
        YLDvec[1, j] = YLDvec[0, j] / (agefac[j] * fact3X[j])
        YLDvec[1, j + 4] = YLDvec[0, j + 4] / (agefac[j] * fact3X[j])
        YLDvec[1, j + 8] = YLDvec[0, j + 8] / (agefac[j] * fact3X[j])
        herd305[1, j] = herd305[0, j] / (agefac[j] * fact3X[j])
        herd365[1, j] = herd365[0, j] / (agefac[j] * fact3X[j])
        herd999[1, j] = herd999[0, j] / (agefac[j] * fact3X[j])
        YLDvec[1, j + 12] = YLDvec[0, j + 12] / (agefac[j] * part3X[j])
    if UNITSout == "P":
        for j in range(3):
            YLDvec[:, j] *= 2.205
            YLDvec[:, j + 4] *= 2.205
            YLDvec[:, j + 8] *= 2.205
            YLDvec[:, j + 12] *= 2.205
            herd305[:, j] *= 2.205
            herd365[:, j] *= 2.205
            herd999[:, j] *= 2.205
    YLDvec[:, 3] /= 305.0
    YLDvec[:, 7] /= 365.0
    YLDvec[:, 11] /= laclen
    if length > 0:
        YLDvec[:, 15] /= length
    else:
        YLDvec[:, 15] = 0.0
    herd305[:, 3] /= 305.0
    herd365[:, 3] /= 365.0
    herd999[:, 3] /= laclen
    if mtrait == 4:
        MTorST[:] = "MT"
    elif mtrait == 1:
        MTorST[:] = "ST"
    elif mtrait == 3:
        MTorST[:3] = "MT"
        MTorST[3] = "ST"
    else:
        print("[ERROR]: Unrecognized value of mtrait (1|3|4): ", mtrait)
    DCRm = DCRvec[0]
    DCRc = (DCRvec[1] + DCRvec[2]) / 2.0
    DCRs = DCRvec[3]
    if ntests[2] == 0:
        DCRc = DCRvec[1]
    for j in range(4):
        if mtrait > 1:
            for l in range(mtrait):
                Yvec[0, j] = herd305[0, j] + (YLDvec[0, j] - herd305[0, j]) / regrel[j]
                Yvec[1, j] = herd305[1, j] + (YLDvec[1, j] - herd305[1, j]) / regrel[j]
            if mtrait == 3 and j == 3:
                Yvec[0, j] = PERSvec[j] / RELpers[j]
                Yvec[1, j] = Yvec[0, j]
        else:
            if RELpers[j] == 0:
                Yvec[0, j] = PERSvec[j]
                Yvec[1, j] = Yvec[0, j]
            else:
                Yvec[0, j] = PERSvec[j] / RELpers[j]
                Yvec[1, j] = Yvec[0, j]
    Yvec[:, 3] /= 305.0
    with open(BLUPout[4], "w") as f:
        f.write("".join([str(Rvec[n]) for n in range(3)]))
    with open(BLUPout[5], "w") as f:
        f.write("".join([str(Rvec[n]) for n in range(3, 6)]))
    with open(BLUPout[6], "w") as f:
        f.write("".join([str(Rvec[n]) for n in range(6, 9)]))
    with open(BLUPout[7], "w") as f:
        f.write(f"{Rvecm}{zero0}{zero0}")
    with open(BLUPout[8], "w") as f:
        f.write(f"{Rvecf}{zero0}{zero0}")
    with open(BLUPout[9], "w") as f:
        f.write(f"{Rvecp}{zero0}{zero0}")
    BLUPn[4:10] = 24
    tempSTD[0, :, :] = grafall[0, :, :] + std[0, :, :]
    for j in range(4):
        std[1, j, std[0, j, :] > -999.0] /= agefac[j] * fact3X[j]
        grafall[1, std[0, j, :] > -999.0, j] /= agefac[j] * fact3X[j]
        tempSTD[1, j, std[0, j, :] > -999.0] /= agefac[j] * fact3X[j]
        DAILYbp[1, j, std[0, j, :] > -999.0] /= agefac[j] * fact3X[j]
        yield_[1, j, std[0, j, :] > -999.0] /= agefac[j] * fact3X[j]
    if UNITSout == "P":
        std[:, :3, std[:, :3, :] > -999.0] *= 2.205
        grafall[:, :, :3][grafall[:, :, :3] > -999.0] *= 2.205
        tempSTD[:, :3, tempSTD[:, :3, :] > -999.0] *= 2.205
        DAILYbp[:, :3, DAILYbp[:, :3, :] > -999.0] *= 2.205
        yield_[:, :3, yield_[:, :3, :] > -999.0] *= 2.205
        tempTD[:3, tempTD[:3, :] > -999.0] *= 2.205
        herdavg[:, :3] *= 2.205
    if WRITEcurve == 1:
        write_curve_data(mtrait, CURVEfile, SHORTtrait, cowid, STorMTlabel, maxlen, DAILYbp, tempTD, std, parity, GRAFplot, CURVEsmall, CURVEsingle)
    if WRITEdata == 1:
        write_yield_data(DATAfile, cowid, STorMTlabel, trt, ntests, 2.205, YLDvec, herd305, DCRvec, MTorST, PERSvec, RELpers, parity, CURVEsingle)
    mstart = 0
    if usetd > 0:
        for i in range(usetd):
            mstop = dimmilk[i]
            milk3X = test3X[0, dimvec[dimmilk[i]] - 1]
            if plotfreq > 0:
                for m in range(mstart, mstop, plotfreq):
                    freq3X[(m + 5) // plotfreq - 1] = milk3X
            mstart = mstop
    for i in range(ntd):
        if dim[i] < 1 or dim[i] > maxlen:
            continue
        if MRD[i] > 1:
            plot[i] = "L"
            if super[i] == 2 or super[i] == 6:
                plot[i] = "U"
        else:
            plot[i] = "S"
            if weigh[i] < Xmilk[i]:
                plot[i] = "B"
            if weigh[i] * 2 == Xmilk[i]:
                plot[i] = "A"
            if weigh[i] * 2 < Xmilk[i]:
                plot[i] = "C"
            if super[i] == 2 or super[i] == 6:
                plot[i] = "O"
                if weigh[i] < Xmilk[i]:
                    plot[i] = "Q"
                if weigh[i] * 2 == Xmilk[i]:
                    plot[i] = "P"
                if weigh[i] * 2 < Xmilk[i]:
                    plot[i] = "R"
        if super[i] > 7:
            plot[i] = "V"
        if super[i] == 0:
            plot[i] = "X"
        if super[i] == 4:
            plot[i] = "X"
        if dimvec[dim[i]] != i + 1:
            plot[i] = "D"
        if yield_[0, 1, i] <= zero:
            k = capital.index(plot[i])
            plot[i] = small[k]
    fatpct.fill("    ")
    propct.fill("    ")
    scs.fill("    ")
    if ncall <= maxprnt:
        for l in range(4):
            if GRAFplot[l] > 0 and maxshow > 0 and ncall <= maxshow:
                if GRAFplot[l] > 2:
                    if LOGon == 1 and LOGfreq > 0:
                        int2str = f"{l + 1:5d}"
                        LogMessage = f"GRAFplot ({int2str}) has an invalid value of "
                        int2str = f"{GRAFplot[l]:5d}"
                        LogMessage += f"{int2str}. Using the default of 2."
                        log_message(LOGfile, LogMessage)
                    GRAFplot[l] = 2
                stdslice.fill(0.0)
                stdslice[0, :] = std[GRAFplot[l] - 1, l, :]
                tempSTDslice.fill(0.0)
                tempSTDslice[0, :] = tempSTD[GRAFplot[l] - 1, l, :]
                if l > 0:
                    stdslice *= 10
                    tempSTDslice *= 10
                if l == 3:
                    stdslice[0, :5] = 0.0
                    tempSTDslice[0, :5] = 0.0
                minyield[l] = 0
                maxyield[l] = max(np.max(stdslice[stdslice > 0]), np.max(tempSTDslice[tempSTDslice > 0]))
                for i in range(ntd):
                    if l == 0:
                        if yield_[GRAFplot[l] - 1, l, i] > maxyield[l]:
                            maxyield[l] = yield_[GRAFplot[l] - 1, l, i]
                        if maxyield[l] > 150:
                            maxyield[l] = 150
                    else:
                        if yield_[GRAFplot[l] - 1, l, i] * 10 > maxyield[l]:
                            maxyield[l] = yield_[GRAFplot[l] - 1, l, i] * 10
                print("-------------------------------------------------------" "---------")
                if GRAFplot[l] == 1:
                    print(GRAFname[l], "(ME)", cowid, fresh[:4], fresh[4:6], fresh[6:])
                else:
                    print(GRAFname[l], "(Actual)", cowid, fresh[:4], fresh[4:6], fresh[6:])
                for j in range(int(maxyield[l] // YIELDunits[l]), int(minyield[l] // YIELDunits[l]) - 1, -YIELDsteps[l]):
                    for i in range(maxlen // plotfreq):
                        plt = " "
                        if i == maxlen // plotfreq - 1:
                            plt = ":"
                        if i == 305 // plotfreq - 1:
                            plt = "."
                        if i == length // plotfreq - 1:
                            plt = "|"
                        if l > 0:
                            testday = std[GRAFplot[l] - 1, l, i * plotfreq] * 10.0
                        else:
                            testday = std[GRAFplot[l] - 1, l, i * plotfreq]
                        if testday / YIELDunits[l] == j:
                            plt = "."
                        if l > 0:
                            testday = tempSTD[GRAFplot[l] - 1, l, i * plotfreq] * 10.0
                        else:
                            testday = tempSTD[GRAFplot[l] - 1, l, i * plotfreq]
                        if testday / YIELDunits[l] == j:
                            plt = "-"
                        tests[i] = plt
                    for i in range(ntd):
                        if dim[i] < 1:
                            continue
                        if yield_[GRAFplot[l] - 1, 1, i] > zero:
                            percent = f"{yield_[GRAFplot[l] - 1, 1, i] * 100 / yield_[GRAFplot[l] - 1, 0, i]:4.1f}"
                            fatpct[dim[i] // 24] = percent
                            percent = f"{yield_[GRAFplot[l] - 1, 2, i] * 100 / yield_[GRAFplot[l] - 1, 0, i]:4.1f}"
                            propct[dim[i] // 24] = percent
                        if yield_[GRAFplot[l] - 1, 3, i] > zero:
                            percent = f"{yield_[GRAFplot[l] - 1, 3, i]:4.1f}"
                            scs[dim[i] // 24] = percent
                        if l > 0:
                            testday = yield_[GRAFplot[l] - 1, l, i] * 10
                        else:
                            testday = yield_[GRAFplot[l] - 1, l, i]
                        middle[i] = dim[i] - (MRD[i] - 1) // 2
                        if testday / YIELDunits[l] == j:
                            if middle[i] // plotfreq <= 0:
                                tests[middle[i]] = plot[i]
                            elif middle[i] // plotfreq <= 60:
                                tests[middle[i] // plotfreq] = plot[i]
                    if l == 0:
                        print(f"{j * YIELDunits[l]:4d}", tests)
                    else:
                        print(f"{float(j * YIELDunits[l]) / 10.:3.1f}", tests)
                print("".join([f"{i * 30:5d}" for i in range(maxlen // 30 + 1)]))
                print("Fat%", "".join(fatpct))
                print("Pro%", "".join(propct))
                print("SCS ", "".join(scs))
        print("")
        print(" Lactation summary for cow ", cowid, ",  fresh ", fresh[:4], fresh[4:6], fresh[6:], ", parity ", parity)
        print(length, laclen)
        print(f" Cntmp     Adjust       Persistency     Data{' '*3}{length}-d    305-d    365-d    {laclen}-d    305-d     Factors    Esti-   Relia-    Coll{' '*7}ectn")
        print(" TRAIT Tests    Yield    Yield    Yield    Yield     Mean   Age,     3X  mate    bility    Rating")
        for l in range(4):
            print(
                trt[l + 4],
                ntests[l],
                YLDvec[0, 12 + l] * X100[l],
                YLDvec[0, l] * X100[l],
                YLDvec[0, 4 + l] * X100[l],
                YLDvec[0, 8 + l] * X100[l],
                herd305[0, l] * X100[l],
                agefac[l],
                fact3X[l],
                PERSvec[l],
                RELpers[l] * 100.0,
                DCRvec[l],
                MTorST[l],
            )
        for l in range(4):
            print(
                trt[l],
                ntests[l],
                YLDvec[1, 12 + l] * X100[l],
                YLDvec[1, l] * X100[l],
                YLDvec[1, 4 + l] * X100[l],
                YLDvec[1, 8 + l] * X100[l],
                herd305[1, l] * X100[l],
                agefac[l],
                fact3X[l],
                PERSvec[l],
                RELpers[l] * 100.0,
                DCRvec[l],
                MTorST[l],
            )
        print(" ")
        print(" ")
    if ncall <= maxshow and ncall > maxprnt and maxshow > 0:
        print("")
        print(" Lactation summary for cow ", cowid, ",  fresh ", fresh[:4], fresh[4:6], fresh[6:], ", parity ", parity)
        print(length, laclen)
        print(f" Cntmp     Adjust       Persistency     Data{' '*3}{length}-d    305-d    365-d    {laclen}-d    305-d     Factors    Esti-   Relia-    Coll{' '*7}ectn")
        print(" Trait Tests    Yield    Yield    Yield    Yield     Mean   Age,     3X  mate    bility    Rating")
        for j in range(4):
            print(
                trt[j + 4],
                ntests[j],
                YLDvec[0, 12 + j] * X100[j],
                YLDvec[0, j] * X100[j],
                YLDvec[0, 4 + j] * X100[j],
                YLDvec[0, 8 + j] * X100[j],
                herd305[0, j] * X100[j],
                agefac[j],
                fact3X[j],
                PERSvec[j],
                RELpers[j] * 100.0,
                DCRvec[j],
                MTorST[j],
            )
        for j in range(4):
            print(
                trt[j],
                ntests[j],
                YLDvec[1, 12 + j] * X100[j],
                YLDvec[1, j] * X100[j],
                YLDvec[1, 4 + j] * X100[j],
                YLDvec[1, 8 + j] * X100[j],
                herd305[1, j] * X100[j],
                agefac[j],
                fact3X[j],
                PERSvec[j],
                RELpers[j] * 100.0,
                DCRvec[j],
                MTorST[j],
            )
        print(" ")
        print(" ")
    dimvec.fill(0)
    for i in range(maxlen // plotfreq):
        i6 = (i + 1) * dailyfreq
        for j in range(ntd):
            if dim[j] < 1 or dim[j] > maxlen:
                continue
    if maxprnt < 0:
        print("======================================================")
        print("Outputs from bestpred for cow=", cowid, " fresh=", fresh)
        print("       tstdays=", tstdays, " parity=", parity, " length=", length, " maxprnt=", maxprnt)
        print("305d ME yields    : ", YLDvec[0, :4])
        print("305d Actual yields: ", YLDvec[1, :4])
        print("DCR               : ", DCRvec)
        print("Persistencies     : ", PERSvec)
        print("Persist REL       : ", RELpers)
        print("======================================================")
    if DEBUGmsgs > 1:
        print("Deallocating memory")
    del curve
    del curvem
    del curves
    del graph1
    del grafall
    del freq3X
    del std
    del stdslice
    del tempTD
    del tempSTD
    del tempSTDslice
    del fatpct
    del propct
    del scs
    del tests
    del dimvec
    return
