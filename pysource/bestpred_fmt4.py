import numpy as np

def bestpred_fmt4(format4, doprev, maxprnt, DCRm, DCRc, DCRs, YLDvec, PERSvec, RELyld, RELpers,
                  Yvec=None, herd305=None, bump=None, BLUPout=None, BLUPn=None, bpfreqin=None,
                  GRAFplot=None, DEBUGmsgs=None, ONscreen=None, mtrait=None, use3X=None,
                  laclen=None, dailyfreq=None, INTmethod=None, maxlen=None, DAILYbp=None,
                  DAILYherd=None, WRITEcurve=None, CURVEfile=None, WRITEdata=None, DATAfile=None,
                  plotfreq=None, INTmethodSCS=None, READparms=None, UNITSin=None, UNITSout=None,
                  breedUNK=None, dim0=None, dim0flag=None, LOGon=None, LOGfile=None, LOGfreq=None,
                  maxshow=None, CURVEsmall=None, CURVEsingle=None, region=None, season=None):

    maxtd = 50
    agebase = 0
    showage = 0

    if mtrait not in [1, 3, 4]:
        mtrait = 1

    zero = 0.0
    herd305 = np.zeros((2, 4))

    count = 0
    docur = 90
    fill2 = 0
    i305 = 305
    agebase = 0
    showage = 0

    if DEBUGmsgs > 0:
        print('[bestpred_fmt4]: CURVEsingle = ', CURVEsingle)

    count += 1
    if count == 1 and showage == 1:
        for i in range(1, 7):
            brd = 'H' if i == 1 else 'J' if i == 2 else 'B' if i == 3 else 'G' if i == 4 else 'A' if i == 5 else 'M'
            if i == 1:
                print('Milk    Fat     Prot    SCS   prevDO')
            for fryr in range(1990, 1965, -6):
                for age in range(22, 100, 14):
                    lacno = (age - 6) // 14
                    parity = lacno
                    if doprev < 1:
                        doprev = 140
                    if lacno == 1:
                        doprev = 0
                    avgfac = np.zeros(4)
                    for frmo in range(1, 13):
                        for state in [21, 35, 42, 74, 93]:
                            scs305 = 329
                            # call aiplage
                            agefac = np.zeros(4)
                            oldfac = np.zeros(4)
                            # Placeholder for aiplage call
                            # agefac[:3] = aiplage(brd, age, fryr, frmo, lacno, state, doprev, agebase)
                            agefac[3] = adjscs4(brd, lacno, frmo, state, age, i305, scs305) / scs305
                            for j in range(4):
                                if j < 3:
                                    oldfac[j] = adjyld2[j] / yld2[j]
                                avgfac[j] += agefac[j] / 60
                            print('Age-par', age, lacno, ' factors ', agefac, doprev)
                            print('Age-par', age, lacno, ' factors ', oldfac, doprev)
                        print('Age-par', age, lacno, ' factors ', avgfac, doprev)

    # Read input record
    # format4 should be parsed according to the provided format
    # Assuming it's provided as a dictionary or similar structured data
    cowid = format4['cowid']
    birth = format4['birth']
    herd = format4['herd']
    fresh = format4['fresh']
    length = format4['length']
    lacno = format4['lacno']
    doprev = format4['doprev']
    ntd = format4['ntd']
    segments = format4['segments']

    # Process segments
    yield_ = np.zeros((2, 4, maxtd))
    for i in range(ntd):
        segment = segments[i]
        dim = segment['dim']
        super_ = segment['super']
        status = segment['status']
        Xmilk = segment['Xmilk']
        weigh = segment['weigh']
        sample = segment['sample']
        MRD = segment['MRD']
        yield_[:, :, i] = segment['yield']

        if status == 3:
            yield_[:, :, i] = zero
        if status == 2:
            yield_[0, 1:3, i] = zero

    # Read the state code stored in bytes 107-108 of Format 4
    herdstate = herd[:2]
    scs305 = 329

    if DEBUGmsgs > 0:
        print('[bestpred_fmt4] herdstate: ', herdstate)

    # Read cow's deviation and me305
    me305 = format4['me305']
    dev305 = format4['dev305']

    if DEBUGmsgs > 0:
        print('[bestpred_fmt4]: herd305 before bestpred_fmt4()', herd305)

    # Backsolve to get mature herd mean
    for j in range(4):
        if herd305[0, j] <= 0:
            if j < 3:
                herd305[0, j] = me305[j] - dev305[j]
                if me305[j] == 0.0:
                    herd305[0, j] = 0.0
            else:
                herd305[0, j] = 0.0

    herd305[1, :] = herd305[0, :]

    if birth == '        ':
        birth = '19500101'
    if birth < '19500101':
        birth = '19500101'

    age = (pday(fresh) - pday(birth)) // 30.5
    if fresh == '        ':
        age = 84

    # Assign missing lactation num
    if lacno <= 0:
        lacno = max(1, (age - 6) // 13)
    if birth == '19500101':
        age = 12 + 15 * lacno

    fryr = int(fresh[:4])
    frmo = int(fresh[4:6])
    state = int(herd[:2])
    if frmo < 1:
        frmo = 1
    brd = cowid[0]
    if brd not in 'ABGHJM':
        brd = 'H'
    parity = lacno
    if doprev < 1:
        doprev = 140
    if lacno == 1:
        doprev = 0

    # Placeholder for aiplage call
    agefac = np.zeros(4)
    # agefac[:3] = aiplage(brd, age, fryr, frmo, lacno, state, doprev, agebase)
    agefac[3] = adjscs4(brd, lacno, frmo, state, age, i305, scs305) / scs305

    if DEBUGmsgs > 1:
        print('Age factors before call to aiplage(): ', agefac)
        print('aiplage() parameters')
        print('--------------------')
        print('    brd     : ', brd)
        print('    age     : ', age)
        print('    fryr    : ', fryr)
        print('    frmo    : ', frmo)
        print('    lacno   : ', lacno)
        print('    state   : ', state)
        print('    doprev  : ', doprev)
        print('    agebase : ', agebase)

    if state == 0:
        print('[bestpred_fmt4]: Changing state from ', state, ' to 35 (Wisconsin).')
        state = 35

    # Call bestpred
    bestpred(mtrait, use3X, maxprnt, ntd, dim, super_, Xmilk, weigh, sample, MRD, yield_,
             herd305, agefac, cowid, fresh, parity, length, DCRm, DCRc, DCRs, YLDvec, PERSvec,
             RELyld, RELpers, Yvec, bump, BLUPout, BLUPn, GRAFplot, DEBUGmsgs, ONscreen, laclen,
             dailyfreq, INTmethod, maxlen, DAILYbp, DAILYherd, WRITEcurve, CURVEfile, WRITEdata,
             DATAfile, plotfreq, INTmethodSCS, READparms, UNITSin, UNITSout, breedUNK, dim0,
             dim0flag, LOGon, LOGfile, LOGfreq, maxshow, herdstate, CURVEsmall, CURVEsingle,
             region, season)
    return

def pday(date8):
    modays = np.array([[31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31],
                       [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]])
    year, month, day = int(date8[:4]), int(date8[4:6]), int(date8[6:8])
    year -= 1900

    if year <= 0:
        return -21916

    leap = year // 4
    number_of_years = year - 60
    year_groups = abs(number_of_years) // 4
    balance_years = abs(number_of_years) - (year_groups * 4)

    if year == leap * 4:
        leap = 2
    else:
        leap = 1

    if month <= 0:
        month = 1
    if month > 12:
        month = 12
    if day == 0:
        day = 1
    if day > modays[leap - 1, month - 1]:
        day = modays[leap - 1, month - 1]

    for i in range(month - 1):
        day += modays[leap - 1, i]

    if number_of_years <= -1:
        day = 365 - day
        return -((year_groups * 1461) + day + (balance_years - 1) * 365)
    else:
        if balance_years > 0:
            balance_years = (((balance_years - 1) * 365) + 366)
        return (year_groups * 1461) + day + balance_years - 1

# Placeholder for other required functions (aiplage, adjscs4, bestpred)
def aiplage(*args):
    pass

def adjscs4(*args):
    return 1.0

def bestpred(*args):
    pass
