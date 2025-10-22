from ageadj_h import *
from age_h import *

# ageadj.py   Apply age-month of calving and days open adjustments
#             agebase = 0 adjusts to mature, 36 to 36 months, etc
#                         based on factors of Schutz, 1994
#             Oct 1994 George Wiggans - Original program
#             Sep 2004 Paul VanRaden  - 36 month option added
#-----------------------------------------------------------------

DOMEAN = 120

def ageadj(breed, agefrsh, fryear, frmonth, parity, state, doprev, agebase, agefacm, agefacf, agefacp):
    # Assume age.h has been included
    """
    import dosol1
    import age_equ
    import region_def
    import reg_mean
    """
    breeds = "ABGHJMW"
    regbrd = [0, 0, 0, 1, 2, 3, 2]
    pdoeffm = pdoefff = pdoeffp = facm = facf = facp = do150 = do150p = age2 = doprev2 = mregtm = fregtm = pregtm = avgfacm = avgfacf = avgfacp = 0.0
    brd = brddo = reg = mo = yrgp = yrgpp = age = lac = lacdo = pdo = regdo = i = j = pass_ = month = months = 0

    agelim = [
        [16, 36], [28, 56], [40, 74], [52, 86], [64, 98], [76, 165]
    ]
    regdot = [0, 0, 1, 4, 4, 1, 3, 2, 3, 5, 5, 6]

    if breed not in breeds:
        brd = 3  # default to Holstein
    else:
        brd = breeds.index(breed)

    brdreg = regbrd[brd]
    reg = region[state][brdreg] - 1  # Storage starts at 0

    if parity == 0:
        print("Parity = 0 in function ageadj")
        exit(25)

    lac = parity - 1
    if lac > 5:
        lac = 5

    # If age outside of range for parity, use age limit
    age = agefrsh
    if age < agelim[lac][0]:
        age = agelim[lac][0]
    elif age > agelim[lac][1]:
        age = agelim[lac][1]

    mo = frmonth - 1
    pdo = doprev
    if (parity > 1 and pdo < 20) or pdo > 305:
        pdo = DOMEAN

    # ME factor on first pass, 36-month factor on 2nd pass
    for pass_ in range(1, 3):
        months = 1
        if pass_ == 2:
            age = agebase
            if age == 0:
                age = 36
            lac = (agebase - 18) // 13
            pdo = 90
            if agebase > 0:
                months = 12
            avgfacm = avgfacf = avgfacp = 0

        for month in range(1, months + 1):
            if pass_ == 2:
                mo = month - 1

            # Calculate previous Days Open Values
            if lac > 0:
                lacdo = 2 if lac > 2 else lac  # Values 0, 1, 2 for lactations 1, 2, 3
                lacdo -= 1
                regdo = regdot[reg]
                doprev2 = pdo * pdo

                if brd == 3:  # Holstein
                    pdoeffm = hmdop[regdo][lacdo][0] + hmdop[regdo][lacdo][1] * pdo + hmdop[regdo][lacdo][2] * doprev2
                    pdoefff = hfdop[regdo][lacdo][0] + hfdop[regdo][lacdo][1] * pdo + hfdop[regdo][lacdo][2] * doprev2
                    pdoeffp = hpdop[regdo][lacdo][0] + hpdop[regdo][lacdo][1] * pdo + hpdop[regdo][lacdo][2] * doprev2
                    if pdo > 150:
                        do150p = (pdo - 150) * (pdo - 150)
                        pdoeffm += hmdop[regdo][lacdo][3] * do150p
                        pdoefff += hfdop[regdo][lacdo][3] * do150p
                        pdoeffp += hpdop[regdo][lacdo][3] * do150p
                else:  # Other Breeds
                    brddo = brd if brd < 3 else brd - 1
                    pdoeffm = mdop[brddo][lacdo][0] + mdop[brddo][lacdo][1] * pdo + mdop[brddo][lacdo][2] * doprev2
                    pdoefff = fdop[brddo][lacdo][0] + fdop[brddo][lacdo][1] * pdo + fdop[brddo][lacdo][2] * doprev2
                    pdoeffp = pdop[brddo][lacdo][0] + pdop[brddo][lacdo][1] * pdo + pdop[brddo][lacdo][2] * doprev2
                    if pdo > 150:
                        do150p = (pdo - 150) * (pdo - 150)
                        pdoeffm += mdop[brddo][lacdo][3] * do150p
                        pdoefff += fdop[brddo][lacdo][3] * do150p
                        pdoeffp += pdop[brddo][lacdo][3] * do150p
            else:
                pdoeffm = pdoefff = pdoeffp = 0.0  # End of Previous Days Open

            if fryear >= 1987:
                yrgp = 4
                yrgpp = 1
            else:
                yrgp = (fryear - 1963) // 6 if fryear >= 1969 else 0
                yrgpp = 0

            lacdo = 2 if lac > 2 else lac
            age2 = age * age
            if brd == 3:  # Holstein
                mregtm = hmregt[reg] * mratio[yrgp]
                fregtm = hfregt[reg] * fratio[yrgp]
                pregtm = hpregt[reg] * pratio[yrgp]
                facm = mregtm / (mregtm + pdoeffm + hmequ0[yrgp][reg][lac][mo] + hmequ[yrgp][reg][lac][0] * age + hmequ[yrgp][reg][lac][1] * age2)
                facf = fregtm / (fregtm + pdoefff + hfequ0[yrgp][reg][lac][mo] + hfequ[yrgp][reg][lac][0] * age + hfequ[yrgp][reg][lac][1] * age2)
                facp = pregtm / (pregtm + pdoeffp + hpequ0[yrgpp][reg][lac][mo] + hpequ[yrgpp][reg][lac][0] * age + hpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 4:  # Jersey
                mregtm = jmregt[reg] * mratio[yrgp]
                fregtm = jfregt[reg] * fratio[yrgp]
                pregtm = jpregt[reg] * pratio[yrgp]
                facm = mregtm / (mregtm + pdoeffm + jmequ0[yrgp][reg][lac][mo] + jmequ[yrgp][reg][lac][0] * age + jmequ[yrgp][reg][lac][1] * age2)
                facf = fregtm / (fregtm + pdoefff + jfequ0[yrgp][reg][lac][mo] + jfequ[yrgp][reg][lac][0] * age + jfequ[yrgp][reg][lac][1] * age2)
                facp = pregtm / (pregtm + pdoeffp + jpequ0[yrgpp][reg][lac][mo] + jpequ[yrgpp][reg][lac][0] * age + jpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 2:  # Guernsey
                mregtm = gmregt[reg] * mratio[yrgp]
                fregtm = gfregt[reg] * fratio[yrgp]
                pregtm = gpregt[reg] * pratio[yrgp]
                facm = mregtm / (mregtm + pdoeffm + gmequ0[yrgp][reg][lac][mo] + gmequ[yrgp][reg][lac][0] * age + gmequ[yrgp][reg][lac][1] * age2)
                facf = fregtm / (fregtm + pdoefff + gfequ0[yrgp][reg][lac][mo] + gfequ[yrgp][reg][lac][0] * age + gfequ[yrgp][reg][lac][1] * age2)
                facp = pregtm / (pregtm + pdoeffp + gpequ0[yrgpp][reg][lac][mo] + gpequ[yrgpp][reg][lac][0] * age + gpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 1:  # Brown Swiss
                mregtm = bmregt[reg] * mratio[yrgp]
                fregtm = bfregt[reg] * fratio[yrgp]
                pregtm = bpregt[reg] * pratio[yrgp]
                facm = mregtm / (mregtm + pdoeffm + bmequ0[yrgp][reg][lac][mo] + bmequ[yrgp][reg][lac][0] * age + bmequ[yrgp][reg][lac][1] * age2)
                facf = fregtm / (fregtm + pdoefff + bfequ0[yrgp][reg][lac][mo] + bfequ[yrgp][reg][lac][0] * age + bfequ[yrgp][reg][lac][1] * age2)
                facp = pregtm / (pregtm + pdoeffp + bpequ0[yrgpp][reg][lac][mo] + bpequ[yrgpp][reg][lac][0] * age + bpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 0:  # Ayrshire
                mregtm = amregt[reg] * mratio[yrgp]
                fregtm = afregt[reg] * fratio[yrgp]
                pregtm = apregt[reg] * pratio[yrgp]
                facm = mregtm / (mregtm + pdoeffm + amequ0[yrgp][reg][lac][mo] + amequ[yrgp][reg][lac][0] * age + amequ[yrgp][reg][lac][1] * age2)
                facf = fregtm / (fregtm + pdoefff + afequ0[yrgp][reg][lac][mo] + afequ[yrgp][reg][lac][0] * age + afequ[yrgp][reg][lac][1] * age2)
                facp = pregtm / (pregtm + pdoeffp + apequ0[yrgpp][reg][lac][mo] + apequ[yrgpp][reg][lac][0] * age + apequ[yrgpp][reg][lac][1] * age2)
            elif brd == 5:  # Milking Shorthorn
                mregtm = mmregt * mratio[yrgp]
                fregtm = mfregt * fratio[yrgp]
                pregtm = mpregt * pratio[yrgp]
                facm = mregtm / (mregtm + pdoeffm + mmequ0[yrgp][lac][mo] + mmequ[yrgp][lac][0] * age + mmequ[yrgp][lac][1] * age2)
                facf = fregtm / (fregtm + pdoefff + mfequ0[yrgp][lac][mo] + mfequ[yrgp][lac][0] * age + mfequ[yrgp][lac][1] * age2)
                facp = pregtm / (pregtm + pdoeffp + mpequ0[yrgpp][lac][mo] + mpequ[yrgpp][lac][0] * age + mpequ[yrgpp][lac][1] * age2)

            if pass_ == 1:
                agefacm[0] = facm
                agefacf[0] = facf
                agefacp[0] = facp

            # Obtain average across 12 months of year
            if pass_ == 2:
                avgfacm += facm / months
                avgfacf += facf / months
                avgfacp += facp / months

            # Divide by 36 month average factor
            if month == 12 and agebase > 0:
                agefacm[0] /= avgfacm
                agefacf[0] /= avgfacf
                agefacp[0] /= avgfacp

# printf("facm %f",facm)
# Standardize lactation yield
# adjyld[0] = facm*yield[0] + .5
# adjyld[1] = facf*yield[1] + .5
# adjyld[2] = facp*yield[2] + .5
