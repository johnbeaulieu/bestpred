import numpy as np
from age_h import *
from ageadj_h import *

# aiplage.c  Obtain age - season - previous days open adjustments */
#            agebase = 0 adjusts to mature, 36 to 36 months, etc  */
#            Factors differ by breed, region, and time period     */
#                1994 Mike Schutz    - Factors estimated          */
#            Oct 1994 George Wiggans - Original program           */
#            Oct 2004 Paul VanRaden  - 36 month option added      */
#-----------------------------------------------------------------*/

# Constants
DOMEAN = 120

# Sample data for the arrays used in calculations
region = np.zeros((50, 4), dtype=int)
hmdop = np.zeros((7, 3, 4))
hfdop = np.zeros((7, 3, 4))
hpdop = np.zeros((7, 3, 4))
mdop = np.zeros((5, 3, 4))
fdop = np.zeros((5, 3, 4))
pdop = np.zeros((5, 3, 4))
hmregtm = np.zeros((7, 5))
hfregtm = np.zeros((7, 5))
hpregtm = np.zeros((7, 2))
hmequ0 = np.zeros((5, 7, 6, 12))
hmequ = np.zeros((5, 7, 6, 2))
hfequ0 = np.zeros((5, 7, 6, 12))
hfequ = np.zeros((5, 7, 6, 2))
hpequ0 = np.zeros((2, 7, 6, 12))
hpequ = np.zeros((2, 7, 6, 2))

agelim = np.array([[16, 36], [28, 56], [40, 74], [52, 86], [64, 98], [76, 165]])
regdot = [0, 0, 1, 4, 4, 1, 3, 2, 3, 5, 5, 6]


def aiplage(breed, agefrsh, fryear, frmonth, parity, state, doprev, agebase):
    # Initial values
    breeds = "ABGHJMW"
    regbrd = [0, 0, 0, 1, 2, 3, 2]

    if breed not in breeds:
        brd = 3  # Default to Holstein
    else:
        brd = breeds.index(breed)

    brdreg = regbrd[brd]
    reg = region[state][brdreg] - 1  # Storage starts at 0

    if parity == 0:
        raise ValueError("Parity = 0 in function aiplage")

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

    for pass_num in range(1, 3):
        months = 1
        if pass_num == 2:
            age = agebase
            if age == 0:
                age = 36
            lac = (agebase - 18) // 13
            pdo = 90
            if agebase > 0:
                months = 12
            avgfacm = avgfacf = avgfacp = 0

        for month in range(1, months + 1):
            if pass_num == 2:
                mo = month - 1

            # Calculate previous Days Open Values
            if lac > 0:
                lacdo = min(2, lac) - 1
                regdo = regdot[reg]
                doprev2 = pdo**2

                if brd == 3:  # Holstein
                    pdoeffm = hmdop[regdo][lacdo][0] + hmdop[regdo][lacdo][1] * pdo + hmdop[regdo][lacdo][2] * doprev2
                    pdoefff = hfdop[regdo][lacdo][0] + hfdop[regdo][lacdo][1] * pdo + hfdop[regdo][lacdo][2] * doprev2
                    pdoeffp = hpdop[regdo][lacdo][0] + hpdop[regdo][lacdo][1] * pdo + hpdop[regdo][lacdo][2] * doprev2
                    if pdo > 150:
                        do150p = (pdo - 150) ** 2
                        pdoeffm += hmdop[regdo][lacdo][3] * do150p
                        pdoefff += hfdop[regdo][lacdo][3] * do150p
                        pdoeffp += hpdop[regdo][lacdo][3] * do150p
                else:  # Other Breeds
                    brddo = brd if brd < 3 else brd - 1
                    pdoeffm = mdop[brddo][lacdo][0] + mdop[brddo][lacdo][1] * pdo + mdop[brddo][lacdo][2] * doprev2
                    pdoefff = fdop[brddo][lacdo][0] + fdop[brddo][lacdo][1] * pdo + fdop[brddo][lacdo][2] * doprev2
                    pdoeffp = pdop[brddo][lacdo][0] + pdop[brddo][lacdo][1] * pdo + pdop[brddo][lacdo][2] * doprev2
                    if pdo > 150:
                        do150p = (pdo - 150) ** 2
                        pdoeffm += mdop[brddo][lacdo][3] * do150p
                        pdoefff += fdop[brddo][lacdo][3] * do150p
                        pdoeffp += pdop[brddo][lacdo][3] * do150p
            else:
                pdoeffm = pdoefff = pdoeffp = 0

            if fryear >= 1987:
                yrgp = 4
                yrgpp = 1
            else:
                yrgp = (fryear - 1963) // 6 if fryear >= 1969 else 0
                yrgpp = 0

            lacdo = min(2, lac)
            age2 = age**2

            if brd == 3:  # Holstein
                facm = hmregtm[reg][yrgp] / (hmregtm[reg][yrgp] + pdoeffm + hmequ0[yrgp][reg][lac][mo] + hmequ[yrgp][reg][lac][0] * age + hmequ[yrgp][reg][lac][1] * age2)
                facf = hfregtm[reg][yrgp] / (hfregtm[reg][yrgp] + pdoefff + hfequ0[yrgp][reg][lac][mo] + hfequ[yrgp][reg][lac][0] * age + hfequ[yrgp][reg][lac][1] * age2)
                facp = hpregtm[reg][yrgpp] / (hpregtm[reg][yrgpp] + pdoeffp + hpequ0[yrgpp][reg][lac][mo] + hpequ[yrgpp][reg][lac][0] * age + hpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 4:  # Jersey
                facm = jmregtm[reg][yrgp] / (jmregtm[reg][yrgp] + pdoeffm + jmequ0[yrgp][reg][lac][mo] + jmequ[yrgp][reg][lac][0] * age + jmequ[yrgp][reg][lac][1] * age2)
                facf = jfregtm[reg][yrgp] / (jfregtm[reg][yrgp] + pdoefff + jfequ0[yrgp][reg][lac][mo] + jfequ[yrgp][reg][lac][0] * age + jfequ[yrgp][reg][lac][1] * age2)
                facp = jpregtm[reg][yrgpp] / (jpregtm[reg][yrgpp] + pdoeffp + jpequ0[yrgpp][reg][lac][mo] + jpequ[yrgpp][reg][lac][0] * age + jpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 2:  # Guernsey
                facm = gmregtm[reg][yrgp] / (gmregtm[reg][yrgp] + pdoeffm + gmequ0[yrgp][reg][lac][mo] + gmequ[yrgp][reg][lac][0] * age + gmequ[yrgp][reg][lac][1] * age2)
                facf = gfregtm[reg][yrgp] / (gfregtm[reg][yrgp] + pdoefff + gfequ0[yrgp][reg][lac][mo] + gfequ[yrgp][reg][lac][0] * age + gfequ[yrgp][reg][lac][1] * age2)
                facp = gpregtm[reg][yrgpp] / (gpregtm[reg][yrgpp] + pdoeffp + gpequ0[yrgpp][reg][lac][mo] + gpequ[yrgpp][reg][lac][0] * age + gpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 1:  # Brown Swiss
                facm = bmregtm[reg][yrgp] / (bmregtm[reg][yrgp] + pdoeffm + bmequ0[yrgp][reg][lac][mo] + bmequ[yrgp][reg][lac][0] * age + bmequ[yrgp][reg][lac][1] * age2)
                facf = bfregtm[reg][yrgp] / (bfregtm[reg][yrgp] + pdoefff + bfequ0[yrgp][reg][lac][mo] + bfequ[yrgp][reg][lac][0] * age + bfequ[yrgp][reg][lac][1] * age2)
                facp = bpregtm[reg][yrgpp] / (bpregtm[reg][yrgpp] + pdoeffp + bpequ0[yrgpp][reg][lac][mo] + bpequ[yrgpp][reg][lac][0] * age + bpequ[yrgpp][reg][lac][1] * age2)
            elif brd == 0:  # Ayrshire
                facm = amregtm[reg][yrgp] / (amregtm[reg][yrgp] + pdoeffm + amequ0[yrgp][reg][lac][mo] + amequ[yrgp][reg][lac][0] * age + amequ[yrgp][reg][lac][1] * age2)
                facf = afregtm[reg][yrgp] / (afregtm[reg][yrgp] + pdoefff + afequ0[yrgp][reg][lac][mo] + afequ[yrgp][reg][lac][0] * age + afequ[yrgp][reg][lac][1] * age2)
                facp = apregtm[reg][yrgpp] / (apregtm[reg][yrgpp] + pdoeffp + apequ0[yrgpp][reg][lac][mo] + apequ[yrgpp][reg][lac][0] * age + apequ[yrgpp][reg][lac][1] * age2)
            elif brd == 5:  # Milking Shorthorn
                facm = mmregtm[yrgp] / (mmregtm[yrgp] + pdoeffm + mmequ0[yrgp][lac][mo] + mmequ[yrgp][lac][0] * age + mmequ[yrgp][lac][1] * age2)
                facf = mfregtm[yrgp] / (mfregtm[yrgp] + pdoefff + mfequ0[yrgp][lac][mo] + mfequ[yrgp][lac][0] * age + mfequ[yrgp][lac][1] * age2)
                facp = mpregtm[yrgpp] / (mpregtm[yrgpp] + pdoeffp + mpequ0[yrgpp][lac][mo] + mpequ[yrgpp][lac][0] * age + mpequ[yrgpp][lac][1] * age2)

            agefacm = 0.0
            agefacf = 0.0
            agefacp = 0.0

            if pass_num == 1:
                agefacm = facm
                agefacf = facf
                agefacp = facp
            if pass_num == 2:
                avgfacm += facm / months
                avgfacf += facf / months
                avgfacp += facp / months
            if month == 12 and agebase > 0:
                agefacm /= avgfacm
                agefacf /= avgfacf
                agefacp /= avgfacp

    return agefacm, agefacf, agefacp


# # Example usage
# breed = "H"
# agefrsh = 25
# fryear = 1990
# frmonth = 3
# parity = 2
# state = 10
# doprev = 100
# agebase = 36

# agefacm, agefacf, agefacp = aiplage(breed, agefrsh, fryear, frmonth, parity, state, doprev, agebase)
# print(f"agefacm: {agefacm}, agefacf: {agefacf}, agefacp: {agefacp}")
