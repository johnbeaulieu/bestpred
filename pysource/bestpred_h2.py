import numpy as np

def date_to_season(freshdate: str, debug_msgs: int = 0) -> int:
    """
    Determines the season based on the month of the date provided.

    :param freshdate: Date in the format YYYYMMDD
    :param debug_msgs: Debug message level
    :return: Season as an integer (1: Spring, 2: Summer, 3: Fall, 4: Winter)
    """
    month = freshdate[4:6]
    season = 1  # Default season (Spring)

    if month in ('03', '04', '05'):
        season = 1  # Spring
    elif month in ('06', '07', '08'):
        season = 2  # Summer
    elif month in ('09', '10', '11'):
        season = 3  # Fall
    elif month in ('12', '01', '02'):
        season = 4  # Winter
    else:
        season = 1  # Default to Spring
        print(f"Could not assign the month {month} to a season. Defaulting to {season}.")

    if debug_msgs > 0:
        print(f"[date_to_season]: Month {month} is in season {season}.")

    return season

#--------------------------------------------------------------------
# Assign herds to regions of the country.  Regions are the same as
# those used in:
# E. Hare, H. D. Norman and J. R. Wright. 2004. Duration of herd
# participation in Dairy Herd Improvement milk recording in the United
# States. J. Dairy Sci. 87:2743-2747.
# Input:  2-digit state (from the herd code on the Format 4) as a string
# Output: Region code as an integer.

def state_to_region(herdstate: str, debug_msgs: int = 0) -> int:
    """
    Determines the region based on the state code provided.

    :param herdstate: State code as a string
    :param debug_msgs: Debug message level
    :return: Region as an integer (1: Mideast, 2: Midwest, 3: Mountain-Prairie,
             4: Northeast, 5: Northwest, 6: Southeast, 7: Southwest)
    """
    region_mapping = {
        1: ['50', '51', '52', '54', '55', '61', '63'],  # Mideast
        2: ['31', '32', '33', '34', '35', '41', '42', '43'],  # Midwest
        3: ['45', '46', '47', '48', '81', '83', '84', '87'],  # Mountain-Prairie
        4: ['11', '12', '13', '14', '15', '16', '21', '22', '23'],  # Northeast
        5: ['82', '91', '92', '96'],  # Northwest
        6: ['56', '57', '58', '64', '65', '71', '72', '73', '74', '94'],  # Southeast
        7: ['85', '86', '88', '93', '95']  # Southwest
    }

    # Default to Midwest if no match found
    region = 2

    for key, states in region_mapping.items():
        if herdstate in states:
            region = key
            break
    else:
        print(f"Could not assign the state {herdstate} to a region. Defaulting to {region}.")

    if debug_msgs > 0:
        print(f"[state_to_region]: State {herdstate} is in region {region}.")

    return region

def write_yield_data(data_file, cowid, st_or_mt_label, trt, ntests, x100, yldvec, herd305, dcrvec, mt_or_st, persvec, relpers, parity, curve_single):
    """
    Write yield data to a text file.

    :param data_file: File name prefix
    :param cowid: Cow ID (string of length 17)
    :param st_or_mt_label: List of 2-character strings (2 elements)
    :param trt: List of strings (from index -3 to 8, total 12 elements)
    :param ntests: List of 4 integers
    :param x100: List of 4 floats
    :param yldvec: 2D list (2x16) of floats
    :param herd305: 2D list (2x4) of floats
    :param dcrvec: List of 4 floats
    :param mt_or_st: List of 4 2-character strings
    :param persvec: List of 4 floats
    :param relpers: List of 4 floats
    :param parity: Integer
    :param curve_single: Integer (0 or 1)
    """
    # Form the complete file name
    curr_date = datetime.datetime.now().strftime('%Y%m%d')
    if curve_single == 0:
        long_data_file = f"{data_file}.{st_or_mt_label[0]}.{curr_date}"
    else:
        long_data_file = f"{data_file}.{cowid.strip()}.{st_or_mt_label[0]}.{curr_date}"

    # Open the file for appending
    with open(long_data_file, 'a') as f:
        # Write lactation yields, reliabilities, etc. to the text file
        for j in range(4):
            f.write(f"{cowid:17s} {parity:2d} {trt[j+3]:7s} {ntests[j]:4d} " +
                    f"{yldvec[0][j] * x100[j]:7.0f} {yldvec[0][j+4] * x100[j]:7.0f} " +
                    f"{yldvec[0][j+8] * x100[j]:7.0f} {yldvec[0][j+12] * x100[j]:7.0f} " +
                    f"{herd305[0][j] * x100[j]:7.0f} {yldvec[1][j] * x100[j]:7.0f} " +
                    f"{yldvec[1][j+4] * x100[j]:7.0f} {yldvec[1][j+8] * x100[j]:7.0f} " +
                    f"{yldvec[1][j+12] * x100[j]:7.0f} {herd305[1][j] * x100[j]:7.0f} " +
                    f"{dcrvec[j]:7.0f} {mt_or_st[j]:3s} {persvec[j]:5.2f} {relpers[j] * 100:4.0f}\n")

def write_curve_data(mtrait, curve_file, short_trait, cowid, st_or_mt_label, maxlen, dailybp, temp_td, std, parity, graf_plot, curve_small, curve_single):
    """
    Write curve data to a text file.

    :param mtrait: Integer
    :param curve_file: File name prefix
    :param short_trait: List of 4 single-character strings
    :param cowid: Cow ID (string of length 17)
    :param st_or_mt_label: List of 2-character strings (2 elements)
    :param maxlen: Integer
    :param dailybp: 3D list (2x4xmaxlen) of floats
    :param temp_td: 2D list (4xmaxlen) of floats
    :param std: 3D list (2x4xmaxlen) of floats
    :param parity: Integer
    :param graf_plot: List of 4 integers
    :param curve_small: Integer (0 or 1)
    :param curve_single: Integer (0 or 1)
    """
    # Form the complete file name
    curr_date = datetime.datetime.now().strftime('%Y%m%d')
    if curve_single == 0:
        long_curve_file = f"{curve_file}.{st_or_mt_label[0]}.{curr_date}"
    else:
        long_curve_file = f"{curve_file}.{cowid.strip()}.{st_or_mt_label[0]}.{curr_date}"

    # Open the file for appending
    with open(long_curve_file, 'a') as f:
        # Write the curves to the file
        for j in range(4):
            if graf_plot[j] != 0:
                for l in range(maxlen):
                    if curve_small == 1:
                        f.write(f"{cowid} {parity} {j+1} {l+1} {temp_td[j][l]} " +
                                f"{dailybp[1][j][l]} {std[1][j][l]}\n")
                    else:
                        f.write(f"{cowid} {parity} {j+1} {l+1} {temp_td[j][l]} " +
                                f"{dailybp[1][j][l]} {std[1][j][l]} " +
                                f"{dailybp[0][j][l]} {std[0][j][l]}\n")

def binsort(lst):
    """
    Binary sort of an integer list using repeated subset merges.
    Sorts the list in ascending order.
    :param lst: List of integers to be sorted
    """
    n = len(lst)
    if n < 2:
        return

    # Check if the list is already sorted
    for i in range(1, n):
        if lst[i] < lst[i - 1]:
            break
    else:
        return

    nseg = 0
    ibig = 1000000000
    nsrt = 1

    # Begin sort loop
    while nsrt < n:
        ip1 = 0
        ip2 = nsrt
        istop1 = ip2
        istop2 = ip2 + nsrt
        if istop2 > n:
            istop2 = n

        iwk = [0] * n

        i = 0
        while i < n:
            # Compare elements of two subsets
            if lst[ip1] <= lst[ip2]:
                iwk[i] = lst[ip1]
                ip1 += 1
                if ip1 < istop1:
                    i += 1
                    continue
                # First subset completed
                ip1 -= 1
                lst[ip1] = ibig
            else:
                iwk[i] = lst[ip2]
                ip2 += 1
                if ip2 < istop2:
                    i += 1
                    continue
                # Second subset completed
                ip2 -= 1
                lst[ip2] = ibig

            nseg += 1
            if nseg < 2:
                i += 1
                continue
            # Both subsets completed
            ip1 += nsrt + 1
            ip2 += nsrt + 1
            istop1 = ip1 + nsrt
            istop2 = ip2 + nsrt
            nseg = 0
            if istop2 > n:
                istop2 = n
            if ip2 < n:
                i += 1
                continue

            # No elements in last subset
            for j in range(ip1, n):
                iwk[j] = lst[j]
            break

        # Update list with sorted elements
        for i in range(n):
            lst[i] = iwk[i]

        if lst[-1] >= ibig:
            print("SOME ELEMENTS TOO BIG, INCREASE IBIG")
            return

        nsrt *= 2

def invrt2(a, cowid):
    """
    Subroutine to invert a non-symmetric matrix of order N.
    Matrix must be non-singular. Algorithm works best for
    positive definite matrices (no row interchanges).
    Note: Matrix a is overwritten with its inverse.

    :param a: 2D numpy array representing the matrix
    :param cowid: String (length 17) representing cow ID
    :return: Inverted matrix
    """
    n = a.shape[0]
    ia = a.shape[1]

    if n > ia:
        print("MATRIX DIMENSION IN INVRT2 LARGER THAN DECLARED")
        return None

    zero = 1e-12

    for i in range(n):
        # Check for singularity
        if abs(a[i, i]) < zero:
            print("Matrix not positive definite (diagonal=0) in subr invrt2")
            print("CowID =", cowid)
            if abs(a[i, i]) <= 0.0:
                raise ValueError("Matrix is singular.")

        # Transform the matrix
        save = 1.0 / a[i, i]
        for j in range(n):
            a[j, i] *= save
        a[i, i] = save
        for k in range(n):
            if a[i, k] == 0.0 or k == i:
                continue
            aik = a[i, k]
            for j in range(n):
                a[j, k] -= a[j, i] * aik
            a[i, k] = -1.0 * save * aik

    return a

def mult(a, b, c):
    """
    Matrix multiplication to compute A = B * C.

    :param a: 2D numpy array (N1 x N3) - Resultant matrix
    :param b: 2D numpy array (N1 x N2) - Matrix B
    :param c: 2D numpy array (N2 x N3) - Matrix C
    :return: 2D numpy array representing the result of B * C
    """
    n1, m3 = a.shape
    m1, n2 = b.shape
    m2, m3 = c.shape

    if n2 != m2 or n1 != m1:
        raise ValueError("Matrix dimensions do not match for multiplication.")

    for j in range(m3):
        for i in range(n1):
            a[i, j] = 0.0
        for k in range(n2):
            ckj = c[k, j]
            for i in range(n1):
                a[i, j] += b[i, k] * ckj

    return a

#---------------------------------------------------------------------
#                                            Expected daily yield from
#                                            Standard lactation curves
#
def ymean(trait, dim, maxlen, mrd, dyield, lacn, last, hratio, herd305):
    """
    Calculate the mean daily yield for a given trait and adjust it using the herd ratio.

    :param trait: Integer, trait identifier (1 to 4)
    :param dim: Integer, current dimension/day
    :param maxlen: Integer, maximum length of days
    :param mrd: Integer, number of previous days to average over
    :param dyield: 3D list or numpy array (maxlen x 4 x last) of daily yields
    :param lacn: Integer, lactation number
    :param last: Integer, last value (maximum index for the last dimension)
    :param hratio: List of 4 floats, herd ratios for each trait
    :param herd305: List of 4 floats, 305-day herd mean for each trait
    :return: Float, adjusted mean daily yield for the given trait
    """
    if trait < 1 or trait > 4:
        print(f"[ERROR]: An invalid value of trait, {trait}, was passed to ymean()!")
        print("[ERROR]: Subsequent calculations may be incorrect.")
        return 0.0

    # Initialize sum
    sum_yield = 0.0

    # Determine the range for averaging
    if trait == 1:
        ibegin = dim - mrd + 1
        iend = dim
    else:
        ibegin = dim - (mrd - 1) // 2
        iend = ibegin

    # Ensure the indices are within valid bounds
    ibegin = max(ibegin, 0)
    iend = min(iend, maxlen - 1)

    # Calculate the sum of daily yields over the specified range
    for i in range(ibegin, iend + 1):
        sum_yield += dyield[i][trait - 1][lacn]

    # Calculate the mean daily yield
    ymean_val = sum_yield / (iend - ibegin + 1)

    # Adjust for 305-day herd mean using the multiplicative herd ratio
    ymean_val *= hratio[trait - 1]

    return ymean_val

def covary(trt305, trait, dim, laclen, maxlen, mrd, covari, covd, sd, lacn, last, precise, stat):
    """
    Compute covariance of test day and lactation yield.

    :param trt305: Integer, 305-day trait identifier
    :param trait: Integer, trait identifier
    :param dim: Integer, dimension/day
    :param laclen: Integer, lactation length
    :param maxlen: Integer, maximum length of days
    :param mrd: Integer, number of previous days to average over
    :param covari: 5D list or numpy array, covariance table (4x4xmaxlenxmaxlenxlast)
    :param covd: 5D list or numpy array, covariance table for persistency (4x4xmaxlenxmaxlenxlast)
    :param sd: 3D list or numpy array, standard deviations (4xmaxlenxlast)
    :param lacn: Integer, lactation number
    :param last: Integer, last value (maximum index for the last dimension)
    :param precise: Integer, precision level
    :param stat: Integer, status flag
    :return: Float, computed covariance
    """
    def vary(i, trt305, *args, sd, lacn, last, maxlen):
        # Placeholder for the vary function. The actual implementation should be provided here.
        # This function is expected to compute variance based on provided parameters.
        return 1.0  # Example return value

    covary_val = 0.0
    if trait == 0:
        # Store covariances of any test day observation with true yield up to day laclen for trait trt305
        # Store DIM covariances needed for persistency
        iplus = (precise - 1) / 2.0
        for j in range(4):
            for k in range(maxlen):
                sum_var = 0.0
                sumd_var = 0.0
                # Save CPU if precise > 1
                for i in range(1, maxlen + 1, precise):
                    var = vary(i, trt305, 1, 2, 2, 1, k, j, 1, 2, 2, 1, sd, lacn, last, maxlen)
                    prec = min(precise, maxlen + 1 - i)
                    sum_var += var * prec
                    sumd_var += var * (i + iplus) * prec
                    if i <= 305 and k <= 305 and trt305 == j + 1:
                        covary_val += var * prec
                    covari[trt305 - 1][j][k][i - 1][lacn - 1] = sum_var
                    covd[trt305 - 1][j][k][i - 1][lacn - 1] = sumd_var
    else:
        # Look up covariances stored in the table or mean of covariances for MRD > 1
        mbegin = dim - mrd + 1
        mend = dim
        if trait > 1:
            mbegin = dim - (mrd - 1) // 2
            mend = mbegin

        if stat == 1:
            for m in range(mbegin, mend + 1):
                covary_val += covari[trt305 - 1][trait - 1][m - 1][laclen - 1][lacn - 1]
        else:
            # Covariances for persistency
            for m in range(mbegin, mend + 1):
                covary_val += covd[trt305 - 1][trait - 1][m - 1][laclen - 1][lacn - 1]

        covary_val /= (mend - mbegin + 1)

    return covary_val

def vary(dim1, trait1, super1, Xmilk1, sample1, MRD1, dim2, trait2, super2, Xmilk2, sample2, MRD2, sd, lacn, last, maxlen):
    """
    Compute covariance of any two test day yields.

    :param dim1: Integer, dimension/day for the first trait
    :param trait1: Integer, first trait identifier
    :param super1: Integer, supervision code for the first trait
    :param Xmilk1: Integer, milk yield for the first trait
    :param sample1: Integer, sample size for the first trait
    :param MRD1: Integer, number of previous days to average over for the first trait
    :param dim2: Integer, dimension/day for the second trait
    :param trait2: Integer, second trait identifier
    :param super2: Integer, supervision code for the second trait
    :param Xmilk2: Integer, milk yield for the second trait
    :param sample2: Integer, sample size for the second trait
    :param MRD2: Integer, number of previous days to average over for the second trait
    :param sd: 3D numpy array, standard deviations (4xmaxlenxlast)
    :param lacn: Integer, lactation number
    :param last: Integer, last value (maximum index for the last dimension)
    :param maxlen: Integer, maximum length of days
    :return: Float, computed covariance
    """

    dcr = np.array([0., 1., 0.77, 0.97, 1., 1., 0.77, 0.97, 1., 1.])
    mfpcorr = np.array([
        [1., 0.67, 0.85, -0.08],
        [0.67, 1., 0.77, -0.14],
        [0.85, 0.77, 1., -0.10],
        [-0.08, -0.14, -0.10, 1.]
    ] * 2).reshape((4, 4, 2))

    vary = 0.0

    ibegin = dim1 - MRD1 + 1
    iend = dim1
    jbegin = dim2 - MRD2 + 1
    jend = dim2

    if trait1 > 1:
        ibegin = dim1 - (MRD1 - 1) // 2
        iend = ibegin

    if trait2 > 1:
        jbegin = dim2 - (MRD2 - 1) // 2
        jend = jbegin

    for i in range(ibegin, iend + 1):
        for j in range(jbegin, jend + 1):
            dimdif = abs(i - j)
            Vmid = (i - i ** 2 / 365.0) * (j - j ** 2 / 365.0) / 91.25 ** 2
            Vlast = i * j / 365.0 ** 2
            Vscs = 0.995 ** dimdif

            if trait1 > 3 or trait2 > 3:
                Vscs = 0.0

            if i == j:
                Icorr = mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]
                Vmid = 1.0
                Vlast = 1.0
                if trait1 == trait2:
                    Vscs = 1.0
                Idiag = 1.0
            else:
                Idiag = 0.0

            if lacn == 1:
                if trait1 < 4 and trait2 < 4:
                    corr = (0.214 * Idiag + 0.786 * 0.998 ** dimdif) * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]
                elif trait1 == 4 and trait2 == 4:
                    corr = (0.199 * Idiag + 0.801 * 0.998 ** dimdif) * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]
                else:
                    sqc1 = np.sqrt(0.214 * Idiag + 0.786 * 0.998 ** dimdif)
                    sqc2 = np.sqrt(0.199 * Idiag + 0.801 * 0.998 ** dimdif)
                    corr = sqc1 * sqc2 * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]
            elif lacn > 1:
                if trait1 < 4 and trait2 < 4:
                    corr = (0.132 * Idiag + 0.868 * 0.997 ** dimdif) * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]
                elif trait1 == 4 and trait2 == 4:
                    corr = (0.199 * Idiag + 0.801 * 0.998 ** dimdif) * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]
                else:
                    sqc1 = np.sqrt(0.132 * Idiag + 0.868 * 0.997 ** dimdif)
                    sqc2 = np.sqrt(0.199 * Idiag + 0.801 * 0.998 ** dimdif)
                    corr = sqc1 * sqc2 * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]

            if trait1 == trait2 and i == j:
                corr = 1.0

            if i == j:
                corr += 0.3 * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1] * (1.0 * Xmilk1 / max(sample1, sample2) - 1.0)
                if trait1 == trait2 and dcr[super1] == 0.0:
                    corr = 1000000.0

            err1 = (1.0 / dcr[super1] - 1.0) * 0.62 if dcr[super1] > 0.0 else 0.0
            err2 = (1.0 / dcr[super2] - 1.0) * 0.62 if dcr[super2] > 0.0 else 0.0

            corr += np.sqrt(err1 * err2) * mfpcorr[trait1 - 1, trait2 - 1, lacn - 1]

            vary += corr * sd[trait1 - 1, i - 1, lacn - 1] * sd[trait2 - 1, j - 1, lacn - 1]

    vary /= (iend - ibegin + 1) * (jend - jbegin + 1)
    return vary

#  Adjust milk, fat, prot, SCS for 3X
def adjust3X(ntd, dim, length, yearfr, parity, Xmilk, meanyld, use3X, maxtd, last, maxlen):
    """
    Adjusts the test day yields based on 3X milking factors.

    :param ntd: Integer, number of test days
    :param dim: List of integers, dimensions/days (size: maxtd)
    :param length: Integer, length of the record
    :param yearfr: Integer, year factor for adjustment
    :param parity: Integer, parity of the cow
    :param Xmilk: List of integers, milk yields (size: maxtd)
    :param meanyld: 3D numpy array, cumulative daily yields (4 x maxlen x last)
    :param use3X: Integer, flag for type of adjustment (0 to 3)
    :param maxtd: Integer, maximum number of test days
    :param last: Integer, maximum index for the last dimension
    :param maxlen: Integer, maximum length of days
    :return: Tuple containing adjusted test3X, fact3X, and part3X
    """

    # Define constants and initial values
    new3X = np.array([[0.12, 0.09, 0.10, 0.00],
                      [0.14, 0.10, 0.11, 0.00],
                      [0.14, 0.10, 0.11, 0.00]])

    old3X = np.array([[0.20, 0.20, 0.20, 0.00],
                      [0.17, 0.17, 0.17, 0.00],
                      [0.15, 0.15, 0.15, 0.00]])

    dimsort = np.zeros(maxtd, dtype=int)
    freq = np.zeros(maxlen, dtype=int)
    test3X = np.ones((4, maxtd))
    fact3X = np.ones(4)
    part3X = np.ones(4)
    lac3X = np.zeros(4)
    yld3X = np.zeros(4)
    adj3X = np.zeros(4)

    # Determine the parity group
    lacn = min(parity, last)
    if lacn <= 0:
        lacn = last
    lacn3 = min(parity, 3)
    if lacn3 <= 0:
        lacn3 = 3

    # Calculate adjustment factors
    for j in range(4):
        if use3X == 0:
            adj3X[j] = 0.0
        elif use3X == 1:
            adj3X[j] = old3X[j, lacn3 - 1]
        elif use3X == 2:
            adj3X[j] = new3X[j, lacn3 - 1]
        elif use3X == 3:
            yrfr = min(yearfr, 1999)
            yrfr = max(yrfr, 1996)
            year0 = float(1999 - yrfr) / (1999 - 1996)
            year1 = 1.0 - year0
            adj3X[j] = year0 * old3X[j, lacn3 - 1] + year1 * new3X[j, lacn3 - 1]

        # Convert to 3X multiplier
        adj3X[j] = 1.0 / (1.0 + adj3X[j])

    # Return if no test days
    if ntd == 0:
        return test3X, fact3X, part3X

    ntd2 = 0
    for i in range(ntd):
        if dim[i] <= maxlen:
            dimsort[i] = dim[i]
            freq[dim[i] - 1] = Xmilk[i]
            ntd2 += 1

        for j in range(4):
            if Xmilk[i] > 2:
                test3X[j, i] = adj3X[j]

    if ntd2 == 0 or dimsort[0] > maxlen or dimsort[0] <= 0:
        return test3X, fact3X, part3X

    # Sort the dimsort array
    dimsort[:ntd2].sort()

    begin1 = 0
    begin2 = 0
    for i in range(ntd2):
        end1 = min(dimsort[i], 305)
        end2 = min(dimsort[i], length)

        for j in range(4):
            a3X = 1.0
            if freq[dimsort[i] - 1] > 2:
                a3X = adj3X[j]

            if i > 0:
                lac3X[j] -= meanyld[j, begin1 - 1, lacn - 1] / a3X
            lac3X[j] += meanyld[j, end1 - 1, lacn - 1] / a3X

            if i > 0:
                yld3X[j] -= meanyld[j, begin2 - 1, lacn - 1] / a3X
            yld3X[j] += meanyld[j, end2 - 1, lacn - 1] / a3X

        begin1 = end1
        begin2 = end2

    for j in range(4):
        if end1 < 305:
            lac3X[j] += (meanyld[j, 304, lacn - 1] - meanyld[j, end1 - 1, lacn - 1]) / adj3X[j]
        if end1 < length:
            yld3X[j] += (meanyld[j, length - 1, lacn - 1] - meanyld[j, end2 - 1, lacn - 1]) / adj3X[j]

        if lac3X[j] == 0.0:
            fact3X[j] = 1.0
        else:
            fact3X[j] = meanyld[j, 304, lacn - 1] / lac3X[j]

        if yld3X[j] == 0.0:
            part3X[j] = 1.0
        else:
            part3X[j] = meanyld[j, length - 1, lacn - 1] / yld3X[j]

    for i in range(ntd2):
        freq[dim[i] - 1] = 0

    return test3X, fact3X, part3X

# Valid methods are:
# ALL:  L: Linear interpolation for MFPS
# MFP:  W: Woods curves for MFP
#       C: Season-specific Woods curves for MFP
#       R: Region-specific Woods curves for MFP
#       T: Region- and season-specific Woods curves for MFP
# SCS:  G: Smooth curves (Morant and Gnanasakthy) for SCS
#       D: Season-specific M&G curves for SCS
#       S: Region-specific M&G curves for SCS
#       U: Region- and season-specific M&G curves for SCS
def interpolate(trait, month, dyield, lacn, dyld, sum_, meanyld, meanp, sd, dsd, method, DEBUGmsgs, maxlen, breed, region, season):
    """
    Interpolate between monthly means and standard deviations.

    :param trait: Integer, trait number
    :param month: Integer, month
    :param dyield: 3D numpy array, daily yields (maxlen x 4 x last)
    :param lacn: Integer, lactation number
    :param dyld: 3D numpy array, monthly yields (12 x 4 x last)
    :param sum_: Float, sum of yields
    :param meanyld: 3D numpy array, cumulative daily yields (4 x maxlen x last)
    :param meanp: 2D numpy array, mean yields (2 x 4)
    :param sd: 3D numpy array, standard deviations (4 x maxlen x last)
    :param dsd: 3D numpy array, monthly standard deviations (12 x 4 x last)
    :param method: Character, interpolation method
    :param DEBUGmsgs: Integer, debug messages flag
    :param maxlen: Integer, maximum length of days
    :param breed: Integer, breed number
    :param region: Integer, region number
    :param season: Integer, season number
    """
    woods_means = np.zeros((3, 3, 2, 6))  # Initialize with appropriate data
    woods_sd = np.zeros((3, 3, 2, 6))     # Initialize with appropriate data
    mandg_means = np.zeros((4, 1, 2, 6))  # Initialize with appropriate data
    mandg_sd = np.zeros((4, 1, 2, 6))     # Initialize with appropriate data

    regional_woods_means = np.zeros((3, 3, 2, 7))  # Initialize with appropriate data
    regional_woods_sd = np.zeros((3, 3, 2, 7))     # Initialize with appropriate data
    regional_mandg_means = np.zeros((4, 1, 2, 7))  # Initialize with appropriate data
    regional_mandg_sd = np.zeros((4, 1, 2, 7))     # Initialize with appropriate data

    calving_woods_means = np.zeros((3, 3, 2, 4))  # Initialize with appropriate data
    calving_woods_sd = np.zeros((3, 3, 2, 4))     # Initialize with appropriate data
    calving_mandg_means = np.zeros((4, 1, 2, 4))  # Initialize with appropriate data
    calving_mandg_sd = np.zeros((4, 1, 2, 4))     # Initialize with appropriate data

    seasonal_woods_means = np.zeros((3, 3, 2, 4, 7))  # Initialize with appropriate data
    seasonal_woods_sd = np.zeros((3, 3, 2, 4, 7))     # Initialize with appropriate data
    seasonal_mandg_means = np.zeros((4, 1, 2, 4, 7))  # Initialize with appropriate data
    seasonal_mandg_sd = np.zeros((4, 1, 2, 4, 7))     # Initialize with appropriate data

    dyield[:, trait, lacn] = 0.0
    sd[trait, :, lacn] = 0.0
    meanyld[trait, :, lacn] = 0.0

    # Check for valid interpolation methods
    if trait < 4:
        if method not in ['L', 'W', 'R', 'T', 'C']:
            print(f"[WARNING]: Invalid interpolation method, {method}, provided for trait {trait}. Using linear interpolation (L).")
            method = 'L'
    else:
        if method not in ['L', 'G', 'S', 'U', 'D']:
            print(f"[WARNING]: Invalid interpolation method, {method}, provided for SCS. Using linear interpolation (L).")
            method = 'L'

    # Default to Holstein if breed is invalid
    if breed < 1 or breed > 6:
        print(f"[WARNING]: Invalid breed, {breed}, provided for trait {trait}. Using Holstein (4).")
        breed = 4

    if method == 'L':
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            month = min(max(1, (i + 15) // 30), 11)
            dyield[i, trait, lacn] = ((i - month * 30 + 15) * dyld[month, trait, lacn] +
                                      ((month + 1) * 30 - 15 - i) * dyld[month - 1, trait, lacn]) / 30.0
            if i > 365:
                dyield[i, trait, lacn] = dyld[11, trait, lacn]
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= 305:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = ((i - month * 30 + 15) * dsd[month, trait, lacn] +
                                  ((month + 1) * 30 - 15 - i) * dsd[month - 1, trait, lacn]) / 30.0
            if i > 365:
                sd[trait, i, lacn] = dsd[11, trait, lacn]

    elif method == 'W':
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (woods_means[0, trait - 1, lacn - 1, breed - 1] *
                                      float(i) ** woods_means[1, trait - 1, lacn - 1, breed - 1] *
                                      np.exp(-float(i) * woods_means[2, trait - 1, lacn - 1, breed - 1]))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (woods_sd[0, trait - 1, lacn - 1, breed - 1] *
                                  float(i) ** woods_sd[1, trait - 1, lacn - 1, breed - 1] *
                                  np.exp(-float(i) * woods_sd[2, trait - 1, lacn - 1, breed - 1]))

    elif method == 'R':
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (regional_woods_means[0, trait - 1, lacn - 1, region - 1] *
                                      float(i) ** regional_woods_means[1, trait - 1, lacn - 1, region - 1] *
                                      np.exp(-float(i) * regional_woods_means[2, trait - 1, lacn - 1, region - 1]))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (regional_woods_sd[0, trait - 1, lacn - 1, region - 1] *
                                  float(i) ** regional_woods_sd[1, trait - 1, lacn - 1, region - 1] *
                                  np.exp(-float(i) * regional_woods_sd[2, trait - 1, lacn - 1, region - 1]))
        if DEBUGmsgs > 0:
            print(f'[R][{lacn}]: mean curve:', regional_woods_means[:, trait - 1, lacn - 1, region - 1])
            print(f'[R][{lacn}]: SD curve  :', regional_woods_sd[:, trait - 1, lacn - 1, region - 1])

    elif method == 'C':
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (calving_woods_means[0, trait - 1, lacn - 1, season - 1] *
                                      float(i) ** calving_woods_means[1, trait - 1, lacn - 1, season - 1] *
                                      np.exp(-float(i) * calving_woods_means[2, trait - 1, lacn - 1, season - 1]))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (calving_woods_sd[0, trait - 1, lacn - 1, season - 1] *
                                  float(i) ** calving_woods_sd[1, trait - 1, lacn - 1, season - 1] *
                                  np.exp(-float(i) * calving_woods_sd[2, trait - 1, lacn - 1, season - 1]))

    elif method == 'T':
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (seasonal_woods_means[0, trait - 1, lacn - 1, season - 1, region - 1] *
                                      float(i) ** seasonal_woods_means[1, trait - 1, lacn - 1, season - 1, region - 1] *
                                      np.exp(-float(i) * seasonal_woods_means[2, trait - 1, lacn - 1, season - 1, region - 1]))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (seasonal_woods_sd[0, trait - 1, lacn - 1, season - 1, region - 1] *
                                  float(i) ** seasonal_woods_sd[1, trait - 1, lacn - 1, season - 1, region - 1] *
                                  np.exp(-float(i) * seasonal_woods_sd[2, trait - 1, lacn - 1, season - 1, region - 1]))
        if DEBUGmsgs > 0:
            print(f'[T][{trait}, {lacn}, {season}, {region}]: mean curve:', seasonal_woods_means[:, trait - 1, lacn - 1, season - 1, region - 1])
            print(f'[T][{trait}, {lacn}, {season}, {region}]: SD curve  :', seasonal_woods_sd[:, trait - 1, lacn - 1, season - 1, region - 1])

    elif method == 'G':
        if trait != 4:
            print(f"[ERROR]: Invalid interpolation method, {method}, provided for trait {trait}. Morant and Gnanasankthy curves are provided only for SCS.")
            return
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (mandg_means[0, 0, lacn - 1, breed - 1] -
                                      mandg_means[1, 0, lacn - 1, breed - 1] * float(i + 10) +
                                      mandg_means[2, 0, lacn - 1, breed - 1] * float(i + 10) ** 2 +
                                      mandg_means[3, 0, lacn - 1, breed - 1] / float(i + 10))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (mandg_sd[0, 0, lacn - 1, breed - 1] -
                                  mandg_sd[1, 0, lacn - 1, breed - 1] * float(i + 10) +
                                  (mandg_sd[2, 0, lacn - 1, breed - 1] * float(i + 10) ** 2) / 2 +
                                  mandg_sd[3, 0, lacn - 1, breed - 1] / float(i + 10))

    elif method == 'S':
        if trait != 4:
            print(f"[ERROR]: Invalid interpolation method, {method}, provided for trait {trait}. Region-specific Morant and Gnanasankthy curves are provided only for SCS.")
            return
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (regional_mandg_means[0, 0, lacn - 1, region - 1] -
                                      regional_mandg_means[1, 0, lacn - 1, region - 1] * float(i + 10) +
                                      regional_mandg_means[2, 0, lacn - 1, region - 1] * float(i + 10) ** 2 +
                                      regional_mandg_means[3, 0, lacn - 1, region - 1] / float(i + 10))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (regional_mandg_sd[0, 0, lacn - 1, region - 1] -
                                  regional_mandg_sd[1, 0, lacn - 1, region - 1] * float(i + 10) +
                                  (regional_mandg_sd[2, 0, lacn - 1, region - 1] * float(i + 10) ** 2) / 2 +
                                  regional_mandg_sd[3, 0, lacn - 1, region - 1] / float(i + 10))
        if DEBUGmsgs > 0:
            print(f'[S][{lacn}]: mean curve:', regional_mandg_means[:, 0, lacn - 1, region - 1])
            print(f'[S][{lacn}]: SD curve  :', regional_mandg_sd[:, 0, lacn - 1, region - 1])

    elif method == 'D':
        if trait != 4:
            print(f"[ERROR]: Invalid interpolation method, {method}, provided for trait {trait}. Season-specific Morant and Gnanasankthy curves are provided only for SCS.")
            return
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (calving_mandg_means[0, 0, lacn - 1, season - 1] -
                                      calving_mandg_means[1, 0, lacn - 1, season - 1] * float(i + 10) +
                                      calving_mandg_means[2, 0, lacn - 1, season - 1] * float(i + 10) ** 2 +
                                      calving_mandg_means[3, 0, lacn - 1, season - 1] / float(i + 10))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (calving_mandg_sd[0, 0, lacn - 1, season - 1] -
                                  calving_mandg_sd[1, 0, lacn - 1, season - 1] * float(i + 10) +
                                  (calving_mandg_sd[2, 0, lacn - 1, season - 1] * float(i + 10) ** 2) / 2 +
                                  calving_mandg_sd[3, 0, lacn - 1, season - 1] / float(i + 10))

    elif method == 'U':
        if trait != 4:
            print(f"[ERROR]: Invalid interpolation method, {method}, provided for trait {trait}. Region and season-specific Morant and Gnanasankthy curves are provided only for SCS.")
            return
        sum_ = 0.0
        meanp[lacn, trait] = 0.0
        for i in range(maxlen):
            dyield[i, trait, lacn] = (seasonal_mandg_means[0, 0, lacn - 1, season - 1, region - 1] -
                                      seasonal_mandg_means[1, 0, lacn - 1, season - 1, region - 1] * float(i + 10) +
                                      seasonal_mandg_means[2, 0, lacn - 1, season - 1, region - 1] * float(i + 10) ** 2 +
                                      seasonal_mandg_means[3, 0, lacn - 1, season - 1, region - 1] / float(i + 10))
            sum_ += dyield[i, trait, lacn]
            meanyld[trait, i, lacn] = sum_
            if i <= maxlen:
                meanp[lacn, trait] += dyield[i, trait, lacn] * i
            sd[trait, i, lacn] = (seasonal_mandg_sd[0, 0, lacn - 1, season - 1, region - 1] -
                                  seasonal_mandg_sd[1, 0, lacn - 1, season - 1, region - 1] * float(i + 10) +
                                  (seasonal_mandg_sd[2, 0, lacn - 1, season - 1, region - 1] * float(i + 10) ** 2) / 2 +
                                  seasonal_mandg_sd[3, 0, lacn - 1, season - 1, region - 1] / float(i + 10))
        if DEBUGmsgs > 0:
            print(f'[U][{lacn}, {season}, {region}]: mean curve:', seasonal_mandg_means[:, 0, lacn - 1, season - 1, region - 1])
            print(f'[U][{lacn}, {season}, {region}]: SD curve  :', seasonal_mandg_sd[:, 0, lacn - 1, season - 1, region - 1])

    else:
        print(f"[ERROR]: Invalid interpolation method, {method}, for trait {trait}!")
