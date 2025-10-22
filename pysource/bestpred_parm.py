import configparser

def read_parms():
    # Initialize parameters with default values
    parms = {
        'laclen': 305,
        'maxlen': 365,
        'dailyfreq': 6,
        'plotfreq': 6,
        'use3X': 3,
        'mtrait': 3,
        'GLOBALmtrait': 3,
        'GRAFplot': [2, 2, 2, 2],
        'PERSfloor': -9.99,
        'PERSceiling': 9.99,
        'source': 11,
        'WRITEcurve': 0,
        'CURVEfile': 'cowcurve',
        'WRITEdata': 0,
        'DATAfile': 'cowdata',
        'INfile': 'pcdart.bpi',
        'OUTfile': 'pcdart.bpo',
        'maxprnt': 1,
        'ONscreen': 1,
        'obs': 99999999,
        'maxshow': 5,
        'maxtd': 50,
        'INTmethod': 'L',
        'INTmethodSCS': 'L',
        'DEBUGmsgs': 0,
        'UNITSin': 'P',
        'UNITSout': 'P',
        'breed11': 4,
        'breedUNK': 4,
        'dim0': [115, 115, 150, 155, 161, 152, 159, 148],
        'dim0flag': 0,
        'LOGon': 1,
        'LOGfile': 'example',
        'LOGfreq': 0,
        'CURVEsmall': 0,
        'CURVEsingle': 0,
        'region': 2,
        'season': 1,
    }

    # Read parameters from file
    config = configparser.ConfigParser()
    config.read('bestpred.par')

    # Update parameters with values from file, if available
    if 'bestpred' in config:
        bestpred = config['bestpred']
        for key in parms:
            if key in bestpred:
                if isinstance(parms[key], list):
                    parms[key] = [int(x) for x in bestpred[key].split(',')]
                elif isinstance(parms[key], int):
                    parms[key] = int(bestpred[key])
                elif isinstance(parms[key], float):
                    parms[key] = float(bestpred[key])
                else:
                    parms[key] = bestpred[key]

    # Validate parameters
    if not (0 <= parms['laclen'] <= 999):
        parms['laclen'] = 305
    if not (0 <= parms['maxlen'] <= 999):
        parms['maxlen'] = 365
    if parms['maxlen'] < parms['laclen']:
        parms['maxlen'] = parms['laclen']
    if not (0 <= parms['dailyfreq'] <= 999):
        parms['dailyfreq'] = 6
    if not (0 <= parms['plotfreq'] <= 999):
        parms['plotfreq'] = 6
    if not (0 <= parms['use3X'] <= 3):
        parms['use3X'] = 3
    if not (1 <= parms['mtrait'] <= 4):
        parms['mtrait'] = 3
    if not (1 <= parms['GLOBALmtrait'] <= 4):
        parms['GLOBALmtrait'] = 3
    parms['GRAFplot'] = [x if 0 <= x <= 2 else 2 for x in parms['GRAFplot']]
    if parms['PERSfloor'] > 0:
        parms['PERSfloor'] = -9.99
    if parms['PERSceiling'] < 0:
        parms['PERSceiling'] = 9.99
    if parms['source'] not in [10, 11, 12, 14, 15, 24]:
        parms['source'] = 11
    if parms['WRITEcurve'] not in [0, 1]:
        parms['WRITEcurve'] = 0
    if not parms['CURVEfile']:
        parms['CURVEfile'] = 'cowcurve'
    if parms['WRITEdata'] not in [0, 1]:
        parms['WRITEdata'] = 0
    if not parms['DATAfile']:
        parms['DATAfile'] = 'cowdata'
    if not parms['INfile']:
        parms['INfile'] = 'pcdart.bpi'
    if not parms['OUTfile']:
        parms['OUTfile'] = 'pcdart.bpo'
    if parms['maxprnt'] < 1:
        parms['maxprnt'] = 1
    if parms['ONscreen'] not in [0, 1]:
        parms['ONscreen'] = 1
    if parms['obs'] < 0:
        parms['obs'] = 99999999
    if not (0 <= parms['maxshow'] <= parms['obs']):
        parms['maxshow'] = 5
    if parms['maxtd'] < 1:
        parms['maxtd'] = 50
    if parms['INTmethod'] not in ['L', 'W', 'R']:
        parms['INTmethod'] = 'L'
    if parms['INTmethodSCS'] not in ['L', 'G', 'S']:
        parms['INTmethodSCS'] = 'L'
    if not (0 <= parms['DEBUGmsgs'] <= 2):
        parms['DEBUGmsgs'] = 0
    if parms['UNITSin'] not in ['P', 'K']:
        parms['UNITSin'] = 'P'
    if parms['UNITSout'] not in ['P', 'K']:
        parms['UNITSout'] = 'P'
    if not (1 <= parms['breed11'] <= 6):
        parms['breed11'] = 4
    if not (1 <= parms['breedUNK'] <= 6):
        parms['breedUNK'] = 4
    parms['dim0'] = [x if 1 <= x <= parms['laclen'] else default for x, default in zip(parms['dim0'], [115, 115, 150, 155, 161, 152, 159, 148])]
    if parms['dim0flag'] not in [0, 1]:
        parms['dim0flag'] = 0
    if not (0 <= parms['LOGon'] <= 1):
        parms['LOGon'] = 1
    if not parms['LOGfile']:
        parms['LOGfile'] = 'example'
    if parms['LOGfreq'] <= 0:
        parms['LOGfreq'] = 0
    if parms['CURVEsmall'] not in [0, 1]:
        parms['CURVEsmall'] = 0
    if parms['CURVEsingle'] not in [0, 1]:
        parms['CURVEsingle'] = 0
    if not (1 <= parms['region'] <= 7):
        parms['region'] = 2
    if not (1 <= parms['season'] <= 4):
        parms['season'] = 1

    return parms

# Example usage
parms = read_parms()
print(parms)
