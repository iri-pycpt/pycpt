import datetime as dt

months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
possible_targets = "JFMAMJJASONDJFMAMJJASOND"

def find_target(target):
    ndx = 0 
    while possible_targets[ndx:ndx+len(target)] != target and ndx + len(target) <= 24:
        ndx += 1 
    assert ndx + len(target) <= 24, "{} Target Not Found".format(target)
    return months[ndx % 12], months[(ndx + len(target)) % 12 - 1]
     
def init_from_lead(lead, tstart):
    return months[ (months.index(tstart) - lead) % 12 ]

threeletters = [None, 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
monthlengths = [ 0, 31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
def parse_target(target):
    """Gets a numerical month from three-letter month abbreviations"""
    assert ('-' in target and len(target) == 7) or len(target) == 3, 'TARGET must be in three-letter format for one month, or a range of months. For example, "Jun" or "Jun-Jul"'
    low, high = target.split('-') if '-' in target else (target, target)
    return threeletters.index(low), threeletters.index(high)

def leads_from_target(fdate, target):
    """calculates lead_low and lead_high"""
    target_low, target_high = parse_target(target)
    #print(target_low, target_high, fdate.month)
    lead_low = 12 - (fdate.month - target_low) + 0.5 if target_low < fdate.month else (target_low - fdate.month) + 0.5 
    lead_high = 12 - (fdate.month - target_high) + 0.5 if target_high < fdate.month else (target_high - fdate.month) + 0.5 
    #print(target_low, target_high, fdate.month, lead_low, lead_high)

    return lead_low, lead_high

def target_from_leads(fdate, lead_low, lead_high):
    low_ndx = fdate.month + int(lead_low) if fdate.month + int(lead_low) <= 12 else fdate.month + int(lead_low) - 12
    high_ndx = fdate.month + int(lead_high) if fdate.month + int(lead_high) <= 12 else fdate.month + int(lead_high) - 12
    target_low = threeletters[low_ndx]
    target_high = threeletters[high_ndx]  
    return target_low if target_low == target_high else '{}_{}'.format(target_low, target_high)

def seasonal_target_length(target): 
    # target must be three-letter-abbr style 
    if '-' in target: 
        total = 0
        start, end = target.split('-')
        start_ndx, end_ndx = threeletters.index(start), threeletters.index(end)
        if end_ndx < start_ndx: # cross-year 
            total += sum(monthlengths[start_ndx:])
            total += sum(monthlengths[:end_ndx+1])
            return total
        else: 
            return sum(monthlengths[start_ndx:end_ndx+1])
    else: 
        return monthlengths[threeletters.index(target)]

def seasonal_target_length_monthly(target): 
    # target must be three-letter-abbr style 
    if '-' in target: 
        total = 0
        start, end = target.split('-')
        start_ndx, end_ndx = threeletters.index(start), threeletters.index(end)
        if end_ndx < start_ndx: # cross-year 
            total += 12 - start_ndx + 1
            total += end_ndx 
            return total
        else: 
            return end_ndx - start_ndx +1
    else: 
        return monthlengths[threeletters.index(target)]

def seasonal_target(fdate, target, lead_low, lead_high):
    if target is not None and lead_high is not None and lead_low is not None: # if all are supplied
        ll, lh = leads_from_target(fdate, target)
        assert ll == lead_low, 'Provided lead_low incompatible with provided target and fdate - calculated {} but given {}'.format(ll, lead_low)
        assert ll == lead_low, 'Provided lead_high incompatible with provided target and fdate - calculated {} but given {}'.format(lh, lead_high)
        return fdate, target, lead_low, lead_high 
    elif target is not None and lead_low is None and lead_high is None: # if we only get a target
        ll, lh = leads_from_target(fdate, target)
        return fdate, target, ll, lh 
    elif target is None and lead_low is not None and lead_high is not None: # if we get leads and no target
        target = target_from_leads(fdate, lead_low, lead_high)
        return fdate, target, lead_low, lead_high 
    else: 
        assert False, 'Must provide either a TARGET in three-letter format for one month, or a range of months, or a set of lead-times, or all three which agree'


def subx_target(target=None, lead_low=None, lead_high=None):
    leads = { 'week1': (1, 7), 'week2': (7, 14), 'week3': (14, 21), 'week4':(21, 28), 'week34': (14, 28), 'week12': (1, 14), 'week23': (7, 21)}
    lr = {leads[r]: r for r in leads.keys() }  
    if target is not None and lead_high is not None and lead_low is not None: # if all are supplied
        assert target in leads.keys(), 'invalid subx target {}'.format(target)
        assert lead_high == leads[target][1] and lead_low == leads[target][0], 'target incompatible with leads '
    elif target is not None and lead_low is None and lead_high is None: 
        assert target in leads.keys(), 'invalid subx target {}'.format(target)
        lead_low, lead_high = leads[target]
    elif target is None and lead_low is not None and lead_high is None: 
        assert False, 'must either provide a target, or both leads, or a set of all three which agree'
    elif target is None and lead_low is None and lead_high is not None: 
        assert False, 'must either provide a target, or both leads, or a set of all three which agree'
    else:
        target  = lr[(lead_low, lead_high)] if (lead_low, lead_high) in lr.keys() else 'custom'
    return target, lead_low, lead_high

    
def first_hdate_in_training_season(fdate):
    start = dt.datetime(fdate.year, fdate.month, 1)
    while start.weekday() != 4: 
        start = start + dt.Timedelta(days=1)
    return start

def targetcenter(target):
    ts, te = parse_target(target)
    if ts == te: 
        return threeletters[ts] 
    else: 
        if te > ts: 
            ndx = (te + ts ) // 2 
            if ndx > 12: 
                ndx -= 12
            return threeletters[ndx]
        else: 
            ndx = (te + 12 + ts ) // 2 
            if ndx > 12: 
                ndx -= 12
            return threeletters[ndx]