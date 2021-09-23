import time

timing_info = []

def timing(f):
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        timing_info.append('{:s} function took {:.3f} ms'.format(f.__qualname__, (time2-time1)*1000.0))
        return ret
    return wrap

