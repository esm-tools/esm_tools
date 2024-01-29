import numexpr as ne
import numpy as np

class point:
    def __init__(self, x, y):
        self.x = x
        self.y = y
    def to_dict(self):
        return {'x': self.x, 'y': self.y}


# https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
def sign(p1, p2, p3):
    return (p1.x - p3.x) * (p2.y - p3.y) - (p2.x - p3.x) * (p1.y - p3.y)

def PointInTriangle(pt, v1, v2, v3):
    d1 = sign(pt, v1, v2)
    d2 = sign(pt, v2, v3)
    d3 = sign(pt, v3, v1)

    has_neg = (d1 < 0) or (d2 < 0) or (d3 < 0)
    has_pos = (d1 > 0) or (d2 > 0) or (d3 > 0)

    return not (has_neg and has_pos)

def PointTriangle_distance(lon0, lat0, lon1, lat1, lon2, lat2, lon3, lat3):
    d1 = ne.evaluate('(lon1 - lon0)**2 + (lat1 - lat0)**2')
    d2 = ne.evaluate('(lon2 - lon0)**2 + (lat2 - lat0)**2')
    d3 = ne.evaluate('(lon3 - lon0)**2 + (lat3 - lat0)**2')
    
    dis = d1+d2+d3
    ind = np.where(dis == np.amin(dis))
   
    p1 = point(lon1[ind], lat1[ind])
    p2 = point(lon2[ind], lat2[ind])
    p3 = point(lon3[ind], lat3[ind])

    return [p1, p2, p3], ind[0][0]

