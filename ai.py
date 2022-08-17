from itertools import chain, combinations, product

import sys
sys.setrecursionlimit(100000)

class fs(frozenset):
    def __init__(self, *args, **kwargs):
        # super().__init__(*args, **kwargs)
        frozenset.__init__(self)
        self.froz = frozenset([]) if len(args) == 0 else args[0]
        
    def __str__(self):
        if self.froz == frozenset():
            return r'{}'
        else:
            return '{' + ','.join(f'{x}' for x in list(self.froz)) + '}'
    def __repr__(self):
        return self.__str__()

def powerset(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))

def P(s):
    return fs([fs(list(x)) for x in list(powerset(s))])


chicks = fs({1,2,3})
chickStates = [(a,b) for a in P(chicks) for b in P(chicks) if a.isdisjoint(b) and set(list(a)+list(b)) == chicks]

wolves = fs({1,2,3})
wolfeStates = [(a,b) for a in P(wolves) for b in P(wolves) if a.isdisjoint(b) and set(list(a)+list(b)) == wolves]

boatStates = ['L','R']

states = list(product(chickStates,wolfeStates,boatStates))

def trans(b,l_c,r_c,l_w,r_w,nl_c,nr_c,nl_w,nr_w):
    if (len(nl_c) < len(nl_w) and len(nl_c) > 0) or (len(nr_c) < len(nr_w) and len(nr_c) > 0):
        return False
    if b == 'L':
        d = abs((len(l_c) + len(l_w)) - (len(nl_c) + len(nl_w)))
        return d >= 1 and d <= 2
    elif b == 'R':
        d = abs((len(r_c) + len(r_w)) - (len(nr_c) + len(nr_w)))
        return d >= 1 and d <= 2
    else:
        print('Non Exhaustive!')
        return False

def delta(q,c):
    ((l_c,r_c),(l_w,r_w),b) = q
    if c == 'R' and b == 'L':
        return [((nl_c,nr_c),(nl_w,nr_w),'R') 
                    for ((nl_c,nr_c),(nl_w,nr_w),nb) in states
                    if trans(b,l_c,r_c,l_w,r_w,nl_c,nr_c,nl_w,nr_w)
                    and nb == 'R'
                ]
    elif c == 'L' and b == 'R':
        return [((nl_c,nr_c),(nl_w,nr_w),'L') 
                    for ((nl_c,nr_c),(nl_w,nr_w),nb) in states
                    if trans(b,l_c,r_c,l_w,r_w,nl_c,nr_c,nl_w,nr_w)
                    and nb == 'L'
                ]
    else:
        return []

q0 = ((chicks,fs([])),(wolves,fs([])),'L')

visited = fs([])
current = [q0]
while len(current) > 0:
    q = current.pop(0)
    visited = visited.union(fs([q]))
    qsL = delta(q,'L')
    qsR = delta(q,'R')
    qs = [q_ for q_ in qsL + qsR if not (q_ in visited or q_ in current)]
    current += qs
    print(q,len(visited),len(current))


def final(q):
    ((l_c,r_c),(l_w,r_w),b) = q
    return len(l_c) == 0 and len(l_w) == 0 and b == 'R'

def recurse(q,seen,trav):


    if final(q):
        seen += [q]
        print('Found!', len(trav + [q]))
        raise Exception()
        print()
        return (q,fs([]))
    

    nextsL = delta(q,'L')
    childsL = fs([recurse(q_,seen, trav + [q]) for q_ in nextsL if (not q_ in trav) and q_ != q])

    nextsR = delta(q,'R')
    childsR = fs([recurse(q_,seen, trav + [q]) for q_ in nextsR if (not q_ in trav) and q_ != q])

    childs = fs(list(childsL.union(childsR)))

    # childs = fs([recurse(q_,seen,trav + [q]) for q_ in (delta(q,'L') + delta(q,'R')) if (not q_ in trav) and q_ != q])
    
    seen += [q]

    return (q,None if childs == fs([]) else childs)


print(recurse(q0,[],[]))