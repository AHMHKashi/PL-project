def range(a=0, b=10, step=1):
    if a == b:
        return [];
    else:
        return [a] + range(a + step, b, step);
    ;
;

print(range(10, 20, 2));

def len(l=[]):
    x = 0;
    for i in l:
        x = x + 1;
    ;
    return x;
;

print(len([5, 3, 4, 8]));

def swap(arr=[], i=0, j=1):
    res = [];
    r = range(0, len(arr));
    for x in r:
        if x == i:
            res = res + [arr[j]];
        else:
            if x == j:
                res = res + [arr[i]];
            else:
                res = res + [arr[x]];
            ;
        ;
        print(res);
    ;
    return res;
;

a = [5, 8, 9, 3];
print(swap(a, 1, 3));
print(swap(a, 0, 1));