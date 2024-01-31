def range(a=0, b=10):
    if a == b:
        return [];
    else:
        return [a] + range(a+1, b);
    ;
;

print(range(10, 20));

def len(l=[]):
    x = 0;
    for i in l:
        x = x + 1;
    ;
    return x;
;

arr = [5, 3, 4, 8];
print(len(arr));