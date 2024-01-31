def range(a=0, b=10):
    if a == b:
        return [];
    else:
        return [a] + range(a+1, b);
    ;
;

print(range(0, 20));
