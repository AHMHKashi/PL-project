def range(a=0, b=10, step=1):
    if a == b:
        return [];
    else:
        return [a] + range(a + step, b, step);
    ;
;

print(range(0, 5));
print(range(0, 10, 2));
print(range(5, 0, 0-1));
