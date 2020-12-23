input = open("input.txt")
lines = input.readlines()


def doSlope(right, down):
    count = 0
    rPos = 0
    dPos = -1
    for line in lines:
        dPos += 1
        if dPos % down != 0:
            continue
        if line[rPos] == '#':
            count += 1
        rPos = (rPos + right) % (len(line) - 1)
    print(r, d, count)
    return count

slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
mult = 1
for r, d in slopes:
    mult *= doSlope(r, d)
print(mult)
