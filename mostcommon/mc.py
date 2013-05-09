import collections

freqs = [[0, chr(i + ord('A'))] for i in range(26)]

for l in open('mostcommon.words').readlines():
    # Leave out the '\n'
    for c in l[0:-1]:
        freqs[ord(c) - ord('A')][0] += 1
freqs.sort(reverse=True)

for freq, char in freqs:
    print "%s %d" % (char, freq)

