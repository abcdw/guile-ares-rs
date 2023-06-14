NC=`guix build netcat-openbsd`/bin/nc

# printf "(+ 1 2)\n" | $NC -v -q 0 127.0.0.1 11211
printf "d2:op4:eval4:code1:+e" | $NC -v -q 0 127.0.0.1 ${1:-7888}
printf "d2:op4:eval4:code1:ee" | $NC -v -N 127.0.0.1 ${1:-7888}
