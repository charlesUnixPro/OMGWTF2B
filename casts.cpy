       01 wordCast based.
           05 wordCasted usage binary-short unsigned.

       01 byteCast based.
           05 byteCasted usage binary-char unsigned.

       01 byteCast1 based.
           05 byteCasted1 usage binary-char unsigned.

       01 byteCast2 based.
           05 byteCasted2 usage binary-char unsigned.

       01 intCast based.
           05 intCasted usage binary-long signed.

       01 doubleCast based.
           05 doubleCasted usage computational-2.

       01 ptrCast based.
           05 ptrCasted usage pointer.

       01 svalCast based.
           05 svalCasted usage binary-char unsigned.

       01 dispCast based.
           05 dispCasted pic x(256) usage is display.

       01 countedString based.
           05 cs-len usage binary-char unsigned.
           05 cs-chars occurs 256 times.
               10 cs-char usage binary-char unsigned.

