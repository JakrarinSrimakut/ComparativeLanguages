Array extend [
   oddlen[
      |oddOrEven|
      oddOrEven := self size.
      (oddOrEven = 0) ifTrue: [^'false'].
      [oddOrEven ~= 1] whileTrue: [
         oddOrEven := oddOrEven-2.
         (oddOrEven = 0) ifTrue: [^'false'].
      ].
      ^'true'.
   ]
].

a:= #(1).
a oddlen printNl.

a:= #().
a oddlen printNl.

a:= #(1 2).
a oddlen printNl.
