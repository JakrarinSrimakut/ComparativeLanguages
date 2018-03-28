Array extend [
   max [
      (self size = 0) 
      ifTrue: [ ^nil ]
      ifFalse:[ |mx|
         mx := self at: 1.
         2 to: self size do: [:i|
            (mx < (self at: i)) ifTrue: [ mx := self at: i ].
         ].
         ^mx.
      ]
   ]
].

a:=#(3 1 4 1 5 9 2 6 5 3 5).
a max printNl.

#(333 9999 88 47) max printNl.

z := Array new.
z size.
z max printNl.
