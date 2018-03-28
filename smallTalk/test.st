"test.st"
"practicing perpose"

b2:=[:a :b|a+b].
(b2 value:3 value: 4) printNl

ourVariable:=false.

ourVariable ifTrue: [
     'Our variable is true.' printNl.
]  ifFalse: [
     'Our variable is false.' printNl.
] 
