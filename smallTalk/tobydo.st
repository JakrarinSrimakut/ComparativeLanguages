"tobydo.st"
"A program to demonstrate the usage of to:by:do: message."
1 to: 10 by: 2 do: [:x |
Transcript show: 'This is the ', x printString, '. line.';
cr.
]
