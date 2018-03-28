"5_lines.st"
"A program which prints 5 lines to demonstrate the usage of to:do:
message."

1 to: 5 do: [:x |
	Transcript show: 'This is the ', x printString, '. line.';
	cr.
]

