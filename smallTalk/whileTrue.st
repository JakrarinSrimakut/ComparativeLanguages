"average.st"
"A program which evaluates the sum of the numbers entered to
demonstrate the whileTrue: message."

| sum enteredIntegers lastEnteredInteger |

sum := 0.
enteredIntegers := 0.

[ lastEnteredInteger ~= -1 ] whileTrue: [
	Transcript cr; show: 'Please enter a number. To exit the
	program enter -1: '.
	
	lastEnteredInteger := stdin nextLine asInteger.
	sum := sum + lastEnteredInteger.

	enteredIntegers := enteredIntegers + 1.

	Transcript show: 'The average of the numbers entered so far
	is: ', (sum / enteredIntegers) printString; cr.
]
