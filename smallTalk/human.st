"human.st"
"The core version of the Human class."
Object subclass: Human [
	| name age |

	setName: aName [
		name := aName.
	]

	getName [
		^name
	]

	setAge: anAge [
		age := anAge.
	]

	getAge [
		^age
	]

	introduceYourself [
		Transcript show: 'Hello, my name is ', name, ' and
		I''m ', age printString, ' years old.'; cr.
	]

	> aHuman [
		^age > aHuman getAge
	]

	< aHuman [
		^age < aHuman getAge
	]

	= aHuman [
		^age = aHuman getAge
	]
]

| me myBrother |

me := Human new.
me setName: 'Canol Gökel'.
me setAge: 24.
myBrother := Human new.
myBrother setName: 'Gürol Gökel'.
myBrother setAge: 27.
me introduceYourself.
myBrother introduceYourself.
(me < myBrother) printNl.
