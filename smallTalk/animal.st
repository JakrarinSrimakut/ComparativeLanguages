"animal.st"
"A program which creates some animal classes to illustrate the
object oriented concepts."

Object subclass: Animal [
	| name |
	animalNumber := 0.
	<comment: 'A class for defining animals.'>

	Animal class >> setAnimalNumber: number [
		"A class method to set the animal number."
		<category: 'accessing'>
		animalNumber := number.
		^animalNumber
	]

	Animal class >> getAnimalNumber [
		"A class method to get the animal number."
		<category: 'accessing'>

		^animalNumber
	]

	setName: newName [
		"An instance method to set the animal's name."
		<category: 'accessing'>
		name := newName.
	]

	getName [
		"An instance method to get the animal's name."
		<category: 'accessing'>
		^name
	]
]

Animal subclass: Dog [
	<comment: 'A dog class.'>

	makeNoise [
		"An instance method to get the dog's noise."
		<category: 'accessing'>

		'Woof!' printNl.
	]
]
Animal subclass: Cat [
	<comment: 'A cat class.'>

	makeNoise [
		"An instance method to get the cat's noise."
		<category: 'accessing'>

		'Miaow!' printNl.
	]
]

dog := Dog new.
Animal setAnimalNumber: 1.
dog setName: 'Karabash'.
dog getName printNl.
dog makeNoise.

cat := Cat new.
Animal setAnimalNumber: 2.
cat setName: 'Minnosh'.
cat getName printNl.
cat makeNoise.
