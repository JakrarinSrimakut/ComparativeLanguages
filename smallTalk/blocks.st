"block.st"
"A program which involves a block with an argument."


|greetings|

greetings := [:platesOfCornFlakes | 'I have eaten ', platesOfCornFlakes printString, ' plates of corn flakes this morning!'].

('hello ma! ', (greetings value: 3)) printNl.
