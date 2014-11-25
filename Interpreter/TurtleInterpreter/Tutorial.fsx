#load "AST.fs"
#load "Interpreter.fsx"

// execute [Repeat(36,[Forward 10;Turn 10])]

// execute [Repeat(10,[Turn 36;Repeat(5,[Forward 54;Turn 72])])]

// execute [Pen Green;Repeat(10,[Turn 36;Repeat(5,[Forward 54;Turn 72;Pen Blue]);Pen Red])]

// execute [Repeat(10,[Turn 36;Repeat(5,[Forward 54;Up;Turn 72]);Down])]

execute [Pen Red;Repeat(10,[Turn 36;Repeat(5,[Forward 54;Up;Turn 72]);Down]);Turn 30;Forward 3;Pen Blue;Repeat(10,[Turn 36;Repeat(5,[Back 54;Up;Turn 72]);Down])]
