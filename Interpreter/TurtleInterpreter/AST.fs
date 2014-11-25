[<AutoOpen>]
module AST

type arg = int
type color = Red | Green | Blue
type command =
   | Forward of arg
   | Back of arg
   | Turn of arg
   | Up
   | Down
   | Pen of color
   | Repeat of arg * command list
