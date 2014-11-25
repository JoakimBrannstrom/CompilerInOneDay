(*
module AST =
   type distance = int
   type degrees = int
   type count = int
   type command =
      | Forward of distance
      | Turn of degrees      
      | Repeat of count * command list
*)
module AST =
    type name = string
    type param = string
    type arg = Number of int | Arg of param
    type command =
       | Forward of arg
       | Left of arg
       | Right of arg
       | SetRandomPosition
       | Repeat of arg * command list
       | Call of name * arg list
       | Proc of name * param list * command list

open AST
open System

let getVar = 
   let varId = ref 0
   fun () -> incr varId; sprintf "_%d" !varId 

let rec emitCommand command =
      match command with
      | Forward arg -> sprintf "forward(%s);" (emitArg arg)  
      | Left arg -> sprintf "turn(-(%s));" (emitArg arg)
      | Right arg -> sprintf "turn(%s);" (emitArg arg)
      | SetRandomPosition -> sprintf "set_random_position();"
      | Call(name, args) -> name + "(" + String.Join(",", [| for arg in args -> emitArg arg |]) + ");"
      | Proc(name, ps, block) -> "function " + name + "(" + String.Join(",", ps) + ") {\r\n" + emitBlock block + "\r\n}\r\n"
      | Repeat(n,commands) ->
         let block = emitBlock commands
         String.Format("for(var {0} = 0; {0} < {1}; {0}++) {{\r\n{2}\r\n}}", getVar(), emitArg n, block);
and emitArg arg = 
    match arg with
    | Number n -> sprintf "%d" n
    | Arg s -> s
and emitBlock commands =
   String.concat "" [|for command in commands -> emitCommand(command)|]

// let program = [Repeat(Number 36,[Forward (Number 2);Right (Number 10)])]

//     [Proc("square",[], [Repeat 4 [ifelse 1 = 0 [forward 10 right 90] [forward 50 right 90]]])];

let program = [
    Proc("square",[], [Repeat(Number 4, [Forward (Number 50); Right (Number 90)])]);
    Proc("flower",[],[Repeat(Number 36, [Right (Number 10); Call("square", [])])]);
    Proc("garden",["count"], [Repeat((Arg "count"),[SetRandomPosition;Call("flower", [])])]);
    Call("garden", [Number 5])]

let generatedJS = emitBlock program

let html = 
   sprintf """<html>
<body>
<canvas id="myCanvas" width="400" height="400" style="border:1px solid #000000;"></canvas>

<script type="text/javascript">
var c = document.getElementById("myCanvas");
var ctx = c.getContext("2d");
var width = 400;
var height = 400;
var x = width/2;
var y = height/2;
ctx.moveTo(x,y);
var a = 23.0;

function forward(n) {
  x += Math.cos((a*Math.PI)/180.0) * n;
  y += Math.sin((a*Math.PI)/180.0) * n;
  ctx.lineTo(x,y);
  ctx.stroke();
}

function turn(n) { a += n; }

// set-random-position - Note: need to escape minus sign
function set_random_position() {
  x = Math.random() * width;
  y = Math.random() * height; 
  ctx.moveTo(x,y); 
}

// Generated JS
%s

</script>
</body>
</html>""" generatedJS

open System.IO

//let path = Path.Combine(__SOURCE_DIRECTORY__, "TurtleGen.html")
let path = Path.Combine(__SOURCE_DIRECTORY__, "Turtle.html")
File.WriteAllText(path, html)

path |> System.Diagnostics.Process.Start 
