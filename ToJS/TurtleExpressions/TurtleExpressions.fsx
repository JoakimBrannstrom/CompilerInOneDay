type name = string
type param = string

type arithmetic =
   Add | Subtract | Multiply | Divide

type comparison =
   Eq | Ne | Lt | Gt | Le | Ge

type logical =
   And | Or

type expr =
   | Number of float 
   | String of string
   | Arg of param 
   | Var of name
   | Arithmetic of expr * arithmetic * expr
   | Comparison of expr * comparison * expr
   | Logical of expr * logical * expr

type condition =
   | Condition of expr * comparison * expr

type command =
   // Begin built-in functions
   | Forward of expr * expr
   | Back of expr
   | Left of expr
   | Right of expr
   | Random of expr
   | SetRandomPosition
   // End built-in functions
   | Repeat of expr * command list
   | Call of name * expr list
   | Proc of name * param list * command list
   | Make of name * expr
   | If of condition * command list
   | Stop

(* Logo Fractal tree
// http://rosettacode.org/wiki/Fractal_tree#Logo
to tree :depth :length :scale :angle
  if :depth=0 [stop]
  forward :length
  right :angle
  tree :depth-1 :length*:scale :scale :angle
  left 2*:angle
  tree :depth-1 :length*:scale :scale :angle
  right :angle
  back :length
end

tree 10 80 0.7 30
*)
let program = 
   [
   Proc("tree", ["depth"; "length"; "scale"; "angle"; "size"],
        [
        If(Condition(Arg("depth"),Eq,Number(0.0)),[Stop])
        Forward(Arg("length"),Arg("size"))
        Right(Arg("angle"))
        Call("tree",[Arithmetic(Arg("depth"),Subtract,Number(1.0));
                     Arithmetic(Arg("length"),Multiply,Arg("scale"));
                     Arg("scale");
                     Arg("angle");
                     Arithmetic(Arg("size"),Divide,Number(2.0))])
        Left(Arithmetic(Number(2.0),Multiply,Arg("angle")))
        Call("tree",[Arithmetic(Arg("depth"),Subtract,Number(1.0));
                     Arithmetic(Arg("length"),Multiply,Arg("scale"));
                     Arg("scale");
                     Arg("angle");
                     Arithmetic(Arg("size"),Divide,Number(2.0))])
        Right(Arg("angle"))
        Back(Arg("length"))
        ])
   Call("tree", [Number(15.0); Number(160.0); Number(0.7); Number(30.0); Number(50.0)])
   ]

open System

let getVar = 
   let varId = ref 0
   fun () -> incr varId; sprintf "_%d" !varId 

let rec emitBlock indent commands =
   String.concat "" [|for command in commands -> emitCommand indent command|]  
and emitCommand indent command =
   let tabs = String.replicate indent "\t"
   match command with
   | Forward(e,w) -> sprintf "forward(%s, %s);" (emitExpr e) (emitExpr w)
   | Back(e) -> sprintf "back(%s);" (emitExpr e)
   | Left(e) -> sprintf "left(%s);" (emitExpr e)
   | Right(e) -> sprintf "right(%s);" (emitExpr e)
   | Repeat(e,commands) ->
      let block = emitBlock (indent+1) commands
      String.Format("for({0}=0;{0}<{1};{0}++) {{\r\n {2}}}", getVar(), emitExpr e, block); 
   | Make(name,e) -> sprintf "var %s = %s;" name (emitExpr e)
   | If(Condition(lhs,op,rhs),commands) ->
      let condition = sprintf "%s%s%s" (emitExpr lhs) (fromComparison op) (emitExpr rhs)
      sprintf "if(%s) {\r\n%s%s}" condition (emitBlock (indent+1) commands) tabs
   | Stop -> "return;"
   | Proc(name,``params``,commands) ->
      sprintf "\r\n%sfunction %s(%s) {\r\n%s%s}" 
        tabs
         name 
         (String.concat "," ``params``) 
         (emitBlock (indent+1) commands) 
         tabs
   | Call(name,args) ->
      sprintf "%s(%s);" name (String.concat "," [for arg in args -> emitExpr arg])
   | _ -> failwith "Not implemented"
   |> fun s -> tabs + s + "\r\n"
and emitExpr expr =
   match expr with
   | Number(n) -> sprintf "%f" n //String.Format("{0}", n)
   | String(s) -> sprintf "\"%s\"" s
   | Arg(s) -> s
   | Var(s) -> s
   | Arithmetic(lhs,op,rhs) -> sprintf "%s%s%s" (emitExpr lhs) (fromArith op) (emitExpr rhs)
   | Comparison(lhs,op,rhs) -> sprintf "%s%s%s" (emitExpr lhs) (fromComparison op) (emitExpr rhs)
   | Logical(lhs,op,rhs) -> failwith "Not implemented"
and fromArith op =
   match op with
   | Add -> "+"
   | Subtract -> "-"
   | Divide -> "/"
   | Multiply -> "*"
and fromComparison op = 
   match op with
   | Eq -> "=="
   | Ne -> "!="
   | Lt -> "<"
   | Gt -> ">"
   | Le -> "<="
   | Ge -> ">=" 



let generatedJS = emitBlock 0 program

let html = 
   sprintf """<html>
<body>
<canvas id="myCanvas" width="600" height="600" style="border: 1px solid #000000;"></canvas>
<script type="text/javascript">
	var c = document.getElementById("myCanvas");
	var ctx = c.getContext("2d");
	var width = 600;
	var height = 600;
	var x = width / 2;
	var y = 590;
	ctx.moveTo(x, y);
	var a = -90.0;
	var lineWidth = 1;

	function getX(n) { return x + Math.cos((a * Math.PI) / 180.0) * n; }
	function getY(n) { return y + Math.sin((a * Math.PI) / 180.0) * n; }

	function forward(n, size) {
		size = Math.round(size);

		ctx.beginPath();
		ctx.moveTo(x, y);
		x = getX(n);
		y = getY(n);
		ctx.lineWidth = size;
		ctx.lineTo(x, y);
		ctx.stroke();
	}

	function back(n) {
		a = a - 180;

		x = getX(n);
		y = getY(n);
		ctx.moveTo(x, y);

		a = a + 180;
	}

	function turn(n) { a += n; }
	function left(n) { turn(-n); }
	function right(n) { turn(n); }

    // set-random-position - Note: need to escape minus sign
	function set_random_position() {
		x = Math.random() * width;
		y = Math.random() * height;
		ctx.moveTo(x, y);
	}

    // Generated JS
%s
</script>
</body>
</html>

""" generatedJS

open System.IO

//let path = Path.Combine(__SOURCE_DIRECTORY__, "TurtleGen.html")
let path = Path.Combine(__SOURCE_DIRECTORY__, "Turtle2.html")
File.WriteAllText(path, html)

path |> System.Diagnostics.Process.Start 
