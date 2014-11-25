[<AutoOpen>]
module Interpreter

#if INTERACTIVE
#load "AST.fs"
#r "System.Drawing.dll"
#r "System.Windows.Forms.dll"
#endif

open AST
open System
open System.Drawing
open System.Windows.Forms

type Turtle = { X:float; Y:float; Angle:int; Draw:bool; Color:Color }

let execute commands =
   let form = new Form (Text="Small Logo", Width=640, Height=480)
   let width, height = 500, 500
   let image = new Bitmap(width, height)
   let picture = new PictureBox(Dock=DockStyle.Fill, Image=image)
   do  form.Controls.Add(picture)
   let turtle = { X=float width/2.0; Y=float height/2.0; Angle = -90; Draw = true; Color = Color.Red }
   // let pen = ref (new Pen(Color.Red))
   
   let drawLine (x1,y1) (x2,y2) (color:Color) =
      let pen = new Pen(color)
      use graphics = Graphics.FromImage(image)
      graphics.DrawLine(pen,int x1,int y1,int x2, int y2)
   
   let move n turtle =
      let r = float turtle.Angle * Math.PI / 180.0
      let dx, dy = float n * cos r, float n * sin r
      let x, y =  turtle.X, turtle.Y
      let x',y' = x + dx, y + dy
      if turtle.Draw then drawLine (x,y) (x',y') turtle.Color
      { turtle with X = x'; Y = y' }

   let getPen color = 
      match color with
      | Red -> Color.Red
      | Green -> Color.Green
      | Blue -> Color.Blue

   let rec perform turtle = function
      | Forward n -> move n turtle        
      | Back n -> move -n turtle
      | Up -> { turtle with Draw = false }
      | Down -> { turtle with Draw = true }
      | Turn n -> { turtle with Angle = turtle.Angle + n }
      | Pen color -> { turtle with Color = getPen(color) }
      | Repeat(n,commands) ->
         let rec repeat turtle = function
            | 0 -> turtle
            | n -> repeat (performAll turtle commands) (n-1)
         repeat turtle n
   and performAll = List.fold perform

   performAll turtle commands |> ignore
   
   form.ShowDialog() |> ignore




