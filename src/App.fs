module TuttyFretty

open Fable.Import
open Elmish

let init () =
  Game.init

let update _msg model =
  model

let setState = GameRenderer.init (Browser.document.getElementById("fretboard")) (init  ())

Program.mkSimple init update (fun model _ -> setState model)
|> Program.run