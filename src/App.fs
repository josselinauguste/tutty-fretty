module TuttyFretty

open Fable.Import
open Elmish

let init () =
  Game.InGame Game.init

type Msg =
| ChooseNote of int

let update msg state =
  match msg with
  | ChooseNote choice ->
    match state with
    | Game.InGame gameState ->
      let note = gameState.NotePropositions.[choice]
      match Game.verifySolution gameState note with
      | Ok _ -> Game.Won
      | Error _ -> Game.Failed
    | _ -> state

let setState = GameRenderer.init (Browser.document.getElementById("fretboard")) (init ()) ChooseNote

Program.mkSimple init update setState
|> Program.run