module TuttyFretty

open Fable.Import

let setState = GameRenderer.init (Browser.document.getElementById("fretboard")) Game.init