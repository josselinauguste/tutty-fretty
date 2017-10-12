module IO

open Fable.Import

let log x =
  Browser.console.log x
  x

let random =
  let rnd = System.Random()
  (fun min max -> rnd.Next(min, max))
