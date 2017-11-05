module Fretboard

type Position = {
  String: String;
  Cell: Cell
}
and String = String of int
and Cell = Cell of int

let rec positionToNote position =
  match position with
  | { Cell = Cell 0 } ->
    let openStringNotes = [Febus.E; Febus.B; Febus.G; Febus.D; Febus.A; Febus.E]
    match position.String with
    | String s when s >= 1 && s <= 6 -> openStringNotes.[s - 1]
    | _ -> invalidArg "position" "Incorrect string"
  | _ ->
    let baseNote = positionToNote { position with Cell = Cell 0 }
    let (Cell cellIndex) = position.Cell
    let cellInterval = Febus.intervalFromSemitones cellIndex
    Febus.addInterval baseNote cellInterval