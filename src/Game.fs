module Game

type GameState = {
  CurrentNote: Fretboard.Position;
}

let init =
  { CurrentNote = { Fretboard.String = Fretboard.String (IO.random 1 6); Fretboard.Cell = Fretboard.Cell (IO.random 0 12) } }