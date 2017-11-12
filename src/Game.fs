module Game

type GameState = {
  CurrentNote: Fretboard.Position;
  NotePropositions: Febus.Note list;
}

type State =
| InGame of GameState
| Won
| Failed

let proposeSolutions generator currentNotePosition =
  let note = currentNotePosition |> Fretboard.positionToNote
  let baseNote =
    generator 4
    |> Febus.intervalFromSemitones
    |> Febus.substractInterval note
  [ for s in 0..4 do yield Febus.addInterval baseNote (Febus.intervalFromSemitones s) ]

let verifySolution state proposedSolution =
  let solution = state.CurrentNote |> Fretboard.positionToNote
  if proposedSolution = solution then Ok solution else Error solution

let init =
  let notePosition =
    {
      Fretboard.String = Fretboard.String (IO.random 1 6);
      Fretboard.Cell = Fretboard.Cell (IO.random 0 12)
    }
  {
    CurrentNote = notePosition;
    NotePropositions = notePosition |> proposeSolutions (IO.random 0)
  }