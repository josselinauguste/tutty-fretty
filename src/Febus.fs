module Febus

type Note = C | CSharp | DFlat | D | DSharp | EFlat | E | F | FSharp | GFlat | G | GSharp | AFlat | A | ASharp | BFlat | B
type Interval = Unison | DiminishedSecond | MinorSecond | AugmentedUnison | MajorSecond | DiminishedThird | MinorThird | AugmentedSecond | MajorThird | DiminishedFourth | Fourth | AugmentedThird | AugmentedFourth | DiminishedFifth | Fifth | DiminishedSixth | MinorSixth | AugmentedFifth | MajorSixth | DiminishedSeventh | MinorSeventh | AugmentedSixth | MajorSeventh

module internal SemitoneArithmetic =
  let noteToSemitonesFromC = function
  | C -> 0
  | CSharp -> 1
  | DFlat -> 1
  | D -> 2
  | DSharp -> 3
  | EFlat -> 3
  | E -> 4
  | F -> 5
  | FSharp -> 6
  | GFlat -> 6
  | G -> 7
  | GSharp -> 8
  | AFlat -> 8
  | A -> 9
  | ASharp -> 10
  | BFlat -> 10
  | B -> 11

  let private reverseIfNegative semitones =
    semitones % 12 |> (fun s -> if s < 0 then s + 12 else s)

  let semitonesToNoteFromC semitones =
    match (semitones |> reverseIfNegative) with
    | 0 -> C
    | 1 -> CSharp
    | 2 -> D
    | 3 -> DSharp
    | 4 -> E
    | 5 -> F
    | 6 -> FSharp
    | 7 -> G
    | 8 -> GSharp
    | 9 -> A
    | 10 -> ASharp
    | 11 -> B
    | _ -> invalidArg "semitones" "must be greater or equal to 0"

  let intervalToSemitones = function
  | Unison -> 0
  | DiminishedSecond -> 0
  | MinorSecond -> 1
  | AugmentedUnison -> 1
  | MajorSecond -> 2
  | DiminishedThird -> 2
  | MinorThird -> 3
  | AugmentedSecond -> 3
  | MajorThird -> 4
  | DiminishedFourth -> 4
  | Fourth -> 5
  | AugmentedThird -> 5
  | AugmentedFourth -> 6
  | DiminishedFifth -> 6
  | Fifth -> 7
  | DiminishedSixth -> 7
  | MinorSixth -> 8
  | AugmentedFifth -> 8
  | MajorSixth -> 9
  | DiminishedSeventh -> 9
  | MinorSeventh -> 10
  | AugmentedSixth -> 10
  | MajorSeventh -> 11

  let intervalFromSemitones semitones =
    match (semitones |> reverseIfNegative) with
    | 0 -> Unison
    | 1 -> MinorSecond
    | 2 -> MajorSecond
    | 3 -> MinorThird
    | 4 -> MajorThird
    | 5 -> Fourth
    | 6 -> AugmentedFourth
    | 7 -> Fifth
    | 8 -> AugmentedFifth
    | 9 -> MajorSixth
    | 10 -> MinorSeventh
    | 11 -> MajorSeventh
    | _ -> invalidArg "semitones" "must be greater or equal to 0"

let addInterval note = function
| Unison -> note
| interval ->
  let semitonesFromC = (SemitoneArithmetic.noteToSemitonesFromC note) + (SemitoneArithmetic.intervalToSemitones interval)
  SemitoneArithmetic.semitonesToNoteFromC semitonesFromC

let substractInterval note = function
| Unison -> note
| interval ->
  let semitonesFromC = (SemitoneArithmetic.noteToSemitonesFromC note) - (SemitoneArithmetic.intervalToSemitones interval)
  SemitoneArithmetic.semitonesToNoteFromC semitonesFromC

let intervalFromSemitones semitones =
  SemitoneArithmetic.intervalFromSemitones semitones

let noteName note =
  note
  |> string
  |> (fun n -> n.Replace("Flat", "♭"))
  |> (fun n -> n.Replace("Sharp", "♯"))