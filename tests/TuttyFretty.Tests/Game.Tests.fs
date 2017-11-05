module Game.Tests

open Expecto
open FsCheck

module Gen =
  type Position = Position of Fretboard.Position
  let positionArb =
    Arb.generate<int32>
    |> Gen.two
    |> Gen.where (fun (s, c) -> s >= 1 && s <= 6 && c >= 0 && c <= 24)
    |> Gen.map (fun (s, c) -> { Fretboard.String = Fretboard.String s; Fretboard.Cell = Fretboard.Cell c })
    |> Arb.fromGen

  let addToConfig config =
    { config with arbitrary = typeof<Position>.DeclaringType::config.arbitrary }

let config = Gen.addToConfig FsCheckConfig.defaultConfig

[<Tests>]
let tests =
  let fixtureState =
    {
      CurrentNote = {
                      Fretboard.String = Fretboard.String 1;
                      Fretboard.Cell = Fretboard.Cell 0
                    };
      NotePropositions = []
    }

  testList "game" [
    testList "generate notes propositions from state" [

      testCase "target note is in the propositions" <| fun _ ->
        let generator _ = 4

        let propositions = proposeSolutions generator fixtureState.CurrentNote

        Expect.contains propositions Febus.E ""

      testPropertyWithConfig config "propositions contains 5 different notes" (fun (Gen.Position position) ->
        let state =
          {
            CurrentNote = position;
            NotePropositions = []
          }

        let propositions = proposeSolutions (IO.random 0) state.CurrentNote

        (propositions |> List.length) = 5 && (propositions |> Seq.contains (position |> Fretboard.positionToNote))
      )
    ]

    testList "verify solution" [

      testCase "right solution returns Ok" <| fun _ ->
        let solution = verifySolution fixtureState Febus.E

        Expect.isOk solution ""

      testCase "wrong solution returns Error" <| fun _ ->
        let solution = verifySolution fixtureState Febus.G

        Expect.isError solution ""
    ]
  ]