module Fretboard.Tests

open Expecto

[<Tests>]
let tests =
  testList "get note at position" [
    testCase "for open E position" <| fun _ ->
      let position = {
        String = String 1;
        Cell = Cell 0
      }
      Expect.equal (positionToNote position) Febus.E ""

    testCase "for intermediate position" <| fun _ ->
      let position = {
        String = String 4;
        Cell = Cell 5
      }
      Expect.equal (positionToNote position) Febus.G ""
  ]