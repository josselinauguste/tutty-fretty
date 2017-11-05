module Febus.Tests

open Expecto
open FsCheck

module Gen =
  type NonUnisonSemitones = NonUnisonSemitones of int32
  let nonUnisonSemitonesArb =
    Arb.generate<int32>
    |> Gen.where (fun v -> v % 12 > 0)
    |> Gen.map NonUnisonSemitones
    |> Arb.fromGen

  type NonUnisonInterval = NonUnisonInterval of Febus.Interval
  let NonUnisonIntervalArb =
    Arb.generate<Febus.Interval>
    |> Gen.where (fun v -> v <> Unison && v <> DiminishedSecond)
    |> Gen.map NonUnisonInterval
    |> Arb.fromGen

  let addToConfig config =
    { config with arbitrary = typeof<NonUnisonInterval>.DeclaringType::typeof<NonUnisonSemitones>.DeclaringType::config.arbitrary }

let config = Gen.addToConfig FsCheckConfig.defaultConfig

[<Tests>]
let tests =
  testList "febus" [
    testList "add an interval to a note" [
      testCase "adding a unison returns the note" <| fun _ ->
        Expect.equal (addInterval Febus.C Unison) Febus.C ""

      testCase "adding a minor second" <| fun _ ->
        Expect.equal (addInterval Febus.C MinorSecond) CSharp ""

      testPropertyWithConfig config "a non unison interval added to a note does not return the note" (fun note (Gen.NonUnisonInterval interval) ->
        (addInterval note interval) <> note
      )

      testProperty "a unison interval added to a note returns the note" <| fun note ->
        (addInterval note Unison) = note
    ]

    testList "substract an interval to a note" [
      testCase "substracting a unison returns the note" <| fun _ ->
        Expect.equal (substractInterval Febus.C Unison) Febus.C ""

      testCase "substracting a minor second" <| fun _ ->
        Expect.equal (substractInterval Febus.C MinorSecond) B ""

      testPropertyWithConfig config "a non unison interval substracted to a note does not return the note" (fun note (Gen.NonUnisonInterval interval) ->
        (substractInterval note interval) <> note
      )

      testProperty "a unison interval added to a note returns the note" <| fun note ->
        (addInterval note Unison) = note
    ]

    testList "interval from semitones" [
      testCase "get a unison" <| fun _ ->
        Expect.equal (intervalFromSemitones 0) Unison ""

      testCase "get a minor second" <| fun _ ->
        Expect.equal (intervalFromSemitones 1) MinorSecond ""

      testPropertyWithConfig config "a number of semitones non multiple of 12 is not a semitone" (fun (Gen.NonUnisonSemitones semitones) ->
        (intervalFromSemitones semitones) <> Unison
      )
    ]
  ]