module Fretboard

type Position = {
  String: String;
  Cell: Cell
}
and String = String of int
and Cell = Cell of int