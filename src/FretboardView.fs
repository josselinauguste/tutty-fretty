module FretboardView

type Viewport = {
  Width: float;
  Height: float;
}

let neckWidth = 658.
let neckPadding = 50.
let fretSize = 50.
let fretPositions = [|40.; 574.; 1077.; 1552.; 2001.; 2424.; 2824.; 3201.; 3557.; 3893.; 4210.; 4509.; 4889.|]
let numberOfStrings = 6

let scaleFretboard availableWidth =
  let backgroundRatio = 4839. / neckWidth
  let height = availableWidth / backgroundRatio
  { Width = availableWidth; Height = height; }

let projectPosition (viewport: Viewport) (position: Fretboard.Position) =
  let radius = (neckWidth - 2. * neckPadding) / ((numberOfStrings - 1) |> float) / 2.
  let markerPosition (position: Fretboard.Position) =
    let x =
      match position.Cell with
      | Fretboard.Cell 0 -> fretPositions.[0] / 2.
      | Fretboard.Cell cell -> fretPositions.[cell - 1] + (fretPositions.[cell] - fretSize - fretPositions.[cell - 1]) / 2.
    let y =
      match position.String with
      | Fretboard.String string -> neckPadding + (string - 1 |> float) * ((neckWidth - 2. * neckPadding) / (numberOfStrings - 1 |> float))
    (x, y, radius)
  let scale (x, y, r) =
    let scaleRatio = viewport.Height / neckWidth
    (x * scaleRatio, y * scaleRatio, r * scaleRatio)
  position |> markerPosition |> scale
