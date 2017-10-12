module GameRenderer

open System
open Fable.Core.JsInterop
open Fable.Import

let drawMarker (ctx: Browser.CanvasRenderingContext2D) (x, y, radius) =
  ctx.beginPath()
  ctx.fillStyle <- !^"#A2A2A2"
  ctx.arc(x, y, radius, 0., Math.PI * 2., true)
  ctx.shadowColor <- "#999"
  ctx.shadowBlur <- 8.
  ctx.shadowOffsetX <- 0.
  ctx.shadowOffsetY <- 0.
  ctx.fill()
  ctx.beginPath()
  ctx.strokeStyle <- !^"#D3D3D2"
  ctx.arc(x, y, radius, 0., Math.PI * 2., true)
  ctx.stroke()
  ctx.fillStyle <- !^"#443404"
  ctx.font <- sprintf "%ipx sans-serif" ((radius * 1.5) |> int)
  ctx.textAlign <- "center"
  ctx.textBaseline <- "middle"
  ctx.fillText("?", x, y)

let init (state: Game.GameState) =
  let rec render ctx _timestamp =
    let viewport = FretboardView.scaleFretboard Browser.window.innerWidth
    let drawMarkerinContext = ctx |> drawMarker
    state.CurrentNote
    |> FretboardView.projectPosition viewport
    |> drawMarkerinContext
  render