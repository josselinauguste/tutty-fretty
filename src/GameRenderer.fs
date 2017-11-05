module GameRenderer

open System
open Fable.Core.JsInterop
open Fable.Import

let private drawMarker (ctx: Browser.CanvasRenderingContext2D) (x, y, radius) =
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

let private draw ctx viewport (state: Game.GameState) =
  state.CurrentNote
    |> FretboardView.projectPosition viewport
    |> drawMarker ctx

let init (container: Browser.HTMLElement) (initialState: Game.GameState) =
  let mutable state = initialState
  let mutable viewport = FretboardView.scaleFretboard Browser.window.innerWidth
  let canvas = container.getElementsByTagName_canvas().[0]
  let ctx = canvas.getContext_2d()
  let rescale availableWidth =
    viewport <- FretboardView.scaleFretboard availableWidth
    container.style.width <- sprintf "%ipx" (viewport.Width |> int)
    canvas.width <- viewport.Width
    container.style.height <- sprintf "%ipx" (viewport.Height |> int)
    canvas.height <- viewport.Height
  let rec redraw _timestamp =
    ctx.clearRect(0., 0., canvas.width, canvas.height)
    state |> draw ctx viewport
    Browser.window.requestAnimationFrame (Browser.FrameRequestCallback redraw) |> ignore
  Browser.window.addEventListener_resize (fun (_) -> rescale (Browser.window.innerWidth); null)
  rescale (Browser.window.innerWidth)
  redraw 0.
  container.style.display <- "block"
  (fun s -> state <- s; s)
