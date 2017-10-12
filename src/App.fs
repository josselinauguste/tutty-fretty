module TuttyFretty

open System
open Fable.Core
open Fable.Import

module Renderer =
  let init (container: Browser.HTMLElement) initialViewport gameRenderer =
    let canvas = container.getElementsByTagName_canvas().[0]
    let rescale viewport =
      let width, height = viewport
      container.style.width <- sprintf "%ipx" (width |> int)
      canvas.width <- width
      container.style.height <- sprintf "%ipx" (height |> int)
      canvas.height <- height
    let ctx = canvas.getContext_2d()
    let rec redraw timestamp =
      ctx.clearRect(0., 0., canvas.width, canvas.height)
      gameRenderer ctx timestamp
      Browser.window.requestAnimationFrame (Browser.FrameRequestCallback redraw) |> ignore
    rescale initialViewport
    container.style.display <- "block"
    redraw 0.
    rescale

let viewport availableWidth =
  let layout = FretboardView.scaleFretboard availableWidth
  (layout.Width, layout.Height)

let mutable state = Game.init
let rescaler = Renderer.init (Browser.document.getElementById("fretboard")) (viewport Browser.window.innerWidth) (state |> GameRenderer.init)
Browser.window.addEventListener_resize (fun (_) -> rescaler (viewport Browser.window.innerWidth); null)