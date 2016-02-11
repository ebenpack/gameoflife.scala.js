package example

import scala.scalajs.js
import org.scalajs.dom
import org.scalajs.dom.html

object ScalaJSExample extends js.JSApp {
  def main(): Unit = {

    val g = new GoL("playground")
    val container = dom.document.getElementById("playground")
    val start = dom.document.createElement("button").asInstanceOf[html.Button]
    val slider = dom.document.createElement("input").asInstanceOf[html.Input]
    slider.`type` = "range"
    slider.min = "0"
    slider.max = "1000"
    slider.addEventListener("change", (e: dom.Event) => {
      g.updateSpeed(1000 - e.target.asInstanceOf[html.Input].value.toInt)
    })

    start.textContent = "Start"
    var started = false
    start.addEventListener("click", (e: dom.MouseEvent) => {
      started = g.toggleStart()
      if (started) {
        start.textContent = "Stop"
      } else {
        start.textContent = "Start"
      }
    })
    container.appendChild(start)
    container.appendChild(slider)
  }

  class GoL(containerId: String, canvasWidth: Int, canvasHeight: Int) {
    val container = dom.document.getElementById(containerId).asInstanceOf[html.Div]
    val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    val grid = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
    canvas.width = canvasWidth
    canvas.height = canvasHeight
    grid.width = canvasWidth
    grid.height = canvasHeight
    canvas.style.border = "1px solid black"
    grid.style.border = "1px solid black"
    container.style.position = "relative"
    grid.style.position = "absolute"
    grid.style.left = "0"
    container.appendChild(canvas)
    container.appendChild(grid)
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    val gridCtx = grid.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    var board = new Board(Vector.fill(80 * 40) {
      false
    }, 80, 40)

    var started = false
    var ramId = 0
    var lastUpdate = 0.0
    var updateSpeed = 500

    def updateSpeed(speed: Int) = {
      updateSpeed = speed
    }

    def toggleStart() = {
      started = !started
      if (started) {
        update(dom.window.performance.now())
      } else {
        dom.window.cancelAnimationFrame(ramId)
      }
    }

    def redraw(b: Board, ctx: dom.CanvasRenderingContext2D, mouseX: Int, mouseY: Int) = {
      ctx.clearRect(0, 0, canvas.width, canvas.height)
      board.draw(ctx, canvas.width, canvas.height, mouseX, mouseY)
    }

    grid.addEventListener("click", (e: dom.MouseEvent) => {
      val rect = canvas.getBoundingClientRect()
      val x = (e.clientX - rect.left).toInt
      val y = (e.clientY - rect.top).toInt

      board = board.toggle(x, y, canvas.width, canvas.height)
      redraw(board, ctx, x, y)
    })
    grid.addEventListener("mousemove", (e: dom.MouseEvent) => {
      val rect = canvas.getBoundingClientRect()
      val x = (e.clientX - rect.left).toInt
      val y = (e.clientY - rect.top).toInt
      redraw(board, ctx, x, y)
    })
    grid.addEventListener("mouseout", (e: dom.MouseEvent) => {
      redraw(board, ctx, -999, -999)
    })

    def update(time: Double): Unit = {
      ramId = dom.window.requestAnimationFrame(update _)
      if (time - lastUpdate > updateSpeed) {
        board = board.update()
        lastUpdate = time
        redraw(board, ctx, -999, -999)
      }
    }

    board.drawGrid(gridCtx, grid.width, grid.height)
    redraw(board, ctx, -999, -999)
  }

  class Board(b: Vector[Boolean], w: Int, h: Int) {
    val board = b
    val width = w
    val height = h
    val boardSize = width * height

    def index(x: Int, y: Int): Int = width * y + x

    def point(i: Int): (Int, Int) = {
      val x = i % width
      val y = i / width
      (x, y)
    }

    def toggle(x: Int, y: Int, w: Int, h: Int): Board = {
      val widthStep = w / width
      val heightStep = h / height
      val i = index(x / widthStep, y / heightStep)
      new Board(board.updated(i, !board(i)), width, height)
    }

    def get(x: Int, y: Int): Boolean = {
      board(index(x, y))
    }

    def update(): Board = {
      def wrap(n: Int, bound: Int): Int = n match {
        case p if p < 0 => bound - math.abs(p)
        case p if p >= bound => p % bound
        case _ => n
      }
      val newBoard = (0 until boardSize).map(
        i => {
          val p = point(i)
          val cell = board(i)
          val x = p._1
          val y = p._2
          val xp1 = wrap(p._1 + 1, width)
          val xm1 = wrap(p._1 - 1, width)
          val yp1 = wrap(p._2 + 1, height)
          val ym1 = wrap(p._2 - 1, height)
          val count = {
            List(
              board(index(x, yp1)),
              board(index(xp1, yp1)),
              board(index(xp1, y)),
              board(index(xp1, ym1)),
              board(index(x, ym1)),
              board(index(xm1, ym1)),
              board(index(xm1, y)),
              board(index(xm1, yp1))
            ).count(_ == true)
          }
          if (cell) {
            if (count < 2 || count > 3) false
            else true
          } else {
            if (count == 3) true
            else false
          }
        }
      )
      new Board(newBoard.toVector, width, height)
    }

    def drawGrid(ctx: dom.CanvasRenderingContext2D, w: Int, h: Int) = {
      ctx.beginPath()
      ctx.strokeStyle = "gray"
      val widthStep = w / width
      val heightStep = h / height
      for (
        x <- 0 until width
      ) yield {
        ctx.moveTo(x * widthStep, 0)
        ctx.lineTo(x * widthStep, h)
      }
      for (
        y <- 0 until height
      ) yield {
        ctx.moveTo(0, y * heightStep)
        ctx.lineTo(w, y * heightStep)
      }
      ctx.stroke()
    }

    def draw(ctx: dom.CanvasRenderingContext2D, w: Int, h: Int, mouseX: Int, mouseY: Int) = {
      ctx.fillStyle = "black"
      ctx.beginPath()
      val widthStep = w / width
      val heightStep = h / height
      for (
        i <- 0 until boardSize
      ) yield {
        if (board(i)) {
          val p = point(i)
          val x = p._1 * widthStep
          val y = p._2 * heightStep
          ctx.fillRect(
            x,
            y,
            widthStep,
            heightStep
          )
        }
      }
      val mouseIndex = index(mouseX / widthStep, mouseY / heightStep)
      if (
        mouseX >= 0 &&
          mouseY >= 0 &&
          mouseIndex < boardSize &&
          !board(mouseIndex)
      ) {
        ctx.fillStyle = "gray"
        ctx.fillRect(
          (mouseX / widthStep) * widthStep,
          (mouseY / heightStep) * widthStep,
          widthStep,
          heightStep
        )
      }
      ctx.closePath()
    }
  }

}
