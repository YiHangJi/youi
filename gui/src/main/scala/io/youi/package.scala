package io

import io.youi.component.util.CanvasPool
import io.youi.font.{GoogleFont, GoogleFontWeight}
import io.youi.paint.Paint
import org.scalajs.dom.{CanvasRenderingContext2D, document, html, window}
import reactify.{Val, Var}
import typekit.{GoogleConfig, WebFont, WebFontConfiguration}

import scala.concurrent.{Future, Promise}
import scala.language.implicitConversions
import scala.scalajs.js

package object youi {
  lazy val isIOS: Boolean = Set("iPad Simulator", "iPhone Simulator", "iPod Simulator", "iPad", "iPhone", "iPod")
    .contains(window.navigator.platform)

  def devicePixelRatio: Double = window.devicePixelRatio
  def backingStoreRatio: Double = CanvasPool.withCanvas(1.0, 1.0) { canvas =>
    def opt(d: js.Dynamic): Option[Double] = d.asInstanceOf[js.UndefOr[Double]].toOption
    val ctx = canvas.context.asInstanceOf[js.Dynamic]
    val webkit = opt(ctx.webkitBackingStorePixelRatio)
    val moz = opt(ctx.mozBackingStorePixelRatio)
    val ms = opt(ctx.msBackingStorePixelRatio)
    val o = opt(ctx.oBackingStorePixelRatio)
    val default = opt(ctx.backingStorePixelRatio)
    webkit.orElse(moz).orElse(ms).orElse(o).orElse(default).getOrElse(1.0)
  }
  def ratio: Double = devicePixelRatio / backingStoreRatio
  lazy val ppi: Double = {
    val div = dom.create[html.Div]("div")
    div.style.width = "1in"
    document.body.appendChild(div)
    try {
      div.offsetWidth
    } finally {
      document.body.removeChild(div)
    }
  }
  lazy val isMobileDevice: Boolean = {
    val ua = window.navigator.userAgent.toLowerCase
    ua.contains("iphone") || ua.contains("ipad") || ua.contains("android")
  }

  implicit def color2Paint(color: Color): Paint = Paint.color(color)

  implicit class ExtendedCanvas(canvas: html.Canvas) {
    def context: CanvasRenderingContext2D = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  }

  implicit class ExtendedGoogleFont(font: GoogleFont) {
    def load(): Future[GoogleFont] = {
      val promise = Promise[GoogleFont]
      val f: js.Function0[Unit] = () => {
        promise.success(font)
        ()
      }
      WebFont.load(new WebFontConfiguration {
        google = new GoogleConfig {
          families = js.Array(font.family)
        }
        active = f
      })
      promise.future
    }
  }

  implicit class ExtendedGoogleFontWeight(weight: GoogleFontWeight) {
    def load(): Future[GoogleFontWeight] = {
      val promise = Promise[GoogleFontWeight]
      val f: js.Function0[Unit] = () => {
        promise.success(weight)
        ()
      }
      WebFont.load(new WebFontConfiguration {
        google = new GoogleConfig {
          families = js.Array(s"${weight.font.family}:${weight.name}")
        }
        active = f
      })
      promise.future
    }
  }

  implicit class UINumericSize[T](t: T)(implicit n: Numeric[T]) {
    private val d = n.toDouble(t)

    /**
      * pixels
      */
    def px: Double = d

    /**
      * degrees conversion (360 converts to 1.0)
      */
    def degrees: Double = d / 360.0

    /**
      * radians conversion (2π converts to 1.0)
      */
    def radians: Double = d / (2.0 * math.Pi)

    /**
      * Returns percentage value `of`.
      */
    def percentOf(of: Val[Double]): Val[Double] = Val(of.get * (d * 0.01))

    /**
      * millimeters
      */
    def mm: Double = in * 25.4

    /**
      * quarter millimeters
      */
    def q: Double = mm * 0.25

    /**
      * centimeters
      */
    def cm: Double = mm * 10.0

    /**
      * inches
      */
    def in: Double = d * ppi

    /**
      * points
      */
    def pt: Double = in / 72.0

    /**
      * picas
      */
    def pica: Double = pt * 12.0

    /**
      * 1/100th of the width of the viewport.
      */
    def vw: Val[Double] = Val((d / 100.0) * ui.size.width)

    /**
      * 1/100th of the height of the viewport.
      */
    def vh: Val[Double] = Val((d / 100.0) * ui.size.height)

    /**
      * 1/100th of the minimum value between the height and the width of the viewport.
      */
    def vmin: Val[Double] = Val(math.min((d / 100.0) * ui.size.width, (d / 100.0) * ui.size.height))

    /**
      * 1/100th of the maximum value between the height and the width of the viewport.
      */
    def vmax: Val[Double] = Val(math.max((d / 100.0) * ui.size.width, (d / 100.0) * ui.size.height))

    /**
      * Returns percentage value `of`.
      */
    def %(of: Val[Double]): Val[Double] = percentOf(of)
  }
}