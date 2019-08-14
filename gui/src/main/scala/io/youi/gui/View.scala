package io.youi.gui

import org.scalajs.dom._
import org.scalajs.dom.raw.HTMLElement

object View extends TypedHTMLComponent[HTMLElement] {
  override protected def element: HTMLElement = document.body

  override def componentType: String = "View"
}