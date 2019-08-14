package io.youi.gui

import org.scalajs.dom.html

trait TypedHTMLComponent[E <: html.Element] extends HTMLComponent {
  protected def element: E
}
