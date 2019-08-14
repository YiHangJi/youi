package io.youi.gui

import org.scalajs.dom.html

trait HTMLComponent extends Component {
  protected def element: html.Element
}
