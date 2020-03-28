package io.youi.component

import io.youi.dom
import io.youi.component.support.FontSupport
import io.youi.component.types.Prop
import org.scalajs.dom.html

class TextInput(element: html.Input = dom.create.input,
                password: Boolean = false) extends Component(element) with FontSupport {
  lazy val value: Prop[String] = new Prop[String](element.value, element.value_=)
  lazy val placeholder: Prop[String] = new Prop[String](element.placeholder, element.placeholder_=)

  if (password) element.`type` = "password"
}