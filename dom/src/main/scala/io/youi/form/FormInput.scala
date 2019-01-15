package io.youi.form

import org.scalajs.dom.{Event, html}

class FormInput(formSupport: FormSupport, val element: html.Element) {
  val error: FieldError = formSupport.createFieldError(this)

  element.addEventListener("blur", (_: Event) => {
    validate(ValidationMode.Blur)
  })
  element.addEventListener("change", (_: Event) => {
    validate(ValidationMode.ValueChange)
  })

  def name: String = element match {
    case i: html.Input => i.name
    case i: html.TextArea => i.name
    case i: html.Select => i.name
    case _ => throw new RuntimeException(s"Unsupported getting name in FormInput for: ${element.innerHTML}")
  }

  def focus(): Unit = element.focus()

  def option: Option[String] = value match {
    case s if s != null && s.trim.nonEmpty => Some(s)
    case _ => None
  }

  def value: String = element match {
    case i: html.Input => i.value
    case i: html.TextArea => i.value
    case i: html.Select => i.value
    case _ => throw new RuntimeException(s"Unsupported getting value to FormInput for: ${element.innerHTML}")
  }

  def value_=(v: String): Unit = element match {
    case i: html.Input => i.value = v
    case i: html.TextArea => i.value = v
    case i: html.Select => i.value = v
    case _ => throw new RuntimeException(s"Unsupported setting value to FormInput for: ${element.innerHTML}")
  }

  def checked: Boolean = element match {
    case i: html.Input => i.checked
    case _ => throw new RuntimeException(s"Unsupported getting checked from FormInput for: ${element.innerHTML}")
  }

  def checked_=(b: Boolean): Unit = element match {
    case i: html.Input => i.checked = b
    case _ => throw new RuntimeException(s"Unsupported setting checked to FormInput for: ${element.innerHTML}")
  }

  def text: String = option.getOrElse(throw new RuntimeException(s"Value is empty for ${element.id}"))

  def show(): Unit = element.style.display = "inline"
  def hide(): Unit = element.style.display = "none"

  def clear(): Unit = value = ""

  object validation {
    private var list = List.empty[FormValidation]

    def apply(validation: Validation, modes: ValidationMode*): FormInput = {
      val m = if (modes.isEmpty) ValidationMode.all else modes.toSet
      list = list ::: List(FormValidation(validation, m))
      FormInput.this
    }

    def all(): List[FormValidation] = list
  }

  def validate(mode: ValidationMode): Boolean = {
    val results = validation.all().flatMap(_.validate(mode, this).asOption)
    error.clear()
    val message = results.mkString("<br/>")
    if (results.nonEmpty) {
      error.show(message)
    }
    results.isEmpty
  }
}