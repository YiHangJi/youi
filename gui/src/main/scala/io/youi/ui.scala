package io.youi

import io.youi.component.Container
import io.youi.component.support.SizeSupport
import io.youi.component.types.Prop
import io.youi.event.{EventSupport, Swipe}
import io.youi.task.TaskSupport
import org.scalajs.dom._

object ui extends Container(document.body) with SizeSupport with EventSupport with TaskSupport {
  lazy val title: Prop[String] = new Prop(document.title, document.title_=)
  lazy val swipe: Swipe = new Swipe(ui, ui.event, onlyTouch = true)

  measure()
  window.addEventListener("resize", (_: Event) => {
    measure()
  })

  AnimationFrame.delta.attach { d =>
    update(d)
  }

  override def measure(): Unit = {
    super.measure()

    size.width @= window.innerWidth
    size.height @= window.innerHeight
  }
}