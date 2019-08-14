package io.youi.gui

import io.youi.Unique
import reactify.Var

trait Component {
  /**
    * Generated unique identifier for this element.
    */
  lazy val id: Var[String] = Var(s"$componentType-${Unique(length = 4, characters = Unique.Readable).toLowerCase}")

  /**
    * Parent to this element.
    */
  lazy val parent: Var[Option[Component]] = Var(None)

  /**
    * Root parent
    */
  lazy val root: Var[Option[Component]] = Var(parent().flatMap(_.root()))

  /**
    * The type of Component. This is useful for client-side introspection and logging. Each custom Component instance
    * should represent a unique `type`.
    */
  def componentType: String

  def hasParent(parent: Component): Boolean = this.parent() match {
    case Some(p) => if (parent == p) {
      true
    } else {
      p.hasParent(parent)
    }
    case None => false
  }

  protected def childComponents: Vector[Component] = Vector.empty

  object sibling {
    def previous(): Option[Component] = parent().flatMap { p =>
      val children = p.childComponents
      val index = children.indexOf(Component.this)
      if (index > 0) {
        Some(children(index - 1))
      } else {
        None
      }
    }

    def next(): Option[Component] = parent().flatMap { p =>
      val children = p.childComponents
      val index = children.indexOf(Component.this)
      if (index < children.size - 1) {
        Some(children(index + 1))
      } else {
        None
      }
    }
  }

  override def toString: String = id()
}