package org.hyperscala.svg

import attributes.XMLSpace
import org.hyperscala.{Unique, PropertyAttribute, Tag}
import org.hyperscala.io.HTMLWriter

/**
 * @author Matt Hicks <mhicks@outr.com>
 */
trait SVGTag extends Tag {
  val id = PropertyAttribute[String]("id", null)
  val xmlBase = PropertyAttribute[String]("xml:base", null)
  val xmlLang = PropertyAttribute[String]("xml:lang", null)
  val xmlSpace = PropertyAttribute[XMLSpace]("xml:space", null)

  /**
   * Gets the id value and sets it to a unique value if it's null.
   */
  def identity = id() match {
    case null => {
      val uuid = Unique()
      id := uuid
      uuid
    }
    case value => value
  }

  def byId[T <: SVGTag](id: String)(implicit manifest: Manifest[T]) = hierarchy.findFirst[T](t => t.id() == id)(manifest)

  def byTag[T <: SVGTag](implicit manifest: Manifest[T]) = hierarchy.findAll[T](t => true)(manifest)

  def outputString = {
    val b = new StringBuilder
    val writer: String => Unit = (s: String) => b.append(s)
    val htmlWriter = HTMLWriter(writer)
    write(htmlWriter)
    b.toString()
  }
}