package org.hyperscala.ui.dynamic

import org.hyperscala.html.HTMLTag
import org.hyperscala.Unique

/**
 * @author Matt Hicks <matt@outr.com>
 */
trait DynamicTagged[T <: HTMLTag] extends HTMLTag {
  this: T =>

  def dynamicTag: DynamicTag[T]

  dynamicTag(this)

  def reId[T <: HTMLTag](id: String, newId: String = Unique())(implicit manifest: Manifest[T]) = {
    val t = getById[T](id)
    t.id := newId
    t
  }
}
