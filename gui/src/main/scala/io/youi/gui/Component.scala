package io.youi.gui

import java.util.concurrent.atomic.AtomicBoolean

import io.youi.Unique
import reactify.reaction.Reaction
import reactify.{Channel, Var}

import scala.concurrent.Future
import scala.concurrent.duration._

trait Component {
  private var _initialized: Boolean = false

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
    * True if this Component's `init` method has been invoked.
    */
  def initialized: Boolean = _initialized

  /**
    * The type of Component. This is useful for client-side introspection and logging. Each custom Component instance
    * should represent a unique `type`.
    */
  def componentType: String

  protected def init(): Future[Unit] = Future.successful(())

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

  override def update(delta: Double): Unit = {
    // Make sure we initialize before we do anything else
    if (!initialized) {
      init().foreach(_ => _initialized = true)
    }

    super.update(delta)

    updateUpdatables(delta, internalUpdatables())
  }

  override def toString: String = id()
}

trait Updatable {
  def update(delta: Double): Unit
}

trait Updates extends Updatable {
  lazy val delta: Channel[Double] = Channel[Double]

  def nextFrame(f: => Unit): Unit = delta.once(_ => f)

  override def update(delta: Double): Unit = this.delta := delta

  def once(delay: FiniteDuration)(f: => Unit): Unit = every(delay, Some(delay))(f)

  def every(delay: FiniteDuration,
            until: Option[FiniteDuration] = None)
           (f: => Unit): Unit = {
    val timeout = delay.toMillis / 1000.0
    val untilTimeout = until.map(_.toMillis / 1000.0)
    var elapsed = 0.0
    var totalElapsed = 0.0

    var reaction: Reaction[Double] = null
    reaction = delta.attach { d =>
      elapsed += d
      totalElapsed += d
      if (elapsed >= timeout) {
        elapsed = 0.0
        f

        untilTimeout.foreach { total =>
          if (totalElapsed >= total) {
            delta.reactions += reaction
          }
        }
      }
    }
  }

  def rateLimited(maxFrequency: FiniteDuration, frequency: FiniteDuration = 0.millis)(f: => Unit): LazyUpdate = {
    val lu = LazyUpdate(f, maxFrequency)
    every(frequency)(lu.update())
    lu
  }
}

class LazyUpdate(f: () => Unit, maxFrequency: FiniteDuration) {
  private var lastUpdate: Long = 0L
  private val dirty = new AtomicBoolean(false)

  def flag(): Unit = dirty.set(true)
  def isFlagged: Boolean = dirty.get()
  def isReady: Boolean = System.currentTimeMillis() - lastUpdate >= maxFrequency.toMillis

  def update(force: Boolean = false): Unit = if ((isReady && dirty.compareAndSet(true, false)) || force) {
    try {
      f()
    } finally {
      lastUpdate = System.currentTimeMillis()
    }
  }
}

object LazyUpdate {
  def apply(f: => Unit, maxFrequency: FiniteDuration = 0.millis): LazyUpdate = new LazyUpdate(() => f, maxFrequency)
}