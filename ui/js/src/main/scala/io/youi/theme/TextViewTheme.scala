package io.youi.theme

import io.youi.component.Component
import io.youi.theme.mixin.CoreTextTheme

trait TextViewTheme extends ComponentTheme with CoreTextTheme {
  override protected def defaultThemeParent: Option[Theme] = Some(Component)
}