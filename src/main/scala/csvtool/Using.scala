package csvtool

import scala.util.Try

object Using {

  def apply[A <:{ def close():Any},B](hasClose:A)(func:A=>B):B={
    try func(hasClose) finally Try( hasClose.close() )
  }
}
