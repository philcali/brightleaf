package brightleaf
package test

import io.Source.{ fromURL => open }
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class BrightLeafParserTest extends FlatSpec with Matchers {
  val parser = new BrightLeafParser {}

  def source(name: String) = open(getClass.getResource(s"/${name}.brs"))

  "BrightLeafParser" should "parse the dim statement" in {
    println(parser(source("dim")))
  }

  it should "parse the for-to statement" in {
    println(parser(source("for-to")))
  }

  it should "parse the for-each statement" in {
    println(parser(source("for-each")))
  }

  it should "parse the single-line if expression" in {
    println(parser(source("if-single")))
  }

  it should "parse the multi-line if expression" in {
    println(parser(source("if-multi")))
  }
}
