package interview

import org.scalatest._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class UnitSpec
    extends WordSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
