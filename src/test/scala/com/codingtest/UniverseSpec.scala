package com.codingtest

import collection.mutable.Stack
import org.scalatest._

class UniverseSpec extends FlatSpec with Matchers {

  "Universe" should "do nothing in dead universe" in {
    val universe = Universe(Array.ofDim[Boolean](2, 2))
    universe.tick()(1, 1) shouldBe false
  }

  "Universe" should "kill any living cell with fewer than two live neighbours" in {
    val grid = Array.ofDim[Boolean](2, 2)
    grid(1)(1) = true
    val universe = Universe(grid)

    universe.tick()(1, 1) shouldBe false
  }

  "Universe" should "not kill any living cell with more than two live neighbours" in {
    val grid = Array.ofDim[Boolean](3, 3)
    grid(1)(1) = true
    grid(0)(0) = true
    grid(0)(1) = true
    grid(0)(2) = true
    val universe = Universe(grid)

    universe.tick()(1, 1) shouldBe true
  }

  "Universe" should "reanimate any dead cell with three live neighbours" in {
    val grid = Array.ofDim[Boolean](3, 3)
    grid(0)(0) = true
    grid(0)(1) = true
    grid(0)(2) = true
    val universe = Universe(grid)

    universe.tick()(1, 1) shouldBe true
  }

}
