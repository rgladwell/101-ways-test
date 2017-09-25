package com.codingtest

case class Universe(grid: Array[Array[Boolean]]) {

  def tick(): Universe = {
    val newGrid = 
      (for {
        row <- 0 to (grid.size - 1)
      } yield {
        (for {
          column <- 0 to (grid(0).size - 1)
        } yield rules(row, column)
        ).toArray
      }).toArray

    Universe(newGrid)
  }

  private def rules(row: Int, column: Int): Boolean = {
    println(s"($row,$column) = ${grid(row)(column)}")
    println(s"neighbours($row,$column) = ${neighbours(row, column)}")
    if(neighbours(row, column) < 2) false
    else if(!grid(row)(column) && neighbours(row, column) == 3) {println("rule 4"); true}
    else grid(row)(column)
  }

  private def neighbours(x: Int, y: Int): Int = {
    val neighbours = Seq(
      (-1, -1),
      (-1, 0),
      (-1, 1),
      (0,  -1),
      (0,  1),
      (1, -1),
      (1, 0),
      (1, 1)
    )

    val count = 
      for {
        (modx, mody) <- neighbours
      } yield {

        val newx = x + modx
        val newy = y + mody

        if(newx < grid.size && newx >= 0 && newy < grid(0).size && newy > 0 && grid(newx)(newy)) 1
        else 0

      }

    count.sum
  }

  def apply(x: Int, y: Int): Boolean = grid(x)(y)

}
