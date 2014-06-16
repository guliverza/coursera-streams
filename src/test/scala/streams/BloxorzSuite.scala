package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(4,11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1,1))
      assert(goal === Pos(4,7))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      println(solution)
      val solve: Block = solve(solution)
      assert(solve === Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }

  test("All possible moves from start position") {
    new Level1 {
      val neighbors: Stream[(Block, List[Move])] = newNeighborsOnly(Stream((startBlock, List())), Set(startBlock))

      println(neighbors.toList)
      assert(Stream((Block(Pos(1,2),Pos(1,3)),List(Right)), (Block(Pos(2,1),Pos(3,1)),List(Down))) === neighbors)
      val next: Stream[(Block, List[Move])] = newNeighborsOnly(neighbors, Set(startBlock) ++ neighbors.map(_._1))
      println(next.toList)
      val next2: Stream[(Block, List[Move])] = newNeighborsOnly(next, Set(startBlock) ++ next.map(_._1) ++ neighbors.map(_._1))
      println(next2.toList)
    }
  }

  test("pathsFromStart") {
    new Level1 {
      println(pathsFromStart.mkString("\n"))
      assert(Stream(1) === Stream(1))
    }
  }

  test("pathsToGoal") {
    new Level1 {
      println("goal=" + goal)
      println(pathsToGoal.take(5).toList.mkString("\n"))
      assert(Stream(1) === Stream(1))
    }
  }

  test("check done") {
    new Level1 {
      assert(done(Block(Pos(4,7), Pos(4,7))))
      assert(!done(Block(Pos(4,7), Pos(5,7))))
      assert(!done(Block(Pos(1,1), Pos(1,1))))
      assert(!done(Block(Pos(0,0), Pos(0,0))))
    }
  }
}
