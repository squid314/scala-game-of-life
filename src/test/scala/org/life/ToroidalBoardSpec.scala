package org.life

import org.life.MatrixBoardFactory.toroid
import org.scalatest._

class ToroidalBoardSpec extends FlatSpec with Matchers {
    "Board" should "maintain an empty board" in {
        assert( emptyBoard.nextBoard( ) == emptyBoard )
    }

    it should "maintain box layouts in the center and the corners" in {
        assert( squareCenter.nextBoard( ) == squareCenter )
        assert( squareCorner.nextBoard( ) == squareCorner )
    }

    it should "flip-flop between blinkers" in {
        assert( blinkerHorizontal.nextBoard( ) == blinkerVertical )
        assert( blinkerVertical.nextBoard( ) == blinkerHorizontal )
    }

    it should "maintain \"infinite\" diagonal" in {
        assert( diagonal.nextBoard( ) == diagonal )
    }

    val emptyBoard = toroid( 4, 4 )( )
    val squareCenter = toroid( 4, 4 )( (1, 1), (1, 2), (2, 1), (2, 2) )
    val squareCorner = toroid( 4, 4 )( (0, 0), (0, 3), (3, 0), (3, 3) )
    val blinkerHorizontal = toroid( 4, 4 )( (1, 0), (1, 1), (1, 2) )
    val blinkerVertical = toroid( 4, 4 )( (0, 1), (1, 1), (2, 1) )
    val diagonal = toroid( 4, 4 )( (0, 3), (1, 2), (2, 1), (3, 0) )
}
