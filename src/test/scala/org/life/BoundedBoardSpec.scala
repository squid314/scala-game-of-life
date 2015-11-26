package org.life

import org.life.MatrixBoardFactory.bounded
import org.scalatest._

class BoundedBoardSpec extends FlatSpec with Matchers {
    "Any board" should "be able to find a specific cell" in {
        assert( center( 0, 0 ) == false )
        assert( center( 0, 1 ) == false )
        assert( center( 0, 2 ) == false )
        assert( center( 1, 0 ) == false )
        assert( center( 1, 1 ) == true )
        assert( center( 1, 2 ) == false )
        assert( center( 2, 0 ) == false )
        assert( center( 2, 1 ) == false )
        assert( center( 2, 2 ) == false )
    }

    it should "recognize starvation rule" in {
        assert( center.nextBoard( )( 1, 1 ) == false )
    }

    it should "recognize live survival rule for 2 neighbors" in {
        assert( blinkerHorizontal.nextBoard( )( 1, 1 ) == true )
    }

    it should "recognize live survival rule for 3 neighbors" in {
        assert( box.nextBoard( )( 1, 1 ) == true )
    }

    it should "recognize dead survival rule for 2 neighbors" in {
        assert( blinkerHorizontal.nextBoard( )( 0, 0 ) == false )
    }

    it should "recognize overpopulation rule" in {
        assert( full.nextBoard( )( 1, 1 ) == false )
    }

    it should "recognize spawning rule" in {
        assert( blinkerHorizontal.nextBoard( )( 0, 1 ) == true )
        assert( corner3.nextBoard( )( 1, 1 ) == true )
    }

    "BoundedBoard" should "be able to find the neighbors of the top left corner in the center layout" in {
        assert( center.neighbors( 0, 0 ).length == 3 )
        assert( center.neighbors( 0, 0 ).count( alive => alive ) == 1 )
        assert( center.neighbors( 0, 0 ).count( alive => !alive ) == 2 )
    }

    it should "be able to find the neighbors of the middle left cell of the center layout" in {
        assert( center.neighbors( 1, 0 ).length == 5 )
        assert( center.neighbors( 1, 0 ).count( alive => alive ) == 1 )
        assert( center.neighbors( 1, 0 ).count( alive => !alive ) == 4 )
    }

    it should "be able to find the neighbors of the middle center cell of the center layout" in {
        assert( center.neighbors( 1, 1 ).length == 8 )
        assert( center.neighbors( 1, 1 ).count( alive => alive ) == 0 )
        assert( center.neighbors( 1, 1 ).count( alive => !alive ) == 8 )
    }

    it should "be able to find the neighbors of the top left cell in the blinker layout" in {
        assert( blinkerHorizontal.neighbors( 0, 0 ).length == 3 )
        assert( blinkerHorizontal.neighbors( 0, 0 ).count( alive => alive ) == 2 )
        assert( blinkerHorizontal.neighbors( 0, 0 ).count( alive => !alive ) == 1 )
    }

    "Board" should "be able to find the next board of center layout" in {
        assert( center.nextBoard( ) == emptyBoard )
    }

    "Board" should "be able to find the next board of 3 corner layout" in {
        assert( corner3.nextBoard( ) == center )
    }

    "Board" should "be able to find the next board of box layout" in {
        assert( box.nextBoard( ) == box )
    }

    "Board" should "be able to find the next board of the horizontal blinker layout" in {
        assert( blinkerHorizontal.nextBoard() == blinkerVertical )
    }

    val emptyBoard = bounded( 3, 3 )( )
    val center = bounded( 3, 3 )( (1, 1) )
    val corner3 = bounded( 3, 3 )( (0, 0), (0, 2), (2, 0) )
    val blinkerHorizontal = bounded( 3, 3 )( (1, 0), (1, 1), (1, 2) )
    val blinkerVertical = bounded( 3, 3 )( (0, 1), (1, 1), (2, 1) )
    val box = bounded( 3, 3 )( (1, 1), (1, 2), (2, 1), (2, 2) )
    val full = bounded( 3, 3 )( (0, 0), (0, 1), (0, 2), (1, 0), (1, 1), (1, 2), (2, 0), (2, 1), (2, 2) )
}
