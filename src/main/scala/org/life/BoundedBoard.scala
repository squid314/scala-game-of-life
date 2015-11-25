package org.life

import scala.collection.immutable.List

abstract class AbstractMatrixBackedBoard {
    val board: List[ List[ Boolean ] ]

    def apply( i: Int, j: Int ) = board( i )( j )

    def neighbors( i: Int, j: Int ): List[ Boolean ]

    def liveNeighborCount( i: Int, j: Int ) = {
        neighbors( i, j ).count( alive => alive )
    }

    def cellNextState( i: Int, j: Int, alive: Boolean ) = {
        val aliveNeighbors = this.liveNeighborCount( i, j )
        if ( aliveNeighbors < 2 ) false    // starvation death
        else if ( aliveNeighbors > 3 ) false // overpopulation death
        else if ( aliveNeighbors == 3 ) true // spawning
        else alive                         // survival
    }

    def nextBoard( ): BoundedBoard = {
        new BoundedBoard( board = pairWithIndexes( board )
                .map( row => row
                        .map( cell => cellNextState( cell._1, cell._2, cell._3 ) )
                        .toList )
                .toList )
    }

    def pairWithIndexes( board: List[ List[ Boolean ] ] ) = {
        board.indices.map( i => board( i ).indices.map( j => (i, j, board( i )( j )) ) )
    }

    override def toString = {
        board.flatMap( row => row.map( cell => if ( cell ) "X " else ". " ) :+ "\n" ).mkString
    }
}

/** Represents a Game of Life board which is bounded by a destructive border. */
case class BoundedBoard( board: List[ List[ Boolean ] ] ) extends AbstractMatrixBackedBoard {
    override def neighbors( i: Int, j: Int ) = {
        ( i - 1 to i + 1 ).filter( board.isDefinedAt )
                .flatMap( ii => ( j - 1 to j + 1 ).filter( board( ii ).isDefinedAt )
                        .map( jj => (ii, jj) ) )
                .filter( pair => pair._1 != i || pair._2 != j )
                .map( pair => board( pair._1 )( pair._2 ) )
                .toList
    }
}

object BoundedBoardFactory {
    def apply( height: Int, width: Int )( positions: (Int, Int)* ) = {
        val newBoard = Array.ofDim[ Boolean ]( height, width )
        positions foreach ( pos => newBoard( pos._1 )( pos._2 ) = true )
        BoundedBoard( board = newBoard.map( _.toList ).toList )
    }
}
