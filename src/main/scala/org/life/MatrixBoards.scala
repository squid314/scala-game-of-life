package org.life

import scala.collection.immutable.List

abstract case class AbstractMatrixBackedBoard[ B ]( board: List[ List[ Boolean ] ] ) {
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

    def nextBoard( ): B = {
        val nextBoard: List[ List[ Boolean ] ] = pairWithIndexes( board )
                .map( row => row
                        .map( { case (x, y, alive) => cellNextState( x, y, alive ) } )
                        .toList )
                .toList
        boardFor( nextBoard )
    }

    protected def boardFor( nextBoard: List[ List[ Boolean ] ] ): B

    def pairWithIndexes( board: List[ List[ Boolean ] ] ) = {
        board.indices.map( i => board( i ).indices.map( j => (i, j, board( i )( j )) ) )
    }

    override def toString = {
        board.map( row => row.map( cell => if ( cell ) "X " else ". " ).mkString ).mkString( "\n" )
    }
}

/** Represents a Game of Life board which is bounded by a destructive border. */
class BoundedBoard( board: List[ List[ Boolean ] ] ) extends AbstractMatrixBackedBoard[ BoundedBoard ]( board ) {
    override def neighbors( x0: Int, y0: Int ) = {
        ( x0 - 1 to x0 + 1 )
                .flatMap( x => ( y0 - 1 to y0 + 1 ).map( y => (x, y) ) )
                .filter { case (x, y) => board.isDefinedAt( x ) && board( x ).isDefinedAt( y ) }
                .filter { case (x, y) => x != x0 || y != y0 }
                .map { case (x, y) => board( x )( y ) }
                .toList
    }

    override protected def boardFor( nextBoard: List[ List[ Boolean ] ] ): BoundedBoard = new BoundedBoard( nextBoard )
}

/** Represent a Game of Life board where the edges are considered connected top-to-bottom and side-to-side. */
class ToroidalBoard( board: List[ List[ Boolean ] ] ) extends AbstractMatrixBackedBoard[ ToroidalBoard ]( board ) {
    override def neighbors( x0: Int, y0: Int ) = {
        ( x0 - 1 to x0 + 1 )
                .flatMap( x => ( y0 - 1 to y0 + 1 ).map( y => (x, y) ) )
                .filter { case (x, y) => x != x0 || y != y0 }
                .map { case (x, y) => fixCoords( x, y ) }
                .map { case (x, y) => board( x )( y ) }
                .toList
    }

    def fixCoords( x: Int, y: Int ): Coordinate = {
        // loop on x-axis
        if ( x < 0 ) fixCoords( x + board.length, y )
        else if ( x >= board.length ) fixCoords( x - board.length, y )
        // loop on y-axis
        else if ( y < 0 ) fixCoords( x, y + board( x ).length )
        else if ( y >= board( x ).length ) fixCoords( x, y - board( x ).length )
        // no change
        else (x, y)
    }

    override protected def boardFor( nextBoard: List[ List[ Boolean ] ] ): ToroidalBoard = new ToroidalBoard( nextBoard )
}

object MatrixBoardFactory {
    def bounded( height: Int, width: Int )( positions: Positions = Positions() ) = {
        val newBoard = Array.ofDim[ Boolean ]( height, width )
        positions foreach { case (x, y) => newBoard( x )( y ) = true }
        new BoundedBoard( board = newBoard.map( _.toList ).toList )
    }

    def toroid( height: Int, width: Int )( positions: Positions = Positions() ) = {
        val newBoard = Array.ofDim[ Boolean ]( height, width )
        positions foreach { case (x, y) => newBoard( x )( y ) = true }
        new ToroidalBoard( board = newBoard.map( _.toList ).toList )
    }
}
