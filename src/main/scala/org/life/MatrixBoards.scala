package org.life

import scala.collection.immutable.List

abstract case class AbstractMatrixBackedBoard[ B ]( board: List[ List[ Boolean ] ] ) {
    //    val board: List[ List[ Boolean ] ]
//
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
                        .map( cell => cellNextState( cell._1, cell._2, cell._3 ) )
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
    override def neighbors( i: Int, j: Int ) = {
        ( i - 1 to i + 1 ).filter( board.isDefinedAt )
                .flatMap( ii => ( j - 1 to j + 1 ).filter( board( ii ).isDefinedAt )
                        .map( jj => (ii, jj) ) )
                .filter( coords => coords._1 != i || coords._2 != j )
                .map( coords => board( coords._1 )( coords._2 ) )
                .toList
    }

    override protected def boardFor( nextBoard: List[ List[ Boolean ] ] ): BoundedBoard = new BoundedBoard( nextBoard )
}

/** Represent a Game of Life board where the edges are considered connected top-to-bottom and side-to-side. */
class ToroidalBoard( board: List[ List[ Boolean ] ] ) extends AbstractMatrixBackedBoard[ ToroidalBoard ]( board ) {
    override def neighbors( i: Int, j: Int ) = {
        ( i - 1 to i + 1 )
                .flatMap( ii => ( j - 1 to j + 1 ).map( jj => (ii, jj) ) )
                .filter( coords => coords._1 != i || coords._2 != j )
                .map( coords => fixCoords( coords._1, coords._2 ) )
                .map( coords => board( coords._1 )( coords._2 ) )
                .toList
    }

    def fixCoords( i: Int, j: Int ): (Int, Int) = {
        // loop on x-axis
        if ( i < 0 ) fixCoords( i + board.length, j )
        else if ( i >= board.length ) fixCoords( i - board.length, j )
        // loop on y-axis
        else if ( j < 0 ) fixCoords( i, j + board.length )
        else if ( j >= board.length ) fixCoords( i, j - board.length )
        // no change
        else (i, j)
    }

    override protected def boardFor( nextBoard: List[ List[ Boolean ] ] ): ToroidalBoard = new ToroidalBoard( nextBoard )
}

object MatrixBoardFactory {
    def bounded( height: Int, width: Int )( positions: (Int, Int)* ) = {
        val newBoard = Array.ofDim[ Boolean ]( height, width )
        positions foreach ( pos => newBoard( pos._1 )( pos._2 ) = true )
        new BoundedBoard( board = newBoard.map( _.toList ).toList )
    }

    def toroid( height: Int, width: Int )( positions: (Int, Int)* ) = {
        val newBoard = Array.ofDim[ Boolean ]( height, width )
        positions foreach ( pos => newBoard( pos._1 )( pos._2 ) = true )
        new ToroidalBoard( board = newBoard.map( _.toList ).toList )
    }
}
