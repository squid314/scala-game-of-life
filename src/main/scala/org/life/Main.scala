package org.life

object Main {
    def main( args: Array[ String ] ) = {
        println( "Hello, world!" )
    }
}

object ConwaysGameOfLifeRunner extends App {
    def randomPositions( height: Int, width: Int, populationRatio: Double ) = {
        ( 0 until height ).flatMap( i => ( 0 until width ).map( j => (i, j) ) )
                .collect { case coords: Coordinate if Math.random < populationRatio => coords }
    }

    def square = List( (0, 0), (0, 1), (1, 0), (1, 1) )
    def blinkerH = List( (0, 0), (0, 1), (0, 2) )
    def blinkerV = List( (0, 0), (1, 0), (2, 0) )

    val height = 50
    val width = 80
    val populationRatio = 0.3333
    val initialPositions: Seq[ Coordinate ] = randomPositions( height, width, populationRatio )

    val initBoard = MatrixBoardFactory.toroid( height, width )( initialPositions: _* )
    // this type cannot be specified explicitly lest we require updating it each time we change from toroid to bounded
    val boardStream = Stream.iterate( initBoard )( _.nextBoard( ) )

    val duration = 100000
    boardStream
            .sliding( 3 )
            .takeWhile( items => {
                val prevPrev = items.head
                val prev = items.tail.head
                val current = items.tail.tail.head
                // will catch boards which are steady state and boards which oscilate (blinkers)
                current != prev && current != prevPrev
            } )
            .take( duration )
            .map( _.tail.tail.head ) // get back to the board we care about (note that we lose printout of the initial board and the first generation; whatever)
            .zipWithIndex
            .foreach( { case (t, index) =>
                println
                println( t )
                println( index )
                try {
                    Thread.sleep( 100 )
                }
                catch {
                    case e: InterruptedException => println( "error" )
                }
            } )
}
