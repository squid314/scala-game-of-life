package org.life

object Main {
    def main( args: Array[ String ] ) = {
        println( "Hello, world!" )
    }
}

object ConwaysGameOfLifeRunner extends App {
    val height = 50
    val width = 80
    val lifeChance = 0.3333
    val initialPositions = ( 0 until height ).flatMap( i => ( 0 until width ).map( j => (i, j) ) )
            .collect { case coords: (Int, Int) if Math.random( ) < lifeChance => coords }

    val initToroid = MatrixBoardFactory.toroid( height, width )( initialPositions: _* )
    val toroidStream: Stream[ ToroidalBoard ] =
        initToroid #:: toroidStream.map( _.nextBoard( ) )

    val duration = 100000
    toroidStream
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
            .foreach( pair => {
                val (t, index: Int) = pair
                println //
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
