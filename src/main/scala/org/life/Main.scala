package org.life

object Main {
    def main( args: Array[ String ] ) = {
        println( "Hello, world!" )
    }
}

object ConwaysGameOfLifeRunner extends App {
    def allPositions( height: Int, width: Int ): Positions = {
        for {x <- 0 until height; y <- 0 until width} yield (x, y)
    }.toSet
    def randomPositions( height: Int, width: Int, populationRatio: Double ): Positions =
        allPositions( height, width )
                .filter( e => Math.random < populationRatio )

    val (height, width) = (50, 80)
    val populationRatio = 0.3333
    val initialPositions: Positions =
        offsetBy( 10, 10 )( blinker )
//        randomPositions( height, width, populationRatio )

    val initBoard = MatrixBoardFactory.toroid( height, width )( initialPositions )
    // this type cannot be specified explicitly lest we require updating it each time we change from toroid to bounded
    val boardStream = Stream.iterate( initBoard )( _.nextBoard( ) )

    val (short, medium, long, epic) = (20, 150, 1125, 8437)
    val (numbing, slow, peppy, fast) = (500, 250, 125, 62)
    val (duration, delay) = (medium, fast)
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
            .foreach { case (t, index) =>
                println
                println( t )
                println( index )
                try {
                    Thread.sleep( delay )
                }
                catch {
                    case e: InterruptedException => println( "error" )
                }
            }
}
