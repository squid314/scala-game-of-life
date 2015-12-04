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

    implicit class BoardStream[ B <: AbstractMatrixBackedBoard[ B ] ]( stream: Stream[ (B, Int) ] ) {
        def haltAfterSteadyState = {
            def noStateMatchesPrevious( items: Stream[ (B, Int) ] ): Boolean = {
                val (current, _) = items.last
                !( items.init exists { case (prev, _) => current == prev } )
            }
            // will catch boards which are steady state and boards which oscilate (blinkers) at period 2
            val maxFoundPeriod = 2
            // TODO rewrite this to not require the stream to have initial throwaway elements
            stream
                    .sliding( maxFoundPeriod + 1 )
                    .takeWhile( noStateMatchesPrevious )
                    .map( _.last ) // get back to the board we care about
        }
    }

    val (height, width) = (50, 80)
    val populationRatio = 0.3333
    val initialPositions: Positions =
        offsetBy( 10, 10 )( blinker )
//        randomPositions( height, width, populationRatio )

    val initBoard = MatrixBoardFactory.bounded( height, width )( initialPositions )
    // this type cannot be specified explicitly lest we require updating it each time we change from toroid to bounded
    val boardStream = Stream( (null, -2), (null, -1) ) ++
            Stream.iterate( initBoard )( _.nextBoard( ) ).zipWithIndex

    val (short, medium, long, epic) = (20, 150, 1125, 8437)
    val (numbing, slow, peppy, fast) = (500, 250, 125, 62)
    val (duration, delay) = (medium, fast)
    boardStream
            .haltAfterSteadyState
            .take( duration )
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
