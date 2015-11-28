package org

package object life {
    type Coordinate = (Int, Int)
    type Positions = Set[ Coordinate ]

    // common shapes
    val square: Positions = Set( (0, 0), (0, 1), (1, 0), (1, 1) )
    val blinker: Positions = Set( (0, 0), (0, 1), (0, 2) )
    val glider: Positions = Set( (0, 0), (0, 1), (0, 2), (1, 0), (2, 1) )

    def offsetBy( dx: Int, dy: Int )( positions: Positions ) = {
        positions map ( coord => (coord._1 + dx, coord._2 + dy) )
    }

    /**
      * Takes a collection of positions and flips them horzontally between 0 and the max(y) in the sequence. Note that
      * flipping twice is not guaranteed to return the pattern to its original state due to the calculation of max(y)
      * each time.
      * @param positions the positions to flip
      * @return the flipped positions
      */
    def flipHorizontally( positions: Positions ) = {
        val maxY: Option[Int] = positions map { case (x, y) => y } reduceOption Math.max
        positions map { case (x, y) => (x, maxY.get - y) }
    }

    /**
      * Takes a collection of positions and flips them vertically between 0 and the max(x) in the sequence. Note that
      * flipping twice is not guaranteed to return the pattern to its original state due to the calculation of max(x)
      * each time.
      * @param positions the positions to flip
      * @return the flipped positions
      */
    def flipVertically( positions: Positions ) = ???

    /**
      * Rotates positions clockwise in 90 degree increments `count` times. This is only applied for counts of 0, 1, 2,
      * and 3. The rotation is applied with respect to `(0, 0)` such that all positions stay within the
      * positive-positive quadrant.
      * @param count number of 90 degree clockwise rotations
      * @param positions positions to rotate
      * @return the new positions
      */
    def rotate( count: Int )( positions: Positions ) = ???
}
