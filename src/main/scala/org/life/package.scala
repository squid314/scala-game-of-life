package org

package object life {
    type Coordinate = (Int, Int)
    type Positions = Set[ Coordinate ]

    // common shapes
    val square: Positions = Set( (0, 0), (0, 1), (1, 0), (1, 1) )
    val blinker: Positions = Set( (0, 0), (0, 1), (0, 2) )
    val glider: Positions = Set( (0, 1), (1, 2), (2, 0), (2, 1), (2, 2) )
    val lwss: Positions = Set(
        /*           */ (0, 2), (0, 3),
        (1, 0), (1, 1), /*   */ (1, 3), (1, 4),
        (2, 0), (2, 1), (2, 2), (2, 3),
        /*   */ (3, 1), (3, 2) )
    //   0000000000 1111111111 2222222222 3333333333
    //   0123456789 0123456789 0123456789 0123456789
    // 0 .......... .......... ....O..... ..........
    // 1 .......... .......... ..O.O..... ..........
    // 2 .......... ..OO...... OO........ ....OO....
    // 3 .......... .O...O.... OO........ ....OO....
    // 4 OO........ O.....O... OO........ ..........
    // 5 OO........ O...O.OO.. ..O.O..... ..........
    // 6 .......... O.....O... ....O..... ..........
    // 7 .......... .O...O.... .......... ..........
    // 8 .......... ..OO...... .......... ..........
    /** shoots gliders at v=(1,1) */
    val gliderGun: Positions = Set(
        (0, 24),
        (1, 22), (1, 24),
        (2, 12), (2, 13), (2, 20), (2, 21), (2, 34), (2, 35),
        (3, 11), (3, 15), (3, 20), (3, 21), (3, 34), (3, 35),
        (4, 0), (4, 1), (4, 10), (4, 16), (4, 20), (4, 21),
        (5, 0), (5, 1), (5, 10), (5, 14), (5, 16), (5, 17), (5, 22), (5, 24),
        (6, 10), (6, 16), (6, 24),
        (7, 11), (7, 15),
        (8, 12), (8, 13) )
    //   0123
    // 0 OO..
    // 1 .O..
    // 2 .O.O
    // 3 ..OO
    /** kills gliders coming in at v=(-1,-1) */
    val gliderKiller: Positions = Set( (0, 0), (0, 1), (1, 1), (2, 1), (2, 3), (3, 2), (3, 3) )
    //   012345678901234
    // 0 .OO.....
    // 1 .OO.....
    // 2 ........
    // 3 .O......
    // 4 O.O.....
    // 5 O..O..OO
    // 6 ....O.OO
    val unix: Positions = offsetBy( 0, 1 )( square ) ++ offsetBy( 5, 6 )( square ) ++
            Set( (3, 1), (4, 0), (4, 2), (5, 0), (5, 3), (6, 4) )

    // other shapes
    // arrow pattern:
    // X .
    // . X
    // X .
    val arrow: Positions = Set( (0, 0), (1, 1), (2, 0) )
    // cross pattern:
    // . X .
    // X X X
    // . X .
    // . X .
    val cross: Positions = Set( (0, 1), (1, 0), (1, 1), (1, 2), (2, 1), (3, 1) )
    // bird pattern:
    // . . . . . X X
    // . . . . . X X
    // X X X X X X .
    // X X . . X . .
    // X . . . X . .
    // X X X X X . .
    // . . X . . . .
    val bird: Positions = Set(
        /*                                   */ (0, 5), (0, 6),
        /*                                   */ (1, 5), (1, 6),
        (2, 0), (2, 1), (2, 2), (2, 3), (2, 4), (2, 5),
        (3, 0), (3, 1), /*           */ (3, 4),
        (4, 0), /*                   */ (4, 4),
        (5, 0), (5, 1), (5, 2), (5, 3), (5, 4),
        /*           */ (6, 2) )

    def offsetBy( dx: Int, dy: Int )( positions: Positions ) = {
        positions map ( coord => (coord._1 + dx, coord._2 + dy) )
    }

    /**
      * Takes a collection of positions and flips them horizontally between 0 and the max(y) in the sequence. Note that
      * flipping twice is not guaranteed to return the pattern to its original state due to the calculation of max(y)
      * each time.
      * @param positions the positions to flip
      * @return the flipped positions
      */
    def flipHorizontally( positions: Positions ) = {
        maxs( positions ) match {
            case None => positions
            case Some( (_, maxy) ) =>
                positions map { case (x, y) => (x, maxy - y) }
        }
    }

    /**
      * Takes a collection of positions and flips them vertically between 0 and the max(x) in the sequence. Note that
      * flipping twice is not guaranteed to return the pattern to its original state due to the calculation of max(x)
      * each time.
      * @param positions the positions to flip
      * @return the flipped positions
      */
    def flipVertically( positions: Positions ) = {
        maxs( positions ) match {
            case None => positions
            case Some( (maxx, _) ) =>
                positions map { case (x, y) => (maxx - x, y) }
        }
    }

    def maxs( positions: Positions ) = {
        positions.reduceOption { ( maxs, next ) => {
            val (maxx, maxy) = maxs
            val (x, y) = next
            (Math.max( maxx, x ), Math.max( maxy, y ))
        }
        }
    }

    /**
      * Rotates positions clockwise in 90 degree increments `count` times. This is only applied for counts of 0, 1, 2,
      * and 3. The rotation is applied with respect to `(0, 0)` such that all positions stay within the
      * positive-positive quadrant.
      * @param count number of 90 degree clockwise rotations
      * @param positions positions to rotate
      * @return the new positions
      */
    def rotate( count: Int )( positions: Positions ): Positions = count match {
        case count: Int if count == 0 => positions
        case count: Int if count == 1 => {
            positions
        }
        case count: Int if count == 2 => positions
        case count: Int if count == 3 => positions
    }

//    def r2( count: Int ) = {
//        case count: Int if count == 0 => ( positions: Positions ) => positions
//    }
}
