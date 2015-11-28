package org

package object life {
    type Coordinate = (Int, Int)
    type Positions = Seq[ Coordinate ]

    // common shapes
    val square: Positions = List( (0, 0), (0, 1), (1, 0), (1, 1) )
    val blinker: Positions = List( (0, 0), (0, 1), (0, 2) )
    val glider: Positions = List( (0, 0), (0, 1), (0, 2), (1, 0), (2, 1) )

    def offsetBy( dx: Int, dy: Int )( positions: Positions ) = {
        positions map ( coord => (coord._1 + dx, coord._2 + dy) )
    }
}
