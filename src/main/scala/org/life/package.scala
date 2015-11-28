package org

package object life {
    type Coordinate = (Int, Int)
    type Positions = Seq[ Coordinate ]

    def offsetBy( dx: Int, dy: Int )( positions: Positions ) = {
        positions map ( coord => (coord._1 + dx, coord._2 + dy) )
    }
}
