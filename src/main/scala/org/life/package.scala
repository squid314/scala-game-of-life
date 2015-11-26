package org

package object life {
    type Coordinate = (Int, Int)

    def offsetBy( dx: Int, dy: Int )( positions: Traversable[ Coordinate ] ) = {
        positions map ( coord => (coord._1 + dx, coord._2 + dy) )
    }
}
